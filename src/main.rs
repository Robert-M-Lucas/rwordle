use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;
use std::time::{Duration, Instant};
use bitvec::macros::internal::funty::Fundamental;
use bitvec::vec::BitVec;
use derive_new::new;
use hhmmss::Hhmmss;
use rayon::prelude::*;

const ASCII_ALPH_OFFSET: u8 = 97;

#[derive(Clone, Eq, PartialEq)]
struct Word<const N: usize> {
    characters: [u32; N],
    all_chars: u32
}

impl<const N: usize> Word<N> {
    fn new(word: &str) -> Word<N> {
        if !word.is_ascii() {
            panic!("Word is not ASCII");
        }
        if word.len() != N {
            panic!("Incorrect word length")
        }
        let chars = core::array::from_fn(
            |i| {
                if word.as_bytes()[i] < ASCII_ALPH_OFFSET || word.as_bytes()[i] > ASCII_ALPH_OFFSET + 26 {
                    panic!("Invalid character - {}", word.as_bytes()[i]);
                }
                1 << (word.as_bytes()[i] - ASCII_ALPH_OFFSET)
            });
        let all_chars = chars.iter().fold(0, |p, x| p | *x);
        Word {
            characters: chars,
            all_chars
        }
    }

    fn chars(&self) -> &[u32; N] {
        &self.characters
    }

    fn all_chars(&self) -> u32 {
        self.all_chars
    }
}

impl<const N: usize> Display for Word<N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = self.characters.iter().map(|c| {
            let mut x = *c;
            if x == 0 { panic!() }
            let mut cu = ASCII_ALPH_OFFSET;
            while x != 1 {
                x = x >> 1;
                cu += 1;
            }
            (cu as u8).as_char().unwrap()
        }).collect::<String>();

        write!(f, "{}", string)
    }
}

const ALL_ALLOWED: u32 = 0b00000011111111111111111111111111;

#[derive(Copy, Clone)]
struct WordMask<const N: usize> {
    char_mask: [u32; N],
    positive_mask: u32,
    negative_mask: u32
}

impl<const N: usize> WordMask<N> {
    fn new() -> WordMask<N> {
        WordMask {
            char_mask: [ALL_ALLOWED; N],
            positive_mask: 0,
            negative_mask: 0
        }
    }
    
    fn new_from_guess(guess: &Word<N>, actual: &Word<N>) -> WordMask<N> {
        let mut new = WordMask::new();
        new.apply_guess(guess, actual, false);
        new
    }

    fn apply_guess(&mut self, guess: &Word<N>, actual: &Word<N>, verbose: bool) {
        if verbose {
            println!("Applying guess: {}, actual: {}, to mask \n {:?}", guess, actual, self);
        }
        let guess_chars = guess.chars();
        let actual_chars = actual.chars();
        for i in 0..N {
            if guess_chars[i] == actual_chars[i] {
                self.char_mask[i] = actual_chars[i];
                self.positive_mask |= actual_chars[i];
                if verbose {
                    println!("Char aligned");
                }
            }
            else if actual_chars.contains(&guess_chars[i]) {
                self.char_mask[i] = self.char_mask[i] & (! guess_chars[i]);
                self.positive_mask |= guess_chars[i];
                if verbose {
                    println!("Char found");
                }
            }
            else {
                self.negative_mask |= guess_chars[i];
                if verbose {
                    println!("Char not found - {:#b}", self.negative_mask);
                }
            }
        }
    }
    
    fn apply_input(&mut self, input: &str) -> bool {
        todo!();

        let mut char_mask = self.char_mask.clone();
        let mut all_char_mask = self.positive_mask;
        
        if !input.is_ascii() {
            println!("Input is not ASCII");
            return false;
        }
        
        let mut skip_next = false;

        let space = " ";
        let mut i_adjust = 0;
        for ((i, c), nc) in input.chars().enumerate().zip(input.chars().skip(1).chain(space.chars())) {
            if skip_next {
                skip_next = false;
                i_adjust += 1;
                continue;
            }
            
            let c = c as u8;
            if c < ASCII_ALPH_OFFSET {
                println!("Invalid character: {c}");
                return false;
            }
            let c = c - ASCII_ALPH_OFFSET;
            if c > 25 {
                println!("Invalid character: {}", c + ASCII_ALPH_OFFSET);
            }
            let c = c as u32;

            if nc == '!' {
                skip_next = true;
                char_mask[i - i_adjust] = 1 << c;
            }
            else if nc == '?' {
                skip_next = true;
                char_mask[i - i_adjust] = char_mask[i - i_adjust] & (! (1 << c));
            }
            else {
                all_char_mask &= ! (1 << c);
            }
        }

        self.char_mask = char_mask;
        self.positive_mask = all_char_mask;

        true
    }

    fn filter_word(&self, word: &Word<N>) -> bool {
        if (word.all_chars() & self.positive_mask) != self.positive_mask {
            return false
        }
        if (word.all_chars() & self.negative_mask) != 0 {
            return false
        }

        let word_chars = word.chars();

        for i in 0..N {
            if self.char_mask[i] & word_chars[i] == 0 {
                return false;
            }
        }

        true
    }
}

impl<const N: usize> Debug for WordMask<N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut sb = "Chars: ".to_string();
        for c in &self.char_mask {
            for i in 0..26 {
                if (*c >> i) & 1 != 0 {
                    sb.push(char::from(ASCII_ALPH_OFFSET + i as u8));
                    sb.push(',');
                }
            }
            sb.push_str(" | ");
        }

        sb.push_str("\nPositive: ");
        for i in 0..26 {
            if (self.positive_mask >> i) & 1 != 0 {
                sb.push(char::from(ASCII_ALPH_OFFSET + i as u8));
                sb.push(',');
            }
        }

        sb.push_str("\nNegative: ");
        for i in 0..26 {
            if (self.negative_mask >> i) & 1 != 0 {
                sb.push(char::from(ASCII_ALPH_OFFSET + i as u8));
                sb.push(',');
            }
        }

        write!(f, "{}", sb)
    }
}

fn get_words<const N: usize, P: AsRef<Path>>(path: P) -> Vec<Word<N>> {
    let mut words = String::new();
    File::open(path).unwrap().read_to_string(&mut words).unwrap();
    words.split_whitespace().map(|w| Word::new(w)).collect()
}


fn main() {
    const WORD_LENGTH: usize = 5;
    let original_valid_answers = get_words::<WORD_LENGTH, _>("valid_answers.txt");
    let valid_guesses = get_words::<WORD_LENGTH, _>("valid_guesses.txt");

    let start = Instant::now();
    let mut total_turns = 0;

    for (i, actual_answer) in original_valid_answers.iter().enumerate() {
        let mut valid_answers = original_valid_answers.clone();
        let mut current_mask = WordMask::new();

        let mut turns = 0;

        // println!("Actual: {}", actual_answer);

        loop {
            turns += 1;

            let mut best_total_remaining = AtomicUsize::new(usize::MAX);
            let mut best_guess = Mutex::new(valid_answers[0].clone());
            let mut second_best_guess = Mutex::new(valid_answers[0].clone());
            // let mut progress = AtomicUsize::new(0);
            // let start = Instant::now();
            // let mut last = Instant::now();

            // println!("Working...");

            valid_guesses.par_iter().for_each(|guess| {
                let mut total_remaining = 0;

                for answer in &valid_answers {
                    let mut filter = current_mask.clone();
                    filter.apply_guess(guess, answer, false);
                    total_remaining += valid_answers.iter().filter(|w| filter.filter_word(w)).count();
                }

                if total_remaining == 0 {
                    return;
                }

                if total_remaining < best_total_remaining.load(Ordering::Acquire) {
                    let mut best = best_guess.lock().unwrap();
                    *second_best_guess.lock().unwrap() = best.clone();
                    *best = guess.clone();
                    best_total_remaining.store(total_remaining, Ordering::Release);
                }

                // progress += 1;
            });
            //
            // for guess in &valid_guesses {
            //     // if Instant::now() - last > Duration::from_secs(5) {
            //     //     println!(
            //     //         "Progress: {}/{} | Elapsed: {} | ETA: {} [{:.2}/s]",
            //     //         progress,
            //     //         valid_guesses.len(),
            //     //         (Instant::now() - start).hhmmss(),
            //     //         (((Instant::now() - start) / progress as u32) * (valid_guesses.len() - progress) as u32).hhmmss(),
            //     //         (Instant::now() - start).as_secs_f32() / progress as f32
            //     //     );
            //     //     last = Instant::now();
            //     // }
            //
            //
            // }

            let best_guess = best_guess.into_inner().unwrap();
            // println!("Best word: {} | Average: {:.2} | Second best: {}", &best_guess, best_total_remaining.into_inner() as f32 / valid_answers.len() as f32, second_best_guess.into_inner().unwrap());

            // println!("Remaining words: {}", valid_answers.len());
            // for (i, word) in valid_answers.iter().enumerate() {
            //     if i == 10 { break };
            //     println!("{}", word);
            // }
            // if valid_answers.len() > 10 {
            //     println!("...");
            // }

            if &best_guess == actual_answer {
                break;
            }
            current_mask.apply_guess(&best_guess, &actual_answer, false);

            // loop {
            //     print!("> ");
            //     std::io::stdout().flush().unwrap();
            //     let mut input = String::new();
            //     std::io::stdin().read_line(&mut input).unwrap();
            //
            //     if current_mask.apply_input(input.trim()) {
            //         break;
            //     }
            // }

            // println!("New mask: {:?}", current_mask);

            let mut removed =  0;
            for i in 0..valid_answers.len() {
                if !current_mask.filter_word(&valid_answers[i - removed]) {
                    valid_answers.remove(i - removed);
                    removed += 1;
                }
            }

            if valid_answers.len() == 1 {
                if &valid_answers[0] != actual_answer {
                    panic!("Final option: {} does not match actual {}", &valid_answers[0], actual_answer);
                }
                else {
                    turns += 1;
                    break;
                }
            }
        }

        total_turns += turns;

        println!("Current average: {:.2} | Elapsed: {} | ETA: {}", total_turns as f32 / (i + 1) as f32, (Instant::now() - start).hhmmss(), (((Instant::now() - start) / (i + 1) as u32) * ((original_valid_answers.len() - (i + 1)) as u32)).hhmmss())
    }

    println!("Average turns: {:.3}", total_turns as f32 / original_valid_answers.len() as f32);
}
