use std::fs::File;
use std::io::{BufRead, BufReader, Lines};
use std::error;

const DIGITS: usize = 9;
struct Counts([usize;DIGITS]);

impl Counts {
    fn new() -> Counts {
        Counts([0;DIGITS])
    }

    fn from_seg_count(str: &str) -> Option<usize> {
        match str.len() {
            2 => Some(1),
            4 => Some(4),
            3 => Some(7),
            7 => Some(8),
            _ => None,
        }
    }

    fn add(&mut self, digit: usize) {
        self.0[digit] += 1;
    }

    fn in_string(&mut self, str: &String) {
        str.split(" ").for_each(|digit_str| {
            match Counts::from_seg_count(digit_str) {
                Some(digit) => self.add(digit),
                None => {}
            }
        });
    }

    fn sum(&self) -> usize {
        self.0.iter().sum()
    }
}

fn drop_till(point: char, str: &String) -> String {
    let mut out = String::new();
    let mut adding = false;

    str.chars().for_each(|ch| {
        if adding {
            out.push(ch);
        }
        if ch == point {
            adding = true;
        }
    });

    out
}


fn main() -> Result<(), Box<dyn error::Error>> {
    let file = File::open("day08/input.txt")?;
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut counts = Counts::new();
    for (_, line) in lines.enumerate() {
        let output : String = drop_till('|', &line?).trim().to_string();
        counts.in_string(&output);
    }

    println!("{}", counts.sum());

    Ok(())
}
