use std::error;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

const DIGITS: usize = 9;
struct Counts([usize; DIGITS]);

impl Counts {
    fn new() -> Counts {
        Counts([0; DIGITS])
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

    fn in_string(&mut self, strs: &Vec<String>) {
        strs.iter().for_each(|digit_str| match Counts::from_seg_count(digit_str) {
            Some(digit) => self.add(digit),
            None => {}
        });
    }

    fn sum(&self) -> usize {
        self.0.iter().sum()
    }
}

/**

  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

*/

#[derive(Debug)]
struct CouldBe {
    a: Vec<char>,
    b: Vec<char>,
    c: Vec<char>,
    d: Vec<char>,
    e: Vec<char>,
    f: Vec<char>,
    g: Vec<char>,
}

impl CouldBe {
    fn new() -> Self {
        CouldBe {
            a: Vec::new(),
            b: Vec::new(),
            c: Vec::new(),
            d: Vec::new(),
            e: Vec::new(),
            f: Vec::new(),
            g: Vec::new(),
        }
    }

    fn any_contains(&self, ch: &char) -> bool {
        self.a.contains(ch)
            || self.b.contains(ch)
            || self.c.contains(ch)
            || self.d.contains(ch)
            || self.e.contains(ch)
            || self.f.contains(ch)
            || self.g.contains(ch)
    }

    fn decode_digit(&self, str: &String) -> u32 {
        // TODO this could use guards but....
        match str.len() {
            2 => 1,
            4 => 4,
            3 => 7,
            7 => 8,
            // 2,3,5
            5 => {
                if str.contains(self.e[0]) {
                    return 2;
                } else if str.contains(self.c[0]) {
                    return 3;
                } else {
                    return 5;
                }
            }
            // 0,6,9
            6 => {
                if !str.contains(self.d[0]) {
                    return 0;
                } else if str.contains(self.e[0]) {
                    return 6;
                } else {
                    return 9;
                }
            }
            _ => 0,
        }
    }
}

fn detect_digits(chunks: Vec<String>) -> CouldBe {
    let mut known = CouldBe::new();

    // 1 4 7 8 are know from length -> lets start with that

    // find digit 1
    {
        let digits = chunks.iter().find(|c| c.len() == 2).unwrap();
        known.c = digits.chars().collect();
        known.f = known.c.clone();
    }

    // find digit 4
    {
        let digits = chunks.iter().find(|c| c.len() == 4).unwrap();
        known.b = digits.chars().filter(|d| !known.c.contains(d)).collect();
        known.d = known.b.clone();
    }

    // find digit 7
    {
        let digits = chunks.iter().find(|c| c.len() == 3).unwrap();
        // FOUND A!!!
        known.a = digits.chars().filter(|d| !known.c.contains(d)).collect();
    }

    // digit 8 is not helpful since it contains everything so we don't bother...

    // 2, 3 and 5 are 5 five segments
    {
        let strings: Vec<&String> = chunks.iter().filter(|c| c.len() == 5).collect();
        // FOUND D!!!
        for posibility in &known.d {
            if strings
                .iter()
                .all(|s| s.chars().collect::<Vec<char>>().contains(&posibility))
            {
                known.d = vec![*posibility];
                break;
            }
        }

        // FOUND B!!!
        for posibility in &known.b {
            if known.d[0] != *posibility {
                known.b = vec![*posibility];
                break;
            }
        }

        // FOUND G!!!
        for ch in strings[0].chars() {
            if known.a[0] == ch || known.d[0] == ch {
                continue;
            }
            if strings
                .iter()
                .all(|str| str.chars().collect::<Vec<char>>().contains(&ch))
            {
                known.g = vec![ch];
            }
        }

        // FOUND E !!! (the only char we just got that is not already placed anywhere)
        for str in strings.iter() {
            for ch in str.chars() {
                if known.any_contains(&ch) {
                    continue;
                } else {
                    known.e = vec![ch];
                    break;
                }
            }
        }
    }

    // 0, 6 and 9 are 6 segments
    {
        let strings: Vec<&String> = chunks.iter().filter(|c| c.len() == 6).collect();
        // both 6 and 9 contain f but 6 doens't contain c so we use that to resolve last two
        for posibility in known.c.clone() {
            if strings
                .iter()
                .all(|s| s.chars().collect::<Vec<char>>().contains(&posibility))
            {
                known.f = vec![posibility];
            } else {
                known.c = vec![posibility];
            }
        }
    }

    known
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = File::open("day08/input.txt")?;
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut counts = Counts::new();
    let mut part2 = 0;
    for (_, line_) in lines.enumerate() {
        let line = line_?;

        let parts: Vec<&str> = line.split(" | ").collect();
        let inputs: Vec<String> = parts[0].split(" ").map(|str| str.to_string()).collect();
        let outputs: Vec<String> = parts[1].split(" ").map(|str| str.to_string()).collect();

        counts.in_string(&outputs);

        let decoder = detect_digits(inputs);
        let mut out_val = 0;
        for str in outputs.iter() {
            out_val = out_val * 10 + decoder.decode_digit(str);
        }

        part2 += out_val;
    }

    println!("part1: {}", counts.sum());
    println!("part2: {}", part2);

    Ok(())
}
