use std::error;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

fn score(illegal: char) -> u32 {
    match illegal {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => 0,
    }
}

fn score2(legal: char) -> u64 {
    match legal {
        '(' => 1,
        '[' => 2,
        '{' => 3,
        '<' => 4,
        _ => 0,
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = File::open("day10/input.txt")?;
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    // part 1
    let mut error_score = 0;

    //part 2
    let mut scores: Vec<u64> = Vec::new();

    'l: for (_, res) in lines.enumerate() {
        let line = res?;

        let mut stack: Vec<char> = Vec::new();

        for ch in line.chars() {
            match ch {
                '(' | '[' | '{' | '<' => stack.push(ch),
                _ => match stack.pop() {
                    None => {
                        // println!("Error: trailing {}", ch);
                        continue 'l;
                    }
                    Some(val) => match (val, ch) {
                        ('(', ')') => {}
                        ('[', ']') => {}
                        ('{', '}') => {}
                        ('<', '>') => {}
                        (a, b) => {
                            // println!("Error: got {} for {}", b, a);
                            error_score += score(b);
                            continue 'l;
                        }
                    },
                },
            }
        }

        // part 2 scores
        stack.reverse();
        let mut completion_score = 0;
        for open in stack.iter() {
            completion_score *= 5;
            completion_score += score2(*open);
        }
        scores.push(completion_score);
    }

    println!("part1: {}", error_score);
    scores.sort();
    println!("part2: {:?}", scores[(scores.len() / 2)]);

    Ok(())
}
