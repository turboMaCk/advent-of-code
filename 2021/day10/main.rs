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

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = File::open("day10/input.txt")?;
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut error_score = 0;
    for (_, res) in lines.enumerate() {
        let line = res?;
        println!("{:?}", line);

        let mut stack: Vec<char> = Vec::new();

        for ch in line.chars() {
            match ch {
                '(' | '[' | '{' | '<' => stack.push(ch),
                _ => match stack.pop() {
                    None => {
                        println!("Error: trailing {}", ch);
                        break;
                    }
                    Some(val) => match (val, ch) {
                        ('(', ')') => {}
                        ('[', ']') => {}
                        ('{', '}') => {}
                        ('<', '>') => {}
                        (a, b) => {
                            println!("Error: got {} for {}", b, a);
                            error_score += score(b);
                            break;
                        }
                    },
                },
            }
        }
    }

    println!("part1: {}", error_score);

    Ok(())
}
