use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

fn calc_increases1(values: &Vec<u32>) -> u32 {
    let mut prev = values[0];
    let mut count: u32 = 0;

    for val in values {
        if prev < *val {
            count += 1;
        }
        prev = *val;
    }
    count
}

fn sum(v1: u32, v2: u32, v3: u32) -> u32 {
    v1 + v2 + v3
}

fn calc_increases2(values: &Vec<u32>) -> u32 {
    let mut p1 = values[0];
    let mut p2 = values[1];
    let mut p3 = values[2];
    let mut count: u32 = 0;
    let mut index = -1;

    for val in values {
        index += 1;
        if index < 3 {
            continue;
        }
        if sum(p1, p2, p3) < sum(p2, p3, *val) {
            count += 1;
        }

        p1 = p2;
        p2 = p3;
        p3 = *val;
    }
    count
}

fn parse_u32<T: Debug>(res: Result<String, T>) -> u32 {
    res.unwrap().parse().unwrap()
}

fn main() {
    let file = File::open("day01/input.txt").unwrap();
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut vals: Vec<u32> = Vec::new();

    for (_index, line) in lines.enumerate() {
        vals.push(parse_u32(line));
    }

    println!("{}", calc_increases1(&vals));
    println!("{}", calc_increases2(&vals));
}
