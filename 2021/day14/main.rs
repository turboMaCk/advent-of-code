#![feature(linked_list_cursors)]

use std::cmp::{max, min};
use std::collections::{HashMap, LinkedList};
use std::error;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

fn step(list: &mut LinkedList<char>, map: &HashMap<(char, char), char>) {
    let mut cur = list.cursor_front_mut();

    loop {
        let current = *cur.current().unwrap();
        match cur.peek_next() {
            Some(next) => {
                match map.get(&(current, *next)) {
                    None => {}
                    Some(new) => {
                        cur.insert_after(*new);
                        cur.move_next();
                    }
                }
            },
            None => break,
        }

        cur.move_next();
    }
}

fn to_int(list: &LinkedList<char>) -> u64 {
    let mut counts: HashMap<char,u64> = HashMap::new();

    for chr in list.iter() {
        match counts.get_mut(chr) {
            None => {
                counts.insert(*chr, 1);
            },
            Some(count) => {
                *count += 1;
            }
        }
    }

    let mut max_val = 0;
    let mut min_val = u64::MAX;

    for (_, count) in counts.iter() {
        max_val = max(max_val, *count);
        min_val = min(min_val, *count);
    }

    max_val - min_val
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = File::open("day14/test.txt").unwrap();
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut polymer: LinkedList<char> = LinkedList::new();
    let mut map: HashMap<(char, char), char> = HashMap::new();
    for (i, res) in lines.enumerate() {
        let line = res?;

        // read polymer
        if i == 0 {
            for chr in line.chars() {
                polymer.push_back(chr);
            }
        } else if i > 1 {
            let mut splited = line.split(" -> ");
            let str = splited.next().unwrap();
            let value = splited.next().unwrap();

            map.insert(
                (str.chars().nth(0).unwrap(), str.chars().nth(1).unwrap()),
                value.chars().nth(0).unwrap(),
            );
        }
    }

    for _ in 0..10 {
        step(&mut polymer, &map);
    }

    println!("part1: {}", to_int(&polymer));

    // for _ in 0..20 {
    //     step(&mut polymer, &map);
    // }

    // println!("part2: {}", polymer.len());

    Ok(())
}
