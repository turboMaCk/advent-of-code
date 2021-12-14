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

fn part2(list: &LinkedList<char>, map: &HashMap<(char, char), char>) -> u64 {
    // compress information to just counts of pairs
    let mut pair_counts: HashMap<(char, char), u64> = HashMap::new();

    // initialize pair_counts (with correct starting values);
    let mut cur = list.cursor_front();
    loop {
        let current = *cur.current().unwrap();
        match cur.peek_next() {
            Some(next) => {
                match pair_counts.get_mut(&(current, *next)) {
                    None => {
                        pair_counts.insert((current, *next), 1);
                    }
                    Some(count) => {
                        *count += 1;
                    }
                }
            },
            None => break,
        }

        cur.move_next();
    }

    // progress 40 times

    // part1 did first 10 steps
    for _ in 0..30 {
        let mut next_gen: HashMap<(char, char), u64> = HashMap::new();

        for ((chr1, chr2), count) in pair_counts.iter() {
          match map.get(&(*chr1, *chr2)) {
              None => {
                  match next_gen.get_mut(&(*chr1, *chr2)) {
                      None => {
                          next_gen.insert((*chr1, *chr2), *count);
                      },
                      Some(cnt) => {
                          *cnt += count;
                      }
                  }
              }
              Some(new) => {
                  match next_gen.get_mut(&(*chr1, *new)) {
                      None => {
                          next_gen.insert((*chr1, *new), *count);
                      },
                      Some(cnt) => {
                          *cnt += count;
                      }
                  }

                  match next_gen.get_mut(&(*new, *chr2)) {
                      None => {
                          next_gen.insert((*new, *chr2), *count);
                      },
                      Some(cnt) => {
                          *cnt += count;
                      }
                  }
              }
          }
        }

        pair_counts = next_gen;
    }

    // calculate counts
    let mut counts: HashMap<char,u64> = HashMap::new();

    // every character is counted twice (member of 2 pairs!)
    for ((chr1, chr2), count) in pair_counts.iter() {
        match counts.get_mut(chr1) {
            None => {
                counts.insert(*chr1, *count / 2);
            },
            Some(cnt) => {
                *cnt += count / 2;
            }
        }
        match counts.get_mut(chr2) {
            None => {
                counts.insert(*chr2, *count / 2);
            },
            Some(cnt) => {
                *cnt += count / 2;
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
    let file = File::open("day14/input.txt").unwrap();
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
    println!("part2: {}", part2(&polymer, &map));

    Ok(())
}
