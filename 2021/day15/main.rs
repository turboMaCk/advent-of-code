#![feature(map_first_last)]

use std::cmp::min;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::error;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

#[derive(Debug, Clone)]
struct Map(Vec<Vec<u32>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    value: u32,
    x: u32,
    y: u32,
}

struct PriorityQueue(BTreeMap<u32, Vec<Point>>);

impl PriorityQueue {
    fn new() -> Self {
        PriorityQueue(BTreeMap::new())
    }

    fn insert(&mut self, score: u32, point: Point) {
        match self.0.get_mut(&score) {
            None => {
                self.0.insert(score, vec![point]);
            }
            Some(vec) => vec.push(point),
        }
    }

    fn pop(&mut self) -> Option<(u32, Point)> {
        match self.0.pop_first() {
            None => None,
            Some((p, mut vec)) => {
                if vec.len() > 1 {
                    let val = vec.pop().unwrap();
                    self.0.insert(p, vec);
                    Some((p, val))
                } else {
                    let val = vec.pop().unwrap();
                    Some((p, val))
                }
            }
        }
    }
}

impl Map {
    fn new() -> Self {
        Map(Vec::new())
    }

    fn fill_row(&mut self, row: String) {
        let mut cells: Vec<u32> = Vec::new();

        for ch in row.chars() {
            cells.push(ch.to_string().parse().unwrap());
        }

        self.0.push(cells);
    }

    fn get_point(&self, x: usize, y: usize) -> Point {
        Point {
            x: x as u32,
            y: y as u32,
            value: self.0[y][x],
        }
    }

    fn get_surrounding(&self, x: u32, y: u32) -> Vec<Point> {
        let i = y as usize;
        let j = x as usize;

        let height = self.0.len();
        let width = self.0[0].len();
        let mut surrounding = Vec::new();

        // 4 corners
        if i == 0 && j == 0 {
            surrounding.push(self.get_point(1, 0));
            surrounding.push(self.get_point(0, 1));
        } else if i == 0 && j + 1 == width {
            surrounding.push(self.get_point(width - 2, 0));
            surrounding.push(self.get_point(width - 1, 1));
        } else if i + 1 == height && j + 1 == width {
            surrounding.push(self.get_point(width - 2, height - 1));
            surrounding.push(self.get_point(width - 1, height - 2));
        } else if i + 1 == height && j == 0 {
            surrounding.push(self.get_point(1, height - 1));
            surrounding.push(self.get_point(0, height - 2));

        // 4 edges
        } else if i == 0 {
            surrounding.push(self.get_point(j - 1, 0));
            surrounding.push(self.get_point(j + 1, 0));
            surrounding.push(self.get_point(j, 1));
        } else if j + 1 == width {
            surrounding.push(self.get_point(j, i - 1));
            surrounding.push(self.get_point(j, i + 1));
            surrounding.push(self.get_point(j - 1, i));
        } else if i + 1 == height {
            surrounding.push(self.get_point(j - 1, i));
            surrounding.push(self.get_point(j + 1, i));
            surrounding.push(self.get_point(j, i - 1));
        } else if j == 0 {
            surrounding.push(self.get_point(j, i - 1));
            surrounding.push(self.get_point(j, i + 1));
            surrounding.push(self.get_point(j + 1, i));

        // rest
        } else {
            surrounding.push(self.get_point(j, i - 1));
            surrounding.push(self.get_point(j, i + 1));
            surrounding.push(self.get_point(j - 1, i));
            surrounding.push(self.get_point(j + 1, i));
        }

        surrounding
    }

    fn get_end(&self) -> Point {
        self.get_point(self.0[0].len() - 1, self.0.len() - 1)
    }

    fn get_start(&self) -> Point {
        self.get_point(0, 0)
    }

    fn part1(&self) -> u32 {
        let start = self.get_start();
        let end = self.get_end();

        let mut point_scores = HashMap::new();
        point_scores.insert(start, 0);

        let mut queue = PriorityQueue::new();
        queue.insert(0, start);

        let mut res = Vec::new();
        loop {
            match queue.pop() {
                None => {
                    break;
                }
                Some((score, point)) => {
                    for p in self.get_surrounding(point.x, point.y) {
                        if p == end {
                            res.push(score + end.value);
                            break;
                        }

                        match point_scores.get_mut(&p) {
                            None => {
                                point_scores.insert(p, score + end.value);
                            }
                            Some(prev_score) => {
                                // avoid cycles!
                                if *prev_score <= (score + end.value) {
                                    continue;
                                }

                                *prev_score = score + end.value;
                            }
                        }

                        queue.insert(score + p.value, p);
                    }
                }
            }
        }

        let mut min_v = u32::MAX;

        for val in res.iter() {
            min_v = min(min_v, *val);
        }

        min_v
    }

    fn print(&self) -> String {
        let height = self.0.len();
        let width = self.0[0].len();

        let mut string = "".to_string();
        for y in 0..height {
            for x in 0..width {
                string.push_str(&self.0[y][x].to_string());
            }

            string.push('\n')
        }

        string
    }

    fn expand(&mut self) {
        let original = self.0.clone();

        // expand down
        for step in 1..5 {
            for vec in original.iter() {
                self.0
                    .push(vec.iter().map(|v| if *v + step > 9 { *v + step - 9 } else { *v + step }).collect())
            }
        }

        // expand right
        let width = self.0[0].len();
        let height = self.0.len();
        for step in 1..5 {
            for j in 0..height {
                let vec = &mut self.0[j];
                for i in 0..width {
                    let mut val = step + vec[i];
                    if val > 9 {
                        val -= 9;
                    }

                    vec.push(val)
                }
            }
        }
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = File::open("day15/input.txt").unwrap();
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut map = Map::new();
    for res in lines {
        let line = res?;
        map.fill_row(line);
    }

    println!("part1: {}", map.part1());
    map.expand();
    println!("part2: {}", map.part1());

    Ok(())
}
