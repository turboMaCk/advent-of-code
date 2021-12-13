use std::cmp::max;
use std::collections::HashSet;
use std::error;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
struct Point {
    x: i32,
    y: i32,
}

impl From<String> for Point {
    fn from(s: String) -> Point {
        let mut vals = s.split(",");
        let x = vals.next().unwrap().parse().unwrap();
        let y = vals.next().unwrap().parse().unwrap();
        Point { x, y }
    }
}

#[derive(Debug, Copy, Clone)]
enum Fold {
    FoldX(i32),
    FoldY(i32),
}

impl From<String> for Fold {
    fn from(s: String) -> Fold {
        use Fold::*;
        let str = s.replace("fold along ", "");
        let mut info = str.split("=");

        match info.next().unwrap() {
            "x" => {
                let val = info.next().unwrap().parse().unwrap();
                FoldY(val)
            }
            _ => {
                let val = info.next().unwrap().parse().unwrap();
                FoldX(val)
            }
        }
    }
}

#[derive(Debug)]
struct Paper {
    points: HashSet<Point>,
    width: i32,
    height: i32,
}

impl Paper {
    fn new() -> Self {
        Paper {
            points: HashSet::new(),
            width: 0,
            height: 0,
        }
    }

    fn fill_line(&mut self, str: String) {
        let point = Point::from(str);
        self.insert(point);
    }

    fn insert(&mut self, p: Point) {
        self.width = max(p.x + 1, self.width);
        self.height = max(p.y + 1, self.height);
        self.points.insert(p);
    }

    fn alter(&mut self, fold: Fold) {
        use Fold::*;

        match fold {
            FoldX(y) => {
                self.height = y;

                for point in self.points.clone() {
                    if point.y > y {
                        self.points.remove(&point);
                        self.points.insert(Point {
                            x: point.x,
                            y: (-1 * (point.y - y)) + y,
                        });
                    }
                }
            }
            FoldY(x) => {
                self.width = x;

                for point in self.points.clone() {
                    if point.x > x {
                        self.points.remove(&point);
                        self.points.insert(Point {
                            x: (-1 * (point.x - x)) + x,
                            y: point.y,
                        });
                    }
                }
            }
        }
    }

    fn print(&self) -> String {
        let mut string = "".to_string();

        for y in 0..self.height {
            for x in 0..self.width {
                if self.points.contains(&Point { x, y }) {
                    string.push('#');
                } else {
                    string.push('.');
                }
            }

            string.push('\n')
        }

        string
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = File::open("day13/input.txt").unwrap();
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut paper = Paper::new();
    let mut points = true;

    let mut instructions: Vec<Fold> = Vec::new();
    for res in lines {
        let line = res?;

        if line == "".to_string() {
            points = false;
            continue;
        }

        if points {
            paper.fill_line(line);
        } else {
            instructions.push(Fold::from(line));
        }
    }

    paper.alter(instructions[0]);
    // paper.alter(instructions[1]);
    println!("{}", paper.print());
    println!("part1 {}", paper.points.len());

    Ok(())
}
