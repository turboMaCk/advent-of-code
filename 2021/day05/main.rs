use std::cmp::{max, min};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

// Like vec2
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Point {
    x: i32,
    y: i32,
}

impl From<String> for Point {
    fn from(str: String) -> Self {
        let vec: Vec<i32> = str.split(",").filter_map(|s| s.parse().ok()).collect();

        Point {
            x: vec[0],
            y: vec[1],
        }
    }
}

#[derive(Debug)]
struct Plane {
    map: HashMap<Point, i32>,
}

impl Plane {
    fn new() -> Self {
        Plane {
            map: HashMap::new(),
        }
    }

    fn add_point(&mut self, p: Point) {
        match self.map.get(&p) {
            None => self.map.insert(p, 1),
            Some(c) => self.map.insert(p, c + 1),
        };
    }

    fn add_line(&mut self, (p1, p2): (Point, Point)) {
        let (start_x, end_x) = (min(p1.x, p2.x), max(p1.x, p2.x));
        let (start_y, end_y) = (min(p1.y, p2.y), max(p1.y, p2.y));
        let len = max(end_x - start_x, end_y - start_y);
        let change_x = (p2.x - p1.x)/len;
        let change_y = (p2.y - p1.y)/len;

        for i in 0..=len {
            self.add_point(Point{
                x: p1.x + change_x*i,
                y: p1.y + change_y*i,
            })
        }
    }

    fn count_at_least(&self, c: i32) -> usize {
        let mut total: usize = 0;
        for (_, v) in self.map.iter() {
            if *v >= c {
                total += 1;
            }
        }
        total
    }

    fn draw(&self, size: i32) -> String {
        let mut str: String = "".to_string();

        for y in 0..size {
            for x in 0..size {
                match self.map.get(&Point { x, y }) {
                    None => str.push('.'),
                    Some(c) => str.push_str(&c.to_string()),
                }
                str.push(' ');
            }
            str.push('\n');
        }

        str
    }
}

fn main() {
    let file = File::open("day05/input.txt").unwrap();
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut plane1: Plane = Plane::new();
    let mut plane2: Plane = Plane::new();

    for (_, entry_) in lines.enumerate() {
        let entry = entry_.unwrap();
        let mut map = entry.split(" -> ").map(|v| Point::from(v.to_string()));

        let (p1, p2): (Point, Point) = (map.next().unwrap(), map.next().unwrap());
        let line = (p1.clone(), p2.clone());

        if p1.x == p2.x || p1.y == p2.y {
            plane1.add_line(line.clone());
            plane2.add_line(line);
        } else if (p1.x - p2.x).abs() == (p1.y - p2.y).abs() {
            plane2.add_line(line);
        }
    }

    println!("plane1:\n{}", plane1.draw(10));
    println!("part1: {}", plane1.count_at_least(2));
    println!("plane2:\n{}", plane2.draw(10));
    println!("part2: {}", plane2.count_at_least(2));
}
