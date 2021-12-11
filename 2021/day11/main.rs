use std::error;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

#[derive(Debug, Clone, Copy)]
struct Point {
    value: i32,
    x: usize,
    y: usize,
}

#[derive(Debug, Clone)]
struct Map(Vec<Vec<i32>>);
impl Map {
    fn new() -> Self {
        Map(Vec::new())
    }

    fn fill_row(&mut self, row: &String) {
        let mut cells: Vec<i32> = Vec::new();

        for ch in row.chars() {
            cells.push(ch.to_string().parse().unwrap());
        }

        self.0.push(cells);
    }

    fn get_point(&self, x: usize, y: usize) -> Point {
        Point {
            x,
            y,
            value: self.0[y][x],
        }
    }

    fn get_surrounding(&self, x: usize, y: usize) -> Vec<Point> {
        let i = y;
        let j = x;

        let height = self.0.len();
        let width = self.0[0].len();
        let mut surrounding = Vec::new();

        // 4 corners
        if i == 0 && j == 0 {
            surrounding.push(self.get_point(1, 0));
            surrounding.push(self.get_point(0, 1));
            surrounding.push(self.get_point(1, 1));
        } else if i == 0 && j + 1 == width {
            surrounding.push(self.get_point(width - 2, 0));
            surrounding.push(self.get_point(width - 1, 1));
            surrounding.push(self.get_point(width - 2, 1));
        } else if i + 1 == height && j + 1 == width {
            surrounding.push(self.get_point(width - 2, height - 1));
            surrounding.push(self.get_point(width - 1, height - 2));
            surrounding.push(self.get_point(width - 2, height - 2));
        } else if i + 1 == height && j == 0 {
            surrounding.push(self.get_point(1, height - 1));
            surrounding.push(self.get_point(0, height - 2));
            surrounding.push(self.get_point(1, height - 2));

        // 4 edges
        } else if i == 0 {
            surrounding.push(self.get_point(j - 1, 0));
            surrounding.push(self.get_point(j + 1, 0));
            surrounding.push(self.get_point(j, 1));
            surrounding.push(self.get_point(j - 1, 1));
            surrounding.push(self.get_point(j + 1, 1));
        } else if j + 1 == width {
            surrounding.push(self.get_point(j, i - 1));
            surrounding.push(self.get_point(j, i + 1));
            surrounding.push(self.get_point(j - 1, i));
            surrounding.push(self.get_point(j - 1, i + 1));
            surrounding.push(self.get_point(j - 1, i - 1));
        } else if i + 1 == height {
            surrounding.push(self.get_point(j - 1, i));
            surrounding.push(self.get_point(j + 1, i));
            surrounding.push(self.get_point(j, i - 1));
            surrounding.push(self.get_point(j + 1, i - 1));
            surrounding.push(self.get_point(j - 1, i - 1));
        } else if j == 0 {
            surrounding.push(self.get_point(j, i - 1));
            surrounding.push(self.get_point(j, i + 1));
            surrounding.push(self.get_point(j + 1, i));
            surrounding.push(self.get_point(j + 1, i + 1));
            surrounding.push(self.get_point(j + 1, i - 1));

        // rest
        } else {
            surrounding.push(self.get_point(j, i - 1));
            surrounding.push(self.get_point(j, i + 1));
            surrounding.push(self.get_point(j - 1, i));
            surrounding.push(self.get_point(j + 1, i));
            surrounding.push(self.get_point(j + 1, i + 1));
            surrounding.push(self.get_point(j + 1, i - 1));
            surrounding.push(self.get_point(j - 1, i + 1));
            surrounding.push(self.get_point(j - 1, i - 1));
        }

        surrounding
    }

    // just increase the charge
    fn cycle(&mut self) {
        let height = self.0.len();
        let width = self.0[0].len();

        for y in 0..height {
            for x in 0..width {
                self.0[y][x] += 1;
            }
        }
    }

    // just flash (without charging!)
    fn flash(&mut self) -> u64 {
        let height = self.0.len();
        let width = self.0[0].len();

        let mut count = 0;
        for y in 0..height {
            for x in 0..width {
                if self.0[y][x] > 9 {
                    // mark as flashed but not discharged
                    self.0[y][x] = -1;

                    count += 1;
                }
            }
        }

        count
    }

    // charge surrounding after flash
    fn charge(&mut self) {
        let height = self.0.len();
        let width = self.0[0].len();

        for y in 0..height {
            for x in 0..width {
                if self.0[y][x] == -1 {
                    let surrounding = self.get_surrounding(x,y);

                    surrounding.iter().for_each(|p| {
                        // don't charge those which flashed
                        if p.value > 0 {
                            self.0[p.y][p.x] += 1;
                        }
                    });

                    // mark as discharged
                    self.0[y][x] = 0;
                }
            }
        }

    }

    // put cycle, flash and charge together
    fn step(&mut self) -> u64 {
        self.cycle();
        let mut flashes: u64 = 0;

        let mut last_flash = self.flash();
        while (last_flash > 0) {
            flashes += last_flash;
            self.charge();

            last_flash = self.flash();
        }

        flashes
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
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = File::open("day11/input.txt")?;
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut map = Map::new();

    for (_, res) in lines.enumerate() {
        let line = res?;
        map.fill_row(&line);
    }

    let mut flashes: u64 = 0;

    for _ in 0..100 {
        flashes += map.step();
    }
    println!("{}", flashes);
    // println!("{}", map.print());

    // println!("----");
    // map.step();
    // println!("{}", map.print());

    // println!("----");
    // map.step();
    // println!("{}", map.print());

    // println!("----");
    // map.step();
    // println!("{}", map.print());

    Ok(())
}
