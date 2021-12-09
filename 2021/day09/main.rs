use std::error;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

#[derive(Debug, Clone)]
struct Map(Vec<Vec<u32>>);

#[derive(Debug, Clone, Copy)]
struct Point {
    value: u32,
    x: usize,
    y: usize,
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

    fn low_points(&self) -> Vec<u32> {
        let mut low_points = Vec::new();
        for (i, row) in self.0.iter().enumerate() {
            for (j, cell) in row.iter().enumerate() {
                let surrounding = self.get_surrounding(j, i);
                if surrounding.len() > 0 && surrounding.iter().all(|p| p.value > *cell) {
                    low_points.push(*cell);
                }
            }
        }

        low_points
    }

    fn solve_basin(&mut self, p: Point) -> usize {
        // needs to get point again
        let pnt = self.get_point(p.x,p.y);
        if pnt.value == 9 {
            0
        } else {
            // ugly mutation
            self.0[p.y][p.x] = 9;

            let mut count = 1;
            for point in self.get_surrounding(p.x, p.y).iter() {
                count += self.solve_basin(*point);
            }

            count
        }
    }

    fn basins(&mut self) -> Vec<usize> {
        let height = self.0.len();
        let width = self.0[0].len();

        let mut basins: Vec<usize> = Vec::new();

        for y in 0..height {
            for x in 0..width {
                let basin_size = self.solve_basin(self.get_point(x, y));
                if basin_size > 0 {
                    basins.push(basin_size);
                }
            }
        }

        basins
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
}

fn part1(map: &Map) -> u32 {
    let points = map.low_points();

    points.iter().map(|p| *p + 1).sum()
}

fn part2(map: &mut Map) -> usize {
    let mut basins = map.basins();
    basins.sort();
    basins.reverse();

    basins[0] * basins[1] * basins[2]
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = File::open("day09/input.txt")?;
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut map = Map::new();
    for (_, line) in lines.enumerate() {
        map.fill_row(line?);
    }

    println!("part1 {:?}", part1(&map));
    println!("part2 {:?}", part2(&mut map));

    Ok(())
}
