use std::error;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

#[derive(Debug)]
struct Map(Vec<Vec<u32>>);

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
        let height = self.0.len();
        let width = self.0[0].len();

        let mut low_points = Vec::new();
        for (i, row) in self.0.iter().enumerate() {
            for (j, cell) in row.iter().enumerate() {
                let mut surrounding = Vec::new();

                // 4 corners
                if i == 0 && j == 0 {
                    surrounding.push(self.0[0][1]);
                    surrounding.push(self.0[1][0]);
                } else if i == 0 && j + 1 == width {
                    surrounding.push(self.0[0][width - 2]);
                    surrounding.push(self.0[1][width - 1]);
                } else if i + 1 == height && j + 1 == width {
                    surrounding.push(self.0[height-1][1]);
                    surrounding.push(self.0[height-2][0]);
                } else if i + 1 == height && j == 0 {
                    surrounding.push(self.0[height-1][width - 2]);
                    surrounding.push(self.0[height-2][width - 1]);

                // 4 edges
                } else if i == 0 {
                    surrounding.push(self.0[0][j-1]);
                    surrounding.push(self.0[0][j+1]);
                    surrounding.push(self.0[1][j]);
                } else if j + 1 == width {
                    surrounding.push(self.0[i-1][j]);
                    surrounding.push(self.0[i+1][j]);
                    surrounding.push(self.0[i][j - 1]);
                } else if i + 1 == height {
                    surrounding.push(self.0[i][j-1]);
                    surrounding.push(self.0[i][j+1]);
                    surrounding.push(self.0[i-1][j]);
                } else if j == 0 {
                    surrounding.push(self.0[i-1][j]);
                    surrounding.push(self.0[i+1][j]);
                    surrounding.push(self.0[i][j+1]);

                // rest
                } else {
                    surrounding.push(self.0[i-1][j]);
                    surrounding.push(self.0[i+1][j]);
                    surrounding.push(self.0[i][j-1]);
                    surrounding.push(self.0[i][j+1]);
                }

                if surrounding.len() > 0 && surrounding.iter().all(|s| s > cell) {
                    low_points.push(*cell);
                }
            }
        }

        low_points
    }
}

/*
bad: 242, 182
more than 242
*/
fn part1(map: &Map) -> u32 {
    let points = map.low_points();

    points.iter().map(|p| *p + 1).sum()
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = File::open("day09/input.txt")?;
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut map = Map::new();
    for (_, line) in lines.enumerate() {
        map.fill_row(line?);
    }

    println!("part1 {:?}", part1(&map));

    Ok(())
}
