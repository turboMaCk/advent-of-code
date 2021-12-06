use std::error::Error;
use std::fs;

#[derive(Debug)]
struct Sea(Vec<u32>);

impl Sea {
    fn from_vec(vec: Vec<u32>) -> Self {
        Sea(vec)
    }

    fn cycle(&mut self) {
        for i in 0..self.0.len() {
            if self.0[i] == 0 {
                self.0.push(8);
                self.0[i] = 6;
            } else {
                self.0[i] -= 1;
            }
        }
    }

    fn count(&self) -> usize {
        self.0.len()
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let init: Vec<u32> = fs::read_to_string("day06/input.txt")?
        .split(",")
        .filter_map(|v| v.trim().parse().ok())
        .collect();

    let mut sea = Sea::from_vec(init);

    for _ in 0..80 {
        sea.cycle();
    }

    println!("{:?}", sea.count());
    Ok(())
}
