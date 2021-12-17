use std::cmp::{max, min};
use std::error;
use std::fs;

const MAX_STEPS: i64 = 1024;

#[derive(Debug, Clone)]
struct Vec2([i64; 2]);

impl Vec2 {
    fn init() -> Self {
        Vec2([0, 0])
    }

    fn new(x: i64, y: i64) -> Self {
        Vec2([x, y])
    }

    fn x(&self) -> i64 {
        self.0[0]
    }

    fn y(&self) -> i64 {
        self.0[1]
    }

    fn add(&mut self, vec: &Vec2) {
        self.0[0] += vec.0[0];
        self.0[1] += vec.0[1];
    }

    fn drag(&mut self) {
        if self.0[0] == 0 {
            return;
        }
        let k = if self.0[0] >= 0 { -1 } else { 1 };

        self.0[0] += k;
    }

    fn try_max_height(&mut self, target: &Target, orig_vel: Vec2) -> i64 {
        let mut max_val = i64::MIN;
        let mut vel = orig_vel.clone();

        let mut i = 0;
        loop {
            self.add(&vel);
            max_val = max(max_val, self.0[1]);

            if target.hit(&self) {
                println!("hit {:?}", orig_vel);
                break;
            }

            if i == MAX_STEPS {
                max_val = i64::MIN;
                break;
            }

            vel.drag();
            vel.add(&Vec2::new(0, -1));

            i += 1;
        }

        max_val
    }
}

#[derive(Debug)]
struct Target {
    x: [i64; 2],
    y: [i64; 2],
}

impl From<String> for Target {
    fn from(str: String) -> Self {
        let mut croped = str.replace("target area: x=", "");
        croped = croped.replace("\n", "");
        let splitted: Vec<&str> = croped.split(", y=-").collect();

        let x_data: Vec<&str> = splitted[0].split("..").collect();
        let x = [x_data[0].parse().unwrap(), x_data[1].parse().unwrap()];

        let y_data: Vec<&str> = splitted[1].split("..").collect();
        let y = [y_data[0].parse().unwrap(), y_data[1].parse().unwrap()];

        Target { x, y }
    }
}

impl Target {
    fn hit(&self, vec: &Vec2) -> bool {
        vec.x() <= max(self.x[0], self.x[1])
            && vec.x() >= min(self.x[0], self.x[1])
            && vec.y() <= max(self.y[0], self.y[1])
            && vec.y() >= min(self.y[0], self.y[1])
    }

    /**
    Had to reverse enginner it from example because requirements are
    completely uncler about ranges... I consider this wrong solution.
    I'm just trying to replicate bug of the author.
     */
    fn max_x(&self) -> i64 {
        max(self.x[0], self.x[1])
    }

    fn max_y(&self) -> i64 {
        max(self.y[0], self.y[1])
    }
}

fn part1(position: &Vec2, target: &Target) -> i64 {
    let pos = position.clone();

    let mut max_val = i64::MIN;

    // This is where the bug is replicated
    for x in 0..target.max_x() {
        for y in 0..target.max_y() {
            let mut p = pos.clone();
            let val = p.try_max_height(&target, Vec2::new(x, y));

            max_val = max(max_val, val);
        }
    }

    max_val
}

// 1035 -> too low
fn main() -> Result<(), Box<dyn error::Error>> {
    let init: String = fs::read_to_string("day17/input.txt")?;

    let target = Target::from(init);
    println!("target: {:?}", target);

    let pos = Vec2::init();

    println!("part1: {}", part1(&pos, &target));
    Ok(())
}
