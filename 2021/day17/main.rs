use std::cmp::{max, min};
// use std::collections::HashSet;
use std::error;
use std::fs;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    fn try_max_height(&mut self, target: &Target, orig_vel: &Vec2) -> i64 {
        let mut max_val = i64::MIN;
        let mut vel: Vec2 = orig_vel.clone();

        // let mut trace: HashSet<Vec2> = HashSet::new();
        loop {
            // trace.insert(self.clone());

            self.add(&vel);
            max_val = max(max_val, self.0[1]);

            if target.hit(&self) {
                // draw_grid(0..40, -20..20, |x, y| {
                //     let v = Vec2::new(x, y);
                //     if trace.contains(&v) {
                //         return Some('#');
                //     }

                //     if target.hit(&v) {
                //         return Some('T');
                //     }
                //     None
                // });
                break;
            }

            if self.0[1] < target.min_y() || self.0[0] > target.max_x() {
                max_val = i64::MIN;
                break;
            }

            vel.drag();
            vel.add(&Vec2::new(0, -1));
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
        let splitted: Vec<&str> = croped.split(", y=").collect();

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

    fn max_x(&self) -> i64 {
        max(self.x[0], self.x[1])
    }

    fn min_y(&self) -> i64 {
        min(self.y[0], self.y[1])
    }
}

fn parts(position: &Vec2, target: &Target) -> (i64, i64) {
    let pos = position.clone();

    let mut max_val = i64::MIN;
    let mut count = 0;

    for x in 1..255 {
        for y in -255..255 {
            let mut p = pos.clone();
            let val = p.try_max_height(&target, &Vec2::new(x, y));

            if val != i64::MIN {
                count += 1;
            }

            max_val = max(max_val, val);
        }
    }

    (max_val, count)
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let init: String = fs::read_to_string("day17/input.txt")?;

    let target = Target::from(init);

    let pos = Vec2::init();

    let (p1, p2) = parts(&pos, &target);
    println!("part1: {}\n part2: {}", p1, p2);
    Ok(())
}

#[allow(dead_code)]
fn draw_grid<F>(x_range: Range<i64>, y_range: Range<i64>, get_char: F)
where
    F: Fn(i64, i64) -> Option<char>,
{
    let mut str = "".to_string();

    for y in y_range {
        for x in x_range.clone() {
            let chr: char = get_char(x, y).unwrap_or('.');
            str.push(chr);
        }

        str.push('\n');
    }

    print!("{}", str);
}
