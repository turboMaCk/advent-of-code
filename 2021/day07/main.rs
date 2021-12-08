use std::cmp::min;
use std::error::Error;
use std::fs;

fn align(input: Vec<i32>) -> i32 {
    let mut lowest_fuel = i32::MAX;

    for position in 0..*input.iter().max().unwrap() {
        let mut fuel = 0;

        for item in input.iter() {
            fuel += (item - position).abs();

            if fuel > lowest_fuel {
                break;
            }
        }

        lowest_fuel = min(lowest_fuel, fuel);
    }

    lowest_fuel
}

fn align2(input: Vec<i32>) -> i32 {
    let mut lowest_fuel = i32::MAX;

    for position in 0..*input.iter().max().unwrap() {
        let mut fuel: i32 = 0;

        for item in input.iter() {
            let val: i32 = (item - position).abs();
            fuel += (1..=val).sum::<i32>();

            if fuel > lowest_fuel {
                break;
            }
        }

        lowest_fuel = min(lowest_fuel, fuel);
    }

    lowest_fuel
}

fn main() -> Result<(), Box<dyn Error>> {
    let init: Vec<i32> = fs::read_to_string("day07/input.txt")?
        .split(",")
        .filter_map(|v| v.trim().parse().ok())
        .collect();

    println!("part1: {:?}", align(init.clone()));
    println!("part2: {:?}", align2(init));

    Ok(())
}
