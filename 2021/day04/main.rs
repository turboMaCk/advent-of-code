use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

const BOARD_SIZE: usize = 5;

#[derive(Debug, Clone)]
struct Board {
    cells: [i32; BOARD_SIZE * BOARD_SIZE],
    cur: usize,
}

impl Board {
    fn new() -> Board {
        Board {
            cells: [0; BOARD_SIZE * BOARD_SIZE],
            cur: 0,
        }
    }

    fn fill<I>(&mut self, data: I)
    where
        I: Iterator<Item = i32>,
    {
        for cell in data {
            self.cells[self.cur] = cell;
            self.cur += 1;
        }
    }

    fn mark(&mut self, val: i32) {
        for index in 0..(BOARD_SIZE * BOARD_SIZE) {
            if self.cells[index] == val {
                self.cells[index] = -1;
            }
        }
    }

    fn won_row(&self, n: usize) -> bool {
        let start = n * BOARD_SIZE;
        for i in start..start+BOARD_SIZE {
            if self.cells[i] != -1 {
                return false;
            }
        }

        return true;
    }

    fn won_column(&self, n: usize) -> bool {
        for i in 0..BOARD_SIZE {
            if self.cells[i*BOARD_SIZE + n] != -1 {
                return false;
            }
        }

        return true;
    }

    fn won(&self) -> bool {
        for i in 0..BOARD_SIZE {
            if self.won_row(i) {
                return true
            }
        }

        for i in 0..BOARD_SIZE {
            if self.won_column(i) {
                return true
            }
        }

        return false;
    }

    fn won_sum(&self) -> Option<i32> {
        if self.won() {
            let mut sum: i32 = 0;
            for i in 0..(BOARD_SIZE*BOARD_SIZE) {
                if self.cells[i] > 0 {
                    sum += self.cells[i];
                }
            }
            Some(sum)
        } else {
            None
        }
    }
}


fn part1(turns: Vec<i32>, mut boards: Vec<Board>) {
    let mut sum: i32 = 0;
    let mut won_val: i32 = 0;

    'lookup: for val in turns.into_iter() {
        for index in 0..boards.len() {
            boards[index].mark(val);

            match boards[index].won_sum() {
                None => {}
                Some(s) => {
                    sum = s;
                    won_val = val;
                    break 'lookup;
                }
            }
        }
    }

    println!("{:?}", sum * won_val);
}

fn part2(turns: Vec<i32>, mut boards: Vec<Board>) {
    let mut sum: i32 = 0;
    let mut won_val: i32 = 0;
    let boards_won: &mut Vec<bool> = &mut boards.clone().into_iter().map(|_| false).collect();

    'lookup: for val in turns.into_iter() {
        for index in 0..boards.len() {
            boards[index].mark(val);

            match boards[index].won_sum() {
                None => {}
                Some(s) => {
                    boards_won[index] = true;

                    if boards_won.into_iter().all(|b| *b) {
                        sum = s;
                        won_val = val;
                        break 'lookup;
                    }
                }
            }
        }
    }

    println!("{:?}", sum * won_val);
}

fn main() {
    let file = File::open("day04/input.txt").unwrap();
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();
    let mut turns: Vec<i32> = Vec::new();
    let mut boards: Vec<Board> = Vec::new();
    let mut current_board: Board = Board::new();

    for (index, line) in lines.enumerate() {
        match index {
            // parse turns
            0 => {
                turns = line
                    .unwrap()
                    .split(",")
                    .map(|v| v.parse().unwrap())
                    .collect();
            }
            // empty line
            1 => {}
            // parse boards
            _ => {
                let string = line.unwrap();
                println!("{:?}", string);

                // empty line (between boards)
                if string == "".to_string() {
                    println!("================");
                    boards.push(current_board.clone());
                    current_board = Board::new();
                } else {
                    let vals = string.split(" ").filter_map(|v| v.parse().ok());
                    current_board.fill(vals);
                }
            }
        }
    }
    boards.push(current_board.clone());

    part1(turns.clone(), boards.clone());
    part2(turns.clone(), boards);
}
