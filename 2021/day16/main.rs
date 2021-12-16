use std::cmp::{max, min};
use std::error;
use std::fs;

// we're working with 8bits rather than 4 bits
fn char_to_byte(ch: char) -> u8 {
    let mut int = ch as u8 - 48;

    if int > 10 {
        int -= 7;
    }

    int
}

#[derive(Debug, Clone)]
struct Buffer {
    data: Vec<u8>,
    pointer: usize,
}

impl From<String> for Buffer {
    fn from(str: String) -> Self {
        let mut data = Vec::new();
        for ch in str.chars() {
            if is_new_line(&ch) {
                continue;
            }
            data.push(char_to_byte(ch));
        }

        Buffer { data, pointer: 0 }
    }
}

impl Buffer {
    fn get(&self, bit: usize) -> bool {
        let bucket = bit / 4;
        let pos = bit % 4;

        let int = self.data[bucket];
        let mask = 8 >> pos;

        let v = int & mask;

        v != 0
    }

    fn pop(&mut self) -> bool {
        let res = self.get(self.pointer);
        self.pointer += 1;
        res
    }

    fn read(&mut self, size: usize) -> u64 {
        let mut val = 0;

        for _ in 0..size {
            val = val << 1;
            if self.pop() {
                val += 1;
            }
        }

        val
    }

    fn print_bin(&self) -> String {
        let mut str = "".to_string();
        for i in 0..self.data.len() * 4 {
            if self.get(i) {
                str.push('1');
            } else {
                str.push('0');
            }
        }

        str
    }

    fn get_pointer(&self) -> u64 {
        self.pointer as u64
    }
}

fn is_new_line(ch: &char) -> bool {
    *ch == '\n'
}

#[derive(Debug)]
enum Id {
    Sum,
    Product,
    Min,
    Max,
    Literal,
    Greater,
    Lesser,
    Equal,
}

#[derive(Debug)]
struct Header {
    version: u64,
    id: Id,
}

fn read_header(buffer: &mut Buffer) -> Header {
    use Id::*;
    let version = buffer.read(3);
    let id = match buffer.read(3) {
        0 => Sum,
        1 => Product,
        2 => Min,
        3 => Max,
        4 => Literal,
        5 => Greater,
        6 => Lesser,
        _ => Equal,
    };

    Header { version, id }
}

fn read_literal(buffer: &mut Buffer) -> u64 {
    let mut read_next = true;

    let mut value = 0;
    while read_next {
        read_next = buffer.pop();
        value = value << 4;
        value += buffer.read(4);
    }

    value
}

#[derive(Debug)]
enum LengthTypeId {
    TotalLen(u64),
    SubPackets(u64),
}

fn read_operator_len(buffer: &mut Buffer) -> LengthTypeId {
    use LengthTypeId::*;

    let length_type_id = buffer.pop();

    if !length_type_id {
        TotalLen(buffer.read(15))
    } else {
        SubPackets(buffer.read(11))
    }
}

fn part1(buffer: &mut Buffer) -> u64 {
    let header = read_header(buffer);
    // println!("{:?}", header);
    match header.id {
        Id::Literal => {
            read_literal(buffer);
            header.version
        }
        _ => match read_operator_len(buffer) {
            LengthTypeId::TotalLen(bits) => {
                let start = buffer.get_pointer();
                let mut sum = header.version;
                while buffer.get_pointer() < start + bits {
                    sum += part1(buffer);
                }
                sum
            }
            LengthTypeId::SubPackets(num) => {
                let mut sum = header.version;
                for _ in 0..num {
                    sum += part1(buffer)
                }
                sum
            }
        },
    }
}

fn part2(buffer: &mut Buffer) -> u64 {
    use Id::*;
    let header = read_header(buffer);
    match header.id {
        Literal => read_literal(buffer),
        Sum => {
            let mut sum: u64 = 0;
            match read_operator_len(buffer) {
                LengthTypeId::TotalLen(bits) => {
                    let start = buffer.get_pointer();
                    while buffer.get_pointer() < start + bits {
                        sum += part2(buffer);
                    }
                }
                LengthTypeId::SubPackets(num) => {
                    for _ in 0..num {
                        sum += part2(buffer);
                    }
                }
            }
            sum
        }
        Product => {
            let mut prod: u64 = 1;
            match read_operator_len(buffer) {
                LengthTypeId::TotalLen(bits) => {
                    let start = buffer.get_pointer();
                    while buffer.get_pointer() < start + bits {
                        prod *= part2(buffer);
                    }
                }
                LengthTypeId::SubPackets(num) => {
                    for _ in 0..num {
                        prod *= part2(buffer)
                    }
                }
            }
            prod
        }
        Min => {
            let mut minv = u64::MAX;
            match read_operator_len(buffer) {
                LengthTypeId::TotalLen(bits) => {
                    let start = buffer.get_pointer();
                    while buffer.get_pointer() < start + bits {
                        minv = min(part2(buffer), minv);
                    }
                }
                LengthTypeId::SubPackets(num) => {
                    for _ in 0..num {
                        minv = min(minv, part2(buffer));
                    }
                }
            }
            minv
        }
        Max => {
            let mut maxv: u64 = 0;
            match read_operator_len(buffer) {
                LengthTypeId::TotalLen(bits) => {
                    let start = buffer.get_pointer();
                    while buffer.get_pointer() < start + bits {
                        maxv = max(part2(buffer), maxv);
                    }
                }
                LengthTypeId::SubPackets(num) => {
                    for _ in 0..num {
                        maxv = max(maxv, part2(buffer));
                    }
                }
            }
            maxv
        }
        Greater => {
            read_operator_len(buffer);
            let a = part2(buffer);
            let b = part2(buffer);

            if a > b {
                1
            } else {
                0
            }
        }
        Lesser => {
            read_operator_len(buffer);
            let a = part2(buffer);
            let b = part2(buffer);

            if a < b {
                1
            } else {
                0
            }
        }
        Equal => {
            read_operator_len(buffer);
            let a = part2(buffer);
            let b = part2(buffer);

            if a == b {
                1
            } else {
                0
            }
        }
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let init: String = fs::read_to_string("day16/input.txt")?;

    let mut buffer = Buffer::from(init);

    // println!("{}", buffer.print_bin());
    println!("part1: {}", part1(&mut buffer.clone()));
    println!("part2: {}", part2(&mut buffer));

    Ok(())
}
