use std::fs;
use std::error;

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
    fn get(&self, byte: usize) -> bool {
        let bucket = byte / 4;
        let pos = byte % 4;

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

    fn read(&mut self, size: usize) -> u32 {
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

    fn get_pointer(&self) -> u32 {
        self.pointer as u32
    }
}

fn is_new_line(ch: &char) -> bool {
    *ch == '\n'
}

#[derive(Debug)]
enum Id {
    Literal,
    Operator,
}

#[derive(Debug)]
struct Header {
    version: u32,
    id: Id,
}

fn read_header(buffer: &mut Buffer) -> Header {
    let version = buffer.read(3);
    let id = match buffer.read(3) {
        4 => Id::Literal,
        _ => Id::Operator,
    };

    Header {version, id}
}

fn read_literal(buffer: &mut Buffer) -> u32 {
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
    TotalLen(u32),
    SubPackets(u32),
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

fn part1(buffer: &mut Buffer) -> u32 {
    let header = read_header(buffer);
    println!("{:?}", header);
    match header.id {
        Id::Literal => {
            read_literal(buffer);
            header.version
        },
        _ => {
            match read_operator_len(buffer) {
                LengthTypeId::TotalLen(bits) => {
                    let start = buffer.get_pointer();
                    let mut sum = header.version;
                    while buffer.get_pointer() < start + bits {
                        sum += part1(buffer);
                    }
                    sum
                },
                LengthTypeId::SubPackets(num) => {
                    let mut sum = header.version;
                    for _ in 0..num {
                        sum += part1(buffer)
                    }
                    sum
                }
            }
        }
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let init: String = fs::read_to_string("day16/input.txt")?;

    let mut buffer = Buffer::from(init);

    println!("{}", buffer.print_bin());
    println!("part1: {}", part1(&mut buffer));

    Ok(())
}
