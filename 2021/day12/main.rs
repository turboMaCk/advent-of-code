use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Lines};
use std::error;

#[derive(Debug)]
struct Graph(HashMap<String,Vec<String>>);

impl Graph {
    fn new() -> Self {
        Graph(HashMap::new())
    }

    fn insert(&mut self, source: String, dest: String) {
        // from source to dest
        match self.0.get_mut(&source) {
            None => { self.0.insert(source.clone(), vec![dest.clone()]); },
            Some(xs) => { xs.push(dest.clone()); }
        }

        // from dest to source
        match self.0.get_mut(&dest) {
            None => { self.0.insert(dest, vec![source]); },
            Some(xs) => { xs.push(source); }
        }
    }

    fn go_from(&self, node: &str, path: &Vec<String>, result: &mut Vec<Vec<String>>) {
        let dests = self.0.get(&node.to_string()).unwrap();
        let ch = node.chars().next().unwrap();

        // avoid cycle
        if ch.is_lowercase() {
            if path.contains(&node.to_string()) {
                // padadam pam
                return ()
            }
        }

        let mut this_path = path.clone();
        this_path.push(node.to_string());

        if node == "end" {
            result.push(this_path);
            return ();
        }

        // continue searching for end
        dests.iter().for_each(|d| {
            self.go_from(d, &this_path, result)
        });
    }

    fn go(&self) -> Vec<Vec<String>> {
        let mut result = Vec::new();
        let path = Vec::new();
        self.go_from("start", &path, &mut result);
        result
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = File::open("day12/input.txt").unwrap();
    let lines: Lines<BufReader<File>> = BufReader::new(file).lines();

    let mut graph = Graph::new();
    for res in lines {
        let line = res?;

        let parts: Vec<&str> = line.split("-").collect();
        graph.insert(parts[0].to_string(), parts[1].to_string());
    }

    println!("{}", graph.go().len());
    Ok(())
}
