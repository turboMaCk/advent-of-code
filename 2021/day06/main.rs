use std::error::Error;
use std::fs;

type Gen = usize;
const BABY_FOR: Gen = 9;
const REPRODUCE_IN: Gen = 7;

#[derive(Debug)]
struct Sea{
    gens: [Gen; REPRODUCE_IN],
    babies: [Gen; BABY_FOR],
}

impl Sea {
    fn from_vec(vec: Vec<Gen>) -> Self {
        let mut gens = [0; REPRODUCE_IN];

        for gen in vec.iter() {
            gens[*gen] += 1;
        }

        Sea{ gens, babies: [0; BABY_FOR] }
    }

    fn cycle(&mut self, steps: usize) {
        for _ in 0..steps {
            let do_fuck = self.gens[0] + self.babies[0];
            // update gens
            for i in 1..REPRODUCE_IN {
                self.gens[i-1] = self.gens[i];
            }

            // restore elders
            self.gens[REPRODUCE_IN - 1] = do_fuck ;

            // update babies
            for i in 1..BABY_FOR {
                self.babies[i-1] = self.babies[i];
            }

            // new borns
            self.babies[BABY_FOR - 1] = do_fuck;

        }
    }

    fn count(&self) -> usize {
        let mut count = 0;
        for gen in self.gens.iter() {
            count += gen;
        }

        for gen in self.babies.iter() {
            count += gen;
        }

        count
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let init: Vec<Gen> = fs::read_to_string("day06/input.txt")?
        .split(",")
        .filter_map(|v| v.trim().parse().ok())
        .collect();

    let mut sea = Sea::from_vec(init);
    sea.cycle(256);

    println!("{:?}", sea.count());
    Ok(())
}
