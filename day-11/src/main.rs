use std::{collections::VecDeque, env, fs::File, io::Read, str::FromStr};

#[derive(Debug)]
struct Monkey {
    operation: (char, String),
    test: i32,
    true_target: i32,
    false_target: i32,
    items: VecDeque<u64>,
    inspect_count: u64,
}

impl Monkey {
    fn has_items(&mut self) -> bool {
        self.items.len() > 0
    }

    fn inspect(&mut self) {
        self.inspect_count += 1;
        self.increase_worry();
    }

    fn increase_worry(&mut self) {
        if !self.has_items() {
            ()
        }

        let (operator, arg) = &self.operation;

        let old = self.items[0].clone();
        let num = arg.parse::<u64>().ok().unwrap_or(old);

        match operator {
            '*' => self.items[0] *= num,
            '+' => self.items[0] += num,
            _ => (),
        }
    }

    fn decrease_worry(&mut self) {
        if !self.has_items() {
            ()
        }
        self.items[0] = &self.items[0] / 3
    }

    fn throw(&mut self) -> (i32, u64) {
        if !self.has_items() {
            ()
        }

        if self.items[0] % self.test as u64 == 0 {
            return (self.true_target, self.items.pop_front().unwrap());
        }
        return (self.false_target, self.items.pop_front().unwrap());
    }

    fn catch(&mut self, item: u64) {
        self.items.push_back(item)
    }
}

impl<'a> FromIterator<&'a str> for Monkey {
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        let mut iter = iter.into_iter();

        // The vec index can serve as the id so I don't need it in my struct
        let _id = iter.next();

        let items = iter
            .next()
            .unwrap()
            .split(":")
            .last()
            .unwrap()
            .trim()
            .split(", ")
            .map(|i| i.parse::<u64>().unwrap())
            .collect::<VecDeque<u64>>();

        let operation = iter
            .next()
            .unwrap()
            .split_whitespace()
            .rev()
            .take(2)
            .collect::<Vec<&str>>();

        let operator = operation[1].chars().nth(0).unwrap();
        let arg = operation[0].to_string();

        let test = iter
            .next()
            .unwrap()
            .split_whitespace()
            .last()
            .unwrap()
            .parse::<i32>()
            .unwrap();

        let true_target = iter
            .next()
            .unwrap()
            .split_whitespace()
            .last()
            .unwrap()
            .parse::<i32>()
            .unwrap();

        let false_target = iter
            .next()
            .unwrap()
            .split_whitespace()
            .last()
            .unwrap()
            .parse::<i32>()
            .unwrap();

        Monkey {
            operation: (operator, arg),
            test,
            true_target,
            false_target,
            items,
            inspect_count: 0,
        }
    }
}

impl FromStr for Monkey {
    type Err = String;
    fn from_str(s: &str) -> Result<Monkey, Self::Err> {
        return Ok(s.split("\n").collect::<Monkey>());
    }
}

fn calculate_monkey_business(monkeys: &Vec<Monkey>) -> u64 {
    let mut inspections = monkeys
        .iter()
        .map(|m| m.inspect_count)
        .collect::<Vec<u64>>();
    inspections.sort_by(|a, b| b.cmp(a));
    inspections[0] * inspections[1]
}

fn run_simulation(monkeys: &mut Vec<Monkey>, total_simulations: u32, worry_decreases: bool) {
    let test_product = monkeys.iter().map(|m| m.test).product::<i32>() as u64;
    for _ in 0..total_simulations {
        for i in 0..monkeys.len() {
            while monkeys[i].has_items() {
                monkeys[i].inspect();
                if worry_decreases {
                    monkeys[i].decrease_worry()
                }
                let (target, mut item) = monkeys[i].throw();
                item = item % test_product;
                monkeys[target as usize].catch(item);
            }
        }
    }
    let monkey_business = calculate_monkey_business(monkeys);
    println!("monkey_business: {:?}", monkey_business)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let monkey_input = file_contents.split("\n\n");

    let mut monkeys = monkey_input
        .clone()
        .map(|m| m.parse::<Monkey>().expect("Good input"))
        .collect::<Vec<Monkey>>();

    run_simulation(&mut monkeys, 20, true);

    let mut monkeys = monkey_input
        .map(|m| m.parse::<Monkey>().expect("Good input"))
        .collect::<Vec<Monkey>>();
    run_simulation(&mut monkeys, 10000, false);
}
