use std::{collections::VecDeque, env, fs::File, io::Read};

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let mut stacks: Vec<VecDeque<char>> = vec![];
    let mut collected_stacks = false;

    for line in file_contents.lines().into_iter() {
        let instruction = line.split_whitespace().collect::<Vec<&str>>();

        if instruction.len() > 0 && instruction[0] == "1" {
            println!("inital_stack {:?}\n", stacks);
            collected_stacks = true;
            continue;
        }

        if collected_stacks {
            if instruction.len() < 6 {
                continue;
            }
            let amt = instruction[1].parse::<usize>().unwrap();
            let from = instruction[3].parse::<usize>().unwrap();
            let to = instruction[5].parse::<usize>().unwrap();

            // problem 1
            // for _num in 0..amt {
            //     let val = stacks[from - 1].pop_front().unwrap();
            //     stacks[to - 1].push_front(val);
            // }

            let mut new_stack = stacks[from - 1].drain(0..amt).collect::<VecDeque<char>>();
            let mut old_stack = stacks[to - 1].clone();
            new_stack.append(&mut old_stack);
            stacks[to - 1] = new_stack;

            continue;
        }

        line.chars()
            .enumerate()
            .map(|(i, chr)| {
                if stacks.len() <= i / 4 {
                    stacks.push(VecDeque::new());
                }
                if chr != ' ' && i % 4 == 1 {
                    stacks[i / 4].push_back(chr)
                }
            })
            .count();
    }

    let result = stacks
        .iter()
        .map(|stack| stack.front().unwrap())
        .collect::<String>();

    println!("Tops {:?}", result)
}
