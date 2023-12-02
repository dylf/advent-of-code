use std::{env, fs::File, io::Read};

fn try_add_signal_strength(
    reg_value: i32,
    curr_cycle: i32,
    cycles: &Vec<i32>,
    target: &mut Vec<i32>,
) {
    if cycles.contains(&curr_cycle) {
        target.push(reg_value * curr_cycle);
    }
}

fn sum_signal_strengths(instructions: &String, cycles: &Vec<i32>) -> i32 {
    let mut curr_cycle = 0;
    let mut reg_value = 1;
    let mut target_registers: Vec<i32> = vec![];

    for line in instructions.lines() {
        let instruction = line.split_whitespace().collect::<Vec<&str>>();
        let op = instruction[0];
        let mut arg = 0;

        if instruction.len() == 2 {
            arg = instruction[1].parse::<i32>().expect("Second arg is num");
        }

        if op == "noop" {
            curr_cycle += 1;
        } else {
            curr_cycle += 1;
            try_add_signal_strength(reg_value, curr_cycle, cycles, &mut target_registers);
            curr_cycle += 1;
        }

        try_add_signal_strength(reg_value, curr_cycle, cycles, &mut target_registers);
        reg_value += arg
    }

    target_registers.iter().sum::<i32>()
}

fn try_draw_pixel(reg_value: i32, curr_cycle: i32, screen: &mut Vec<char>) {
    match ((curr_cycle - 1) % 40) - reg_value {
        -1 | 0 | 1 => screen.push('#'),
        _ => screen.push('.'),
    }
}

fn render_crt(instructions: &String) {
    let mut curr_cycle = 0;
    let mut reg_value = 1;
    let mut screen: Vec<char> = vec![];

    for line in instructions.lines() {
        let instruction = line.split_whitespace().collect::<Vec<&str>>();
        let op = instruction[0];
        let mut arg = 0;

        if instruction.len() == 2 {
            arg = instruction[1].parse::<i32>().expect("Second arg is num");
        }

        if op == "noop" {
            curr_cycle += 1;
        } else {
            curr_cycle += 1;
            try_draw_pixel(reg_value, curr_cycle, &mut screen);
            curr_cycle += 1;
        }

        try_draw_pixel(reg_value, curr_cycle, &mut screen);
        reg_value += arg;
    }

    screen
        .chunks(40)
        .for_each(|line| println!("{}", line.iter().cloned().collect::<String>()))
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let cycles = vec![20, 60, 100, 140, 180, 220];
    println!(
        "Sum of signals {:?} at cycles {:?}",
        sum_signal_strengths(&file_contents, &cycles),
        cycles
    );

    render_crt(&file_contents)
}
