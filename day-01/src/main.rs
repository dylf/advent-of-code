use std::{env, fs::File, io::Read};

fn main() {
    let args: Vec<String> = env::args().collect();
    dbg!(&args);
    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let mut max_elf_index = 0;
    let mut max_elf_calories = 0;
    let mut curr_total_calories = 0;
    let mut curr_elf_index = 0;

    let mut calorie_totals = Vec::<i32>::new();

    for line in file_contents.lines().into_iter() {
        if line.is_empty() {
            if curr_total_calories > max_elf_calories {
                max_elf_calories = curr_total_calories;
                max_elf_index = curr_elf_index;
            }
            calorie_totals.push(curr_total_calories);
            curr_elf_index = curr_elf_index + 1;
            curr_total_calories = 0;
            continue;
        }

        curr_total_calories = curr_total_calories + line.parse::<i32>().unwrap();
    }

    if curr_total_calories > max_elf_calories {
        max_elf_calories = curr_total_calories;
        max_elf_index = curr_elf_index;
    }
    calorie_totals.push(curr_total_calories);

    println!(
        "Elf {} is carrying {} calories",
        max_elf_index + 1,
        max_elf_calories,
    )
}
