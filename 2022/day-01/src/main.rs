use std::{env, fs::File, io::Read};

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let mut calorie_totals = Vec::<i32>::new();

    let mut curr_total_calories = 0;
    for line in file_contents.lines().into_iter() {
        if line.is_empty() {
            calorie_totals.push(curr_total_calories);
            curr_total_calories = 0;
            continue;
        }

        curr_total_calories = curr_total_calories + line.parse::<i32>().unwrap();
    }

    // Push the final count after the loop
    calorie_totals.push(curr_total_calories);

    calorie_totals.sort_by(|a, b| b.cmp(a));

    println!(
        "Top elf is carrying {} calories",
        calorie_totals.clone().first().unwrap(),
    );

    println!(
        "Top 3 elves are carrying {} calories",
        calorie_totals.clone().iter().take(3).sum::<i32>(),
    )
}
