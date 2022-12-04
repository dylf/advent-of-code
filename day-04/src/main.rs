use std::{env, fs::File, io::Read};

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let mut total_full_overlaps = 0;
    for line in file_contents.lines().into_iter() {
        let ranges: Vec<&str> = line.split(',').collect();
        let split_ranges: Vec<i32> = ranges
            .iter()
            .flat_map(|&range| range.split('-').collect::<Vec<&str>>())
            .map(|num: &str| num.parse::<i32>().unwrap())
            .collect();

        if (split_ranges[0] <= split_ranges[2]) && (split_ranges[1] >= split_ranges[3])
            || (split_ranges[0] >= split_ranges[2] && split_ranges[1] <= split_ranges[3])
        {
            total_full_overlaps += 1;
        }
    }

    let mut total_overlaps = 0;
    for line in file_contents.lines().into_iter() {
        let ranges: Vec<&str> = line.split(',').collect();
        let split_ranges: Vec<i32> = ranges
            .iter()
            .flat_map(|&range| range.split('-').collect::<Vec<&str>>())
            .map(|num: &str| num.parse::<i32>().unwrap())
            .collect();

        if (split_ranges[0] <= split_ranges[2] && split_ranges[1] >= split_ranges[2])
            || (split_ranges[0] >= split_ranges[2] && split_ranges[0] <= split_ranges[3])
        {
            total_overlaps += 1;
        }
    }

    println!("Total full overlaps {}", total_full_overlaps);
    println!("Total overlaps {}", total_overlaps)
}
