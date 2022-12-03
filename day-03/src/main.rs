use std::{
    collections::{HashMap, HashSet},
    env,
    fs::File,
    io::Read,
};

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let mut map = HashMap::new();

    let mut count = 1;
    for ch in 'a'..='z' {
        map.insert(ch, count);
        count += 1;
    }

    for ch in 'A'..='Z' {
        map.insert(ch, count);
        count += 1;
    }

    let mut total_priority = 0;
    for line in file_contents.lines().into_iter() {
        let line_len = line.len();
        let (first_comp_str, last_comp_str) = line.split_at(line_len / 2);
        let mut first_comp = first_comp_str.chars();
        let last_comp: HashSet<char> = last_comp_str.chars().collect();

        let common_char = first_comp.find(|&chr| last_comp.contains(&chr));

        if let Some(chr) = common_char {
            if let Some(pri) = map.get(&chr) {
                total_priority += pri;
            }
        }
    }

    let mut badge_priority = 0;
    let mut line_count = 0;
    let mut group = ["", "", ""];
    for line in file_contents.lines().into_iter() {
        group[(line_count % 3)] = line;
        if (line_count % 3) == 2 {
            let common_char = group[0].chars().find(|&chr| {
                group[1].chars().any(|chr2| chr2 == chr) && group[2].chars().any(|chr2| chr2 == chr)
            });

            if let Some(chr) = common_char {
                if let Some(pri) = map.get(&chr) {
                    badge_priority += pri;
                }
            }
        }
        line_count += 1;
    }

    println!("Total priority: {}", total_priority,);
    println!("Total badge priority: {}", badge_priority,);
}
