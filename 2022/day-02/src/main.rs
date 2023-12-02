use std::{env, fs::File, io::Read};

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let mut total_score = 0;

    for line in file_contents.lines().into_iter() {
        let mut round = line.split(' ');
        let opponent = round.next().unwrap();
        let player = round.next().unwrap();

        total_score += match player {
            "X" => 0,
            "Y" => 3,
            "Z" => 6,
            _ => 0,
        };

        total_score += match (opponent, player) {
            ("A", "Y") | ("B", "X") | ("C", "Z") => 1,
            ("A", "Z") | ("B", "Y") | ("C", "X") => 2,
            ("A", "X") | ("B", "Z") | ("C", "Y") => 3,
            _ => 0,
        };
    }

    println!("Total score: {}", total_score)
}
