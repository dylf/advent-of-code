use std::{collections::HashSet, env, fs::File, io::Read};

fn move_head(pos: (i32, i32), dir: &str) -> (i32, i32) {
    let (x, y) = pos;
    match dir {
        "L" => (x - 1, y),
        "R" => (x + 1, y),
        "D" => (x, y - 1),
        "U" => (x, y + 1),
        _ => pos,
    }
}

fn move_tail(head: (i32, i32), tail: (i32, i32)) -> (i32, i32) {
    let (head_x, head_y) = head;
    let (tail_x, tail_y) = tail;
    let diff_x = head_x - tail_x;
    let diff_y = head_y - tail_y;
    if diff_x.abs() > 1 || diff_y.abs() > 1 {
        return (tail_x + diff_x.signum(), tail_y + diff_y.signum());
    }

    tail
}

fn count_moves(file_contents: &String, rope_length: usize) -> usize {
    let mut rope: Vec<(i32, i32)> = vec![(0, 0); rope_length];
    let mut visited = HashSet::new();
    visited.insert((0, 0));

    for line in file_contents.lines() {
        let motion = line.split_whitespace().collect::<Vec<&str>>();
        let dir = motion[0];
        let amt = motion[1].parse::<i32>().expect("AoC input good");
        for _n in 0..amt {
            rope = rope
                .iter()
                .enumerate()
                .map(|(i, &k)| {
                    if i == 0 {
                        return move_head(k, dir);
                    }
                    move_tail(rope[i - 1], k)
                })
                .collect::<Vec<(i32, i32)>>();
            visited.insert(*rope.last().expect("Aoc input good"));
        }
    }

    visited.insert(*rope.last().expect("Aoc input good"));
    visited.len()
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    println!("Tail 2 visited {:?}", count_moves(&file_contents, 2));
    println!("Tail 10 visited {:?}", count_moves(&file_contents, 10));
}
