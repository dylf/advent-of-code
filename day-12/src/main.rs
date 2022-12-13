use std::{
    collections::{HashMap, HashSet, VecDeque},
    env,
    fs::File,
    io::Read,
};

type Point = (usize, usize);

type HeightMap = Vec<Vec<char>>;

fn find_point(height_map: &HeightMap, target: char) -> Vec<Point> {
    let found_points = height_map
        .iter()
        .enumerate()
        .flat_map(|(i, row)| {
            let pos = row.iter().position(|&c| c == target);
            match pos {
                Some(pos) => Some((i as usize, pos as usize)),
                _ => None,
            }
        })
        .collect::<Vec<_>>();

    found_points
}

fn can_travel(cur_char: char, target_char: char) -> bool {
    let mut cur = cur_char as i32;
    let mut target = target_char as i32;
    if cur_char == 'S' {
        cur = 'a' as i32;
    }
    if target_char == 'E' {
        target = 'z' as i32;
    }
    let diff: i32 = target as i32 - cur as i32;
    diff <= 1
}

fn next_spots(height_map: &HeightMap, curr_point: &Point) -> Vec<Point> {
    let (x, y) = *curr_point;
    let mut adjacent_spots: Vec<Point> = vec![];
    if x != 0 {
        if can_travel(height_map[x][y], height_map[x - 1][y]) {
            adjacent_spots.push((x - 1, y));
        }
    }

    if x < height_map.len() - 1 {
        if can_travel(height_map[x][y], height_map[x + 1][y]) {
            adjacent_spots.push((x + 1, y));
        }
    }

    if y != 0 {
        if can_travel(height_map[x][y], height_map[x][y - 1]) {
            adjacent_spots.push((x, y - 1));
        }
    }

    if y < height_map[x].len() - 1 {
        if can_travel(height_map[x][y], height_map[x][y + 1]) {
            adjacent_spots.push((x, y + 1));
        }
    }
    adjacent_spots
}

fn get_path_length(prev: &HashMap<Point, Option<Point>>, end: Point) -> u32 {
    let mut curr_pos = Some(end);
    let mut path: Vec<Point> = vec![end];

    loop {
        curr_pos = *prev
            .get(&curr_pos.unwrap())
            .expect("All points should be in prev");

        match curr_pos {
            Some(point) => path.push(point),
            _ => break,
        }
    }

    // Account for start point in path.
    path.len() as u32 - 1
}

fn get_shortest_path_from_point(height_map: &HeightMap, start: Point, end: Point) -> Option<u32> {
    let mut prev: HashMap<Point, Option<Point>> = HashMap::new();
    let mut queue: VecDeque<Point> = VecDeque::new();
    let mut visited: HashSet<Point> = HashSet::new();

    let mut curr_pos = start;
    prev.insert(curr_pos, None);
    queue.push_back(curr_pos);
    visited.insert(curr_pos);

    while !queue.is_empty() {
        curr_pos = queue.pop_front().expect("The queue should not be empty");
        if curr_pos == end {
            return Some(get_path_length(&prev, end));
        }

        for next in next_spots(&height_map, &curr_pos) {
            if visited.insert(next) {
                prev.insert(next, Some(curr_pos));
                queue.push_back(next);
            }
        }
    }
    None
}

fn get_shortest_path(height_map: &HeightMap, starting_char: char) -> u32 {
    let end = find_point(&height_map, 'E')[0];
    let mut paths: Vec<u32> = vec![];

    let start_points = find_point(&height_map, starting_char);
    for point in start_points {
        let path_length = get_shortest_path_from_point(&height_map, point, end);
        if let Some(length) = path_length {
            paths.push(length);
        }
    }

    *paths.iter().min().unwrap()
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let height_map: Vec<Vec<char>> = file_contents.lines().map(|l| l.chars().collect()).collect();

    println!(
        "Shortest path is: {:?}",
        get_shortest_path(&height_map, 'S')
    );
    println!(
        "Shortest path is: {:?}",
        get_shortest_path(&height_map, 'a')
    )
}
