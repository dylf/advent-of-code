use std::{collections::HashSet, env, fs::File, io::Read};

fn get_marker_char(value: &str, size: usize) -> usize {
    let mut set = HashSet::new();

    let pos = value
        .chars()
        .collect::<Vec<char>>()
        .windows(size)
        .position(|chrs| {
            set.clear();
            chrs.into_iter().all(|chr| set.insert(chr))
        });

    size + pos.expect("AoC input was bad")
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let pkt_marker = get_marker_char(&file_contents, 4);
    println!("Packet marker after character {}", pkt_marker);

    let msg_marker = get_marker_char(&file_contents, 14);
    println!("Message marker after character {}", msg_marker);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_marker_packet() {
        assert_eq!(5, get_marker_char("bvwbjplbgvbhsrlpgdmjqwftvncz", 4));
        assert_eq!(6, get_marker_char("nppdvjthqldpwncqszvftbrmjlhg", 4));
        assert_eq!(10, get_marker_char("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4));
        assert_eq!(11, get_marker_char("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4));
    }

    #[test]
    fn test_marker_message() {
        assert_eq!(19, get_marker_char("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14));
        assert_eq!(23, get_marker_char("bvwbjplbgvbhsrlpgdmjqwftvncz", 14));
        assert_eq!(23, get_marker_char("nppdvjthqldpwncqszvftbrmjlhg", 14));
        assert_eq!(29, get_marker_char("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14));
        assert_eq!(26, get_marker_char("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14));
    }
}
