use std::{cmp::max, env, fs::File, io::Read};

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut file = File::open(&args[1]).expect("Error opening file");
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents)
        .expect("Error reading line");

    let matrix = file_contents
        .lines()
        .into_iter()
        .map(|s| {
            s.chars()
                .map(|c| c.to_digit(10).expect("AoC input valid"))
                .collect::<Vec<u32>>()
        })
        .collect::<Vec<Vec<u32>>>();
    let visible_trees: u32 = matrix
        .iter()
        .enumerate()
        .map(|(row_i, row)| {
            let (rows_before, tail) = matrix.split_at(row_i);
            let rows_after = tail.iter().skip(1).collect::<Vec<&Vec<u32>>>();
            row.iter()
                .enumerate()
                .map(|(col_i, &number)| {
                    let (cols_before, tail) = row.split_at(col_i);
                    let cols_after = tail.iter().skip(1).collect::<Vec<&u32>>();
                    if rows_before.iter().all(|r| number > r[col_i])
                        || rows_after.iter().all(|&r| number > r[col_i])
                        || cols_before.iter().all(|&r| number > r)
                        || cols_after.iter().all(|&r| number > *r)
                    {
                        return 1;
                    }

                    0
                })
                .sum::<u32>()
        })
        .sum();

    let max_scenic_score = matrix
        .iter()
        .enumerate()
        .map(|(row_i, row)| {
            let (rows_before, tail) = matrix.split_at(row_i);
            let rows_after = tail.iter().skip(1).collect::<Vec<&Vec<u32>>>();

            row.iter()
                .enumerate()
                .map(|(col_i, &number)| {
                    let (cols_before, tail) = row.split_at(col_i);
                    let cols_after = tail.iter().skip(1).collect::<Vec<&u32>>();
                    let above = rows_before
                        .iter()
                        .rev()
                        .position(|r| number <= r[col_i])
                        .map_or_else(|| max(0, rows_before.len()), |x| x + 1);
                    let below = rows_after
                        .iter()
                        .position(|&r| number <= r[col_i])
                        .map_or_else(|| max(0, rows_after.len()), |x| x + 1);
                    let before = cols_before
                        .iter()
                        .rev()
                        .position(|&r| number <= r)
                        .map_or_else(|| max(0, cols_before.len()), |x| x + 1);
                    let after = cols_after
                        .iter()
                        .position(|&r| number <= *r)
                        .map_or_else(|| max(0, cols_after.len()), |x| x + 1);
                    above * below * before * after
                })
                .max()
                .expect("Value should exist")
        })
        .max()
        .expect("Value should exist");

    println!("Visible trees {:?}", visible_trees);
    println!("Max scenic score {:?}", max_scenic_score);
}
