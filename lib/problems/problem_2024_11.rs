use std::env;
use std::collections::HashMap;

enum BlinkTransform<T = u64> {
    One(T),
    Two(T, T)
}

fn parse_input(filename: &str) -> HashMap<u64, usize> {
    std::fs::read_to_string(filename)
        .expect("Could not read input file")
        .split(' ')
        .map(|v| v.trim().parse::<u64>().unwrap())
        .fold(HashMap::new(), |mut acc, v| {
            *acc.entry(v).or_insert(0) += 1;
            acc
        })
}

fn blink_transform(number: u64) -> BlinkTransform {
    let s = number.to_string();
    let len = s.len();
    if number == 0 {
        BlinkTransform::One(1)
    } else if len % 2 == 0 {
        let half_len = len / 2;
        let first_half = s[0..half_len].to_string();
        let second_half = s[half_len..].to_string();
        BlinkTransform::Two(first_half.parse::<u64>().unwrap(), second_half.parse::<u64>().unwrap())
    } else {
        BlinkTransform::One(number * 2024)
    }
}

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let mut counts = parse_input(&args[1]);

    for _ in 0..75 {
        let mut updated_counts = HashMap::new();

        for (number, count) in counts.iter_mut() {
            match blink_transform(*number) {
                BlinkTransform::One(a) => {
                    *updated_counts.entry(a).or_insert(0) += *count;
                }
                BlinkTransform::Two(a, b) => {
                    *updated_counts.entry(a).or_insert(0) += *count;
                    *updated_counts.entry(b).or_insert(0) += *count;
                }
            }
        }

        counts = updated_counts;
    }

    println!("{}", counts.values().sum::<usize>());
}