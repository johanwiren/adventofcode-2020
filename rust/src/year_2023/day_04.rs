use std::collections::{HashSet, LinkedList};
use std::time::Instant;

use crate::utils;

fn parse_line(line: &str) -> usize {
    let parts: Vec<&str> = line.split('|').collect();
    let numbers: HashSet<usize> = parts[0]
        .split(|c: char| !c.is_ascii_digit())
        .filter_map(|num| num.parse().ok())
        .skip(1)
        .collect();

    let winning: HashSet<usize> = parts[1]
        .split(|c: char| !c.is_ascii_digit())
        .filter_map(|num| num.parse().ok())
        .collect();

    let count: usize = numbers.intersection(&winning).count();

    count
}

fn parse_input(input: &str) -> Vec<usize> {
    input.lines().map(|line| parse_line(line)).collect()
}

fn part_2_solver(input: &str) -> usize {
    let mut parsed_input = parse_input(input);
    parsed_input.reverse();
    let mut accumulator = LinkedList::new();
    let mut sum = 0;

    for &score in parsed_input.iter() {
        let cards = accumulator.iter().take(score).sum::<usize>() + 1;
        accumulator.push_front(cards);
        sum += cards;
    }

    sum
}

pub fn run() {
    let start_time = Instant::now();
    let input = utils::get_input("2023", "04");
    let result = part_2_solver(&input);

    let elapsed_time = start_time.elapsed();

    println!("{}", result);
    println!("Elapsed time: {:.2?}", elapsed_time);
}
