use std::collections::HashMap;
use std::hash::Hash;
use std::fs;

pub fn get_input(year: &str, day: &str) -> String {
    let file: String = format!("../../adventofcode-inputs/{}/day-{}.txt", year, day);
    return fs::read_to_string(file).expect("Failed to read input file");
}

// Clojure-like frequencies
pub fn frequencies<T: Hash + PartialEq + Eq>(iter: impl Iterator<Item = T>) -> HashMap<T, usize>{
    let h = HashMap::new();
    iter.fold(h, |mut acc: HashMap<T, usize>, x| {
        *acc.entry(x).or_insert(0) += 1;
        acc
    })
}
