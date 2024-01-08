use std::time::Instant;
use std::str::Chars;

use crate::utils;

#[derive(Debug)]
struct Hand<'a> {
    cards: Chars<'a>,
    bid: usize,
}

const CARD_SCORE: [char;13] = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'];
const HANDS_BY_SCORE: [&'static str;7] = ["11111", "2111", "221", "311", "32", "41", "5"];

fn hand_score(hand: &Hand) -> String {
    let mut card_score = utils::frequencies(hand.cards.clone().into_iter())
        .into_values()
        .into_iter()
        .collect::<Vec<_>>();
    card_score.sort_by(|a, b| b.cmp(a));
    let card_score_str = String::from_iter(card_score.iter().map(|x| format!("{}", x)));
    let card_score = HANDS_BY_SCORE.iter().position(|x| *x == card_score_str);
    format!("{:02?}", card_score.unwrap() + 1)
}

fn card_score(hand: &Hand) -> String {
    let s = hand.cards.clone().into_iter()
        .map(|c| CARD_SCORE.iter().position(|x| *x == c))
        .map(|x| format!("{:02?}", x.unwrap()))
        .collect::<String>();
    s
}

fn part_1_solver(input: &String) -> usize {
    let mut hands: Vec<Hand> = input.lines()
        .map(|l| l.split_whitespace().collect::<Vec<_>>())
        .map(|s| Hand {cards: s[0].chars(), bid: s[1].parse().unwrap()})
        .collect();
    hands.sort_by(|a, b| format!("{}{}", hand_score(a), card_score(a))
                  .cmp(&format!("{}{}", hand_score(b), card_score(b))));
    let res = hands.iter()
        .enumerate()
        .fold(0, |acc, x| {
            acc + ((x.0 + 1) * x.1.bid)
    });
    res
}

pub fn run() {
    let start_time = Instant::now();
    let input = utils::get_input("2023", "07");

    let result = part_1_solver(&input);

    let elapsed_time = start_time.elapsed();

    println!("{}", result);
    println!("Elapsed time: {:.2?}", elapsed_time);
}
