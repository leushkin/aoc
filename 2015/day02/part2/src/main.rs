use std::env;
use std::fs;

fn calculate_size((l, w, h): (i32, i32, i32)) -> i32 {
  [2 * l + 2 * w, 2 * l + 2 * h, 2 * w + 2 * h]
    .iter()
    .min()
    .unwrap()
    + l * w * h
}

fn solution(input: String) -> i32 {
  return input
    .split('\n')
    .into_iter()
    .map(|x| x.split('x').collect())
    .map(|x: Vec<&str>| {
      let mut numbers = x.into_iter().map(|y| y.parse::<i32>().expect("an i32"));

      if numbers.len() != 3 {
        panic!("expected format <i32>x<i32>x<i32>")
      }

      (
        numbers.next().unwrap(),
        numbers.next().unwrap(),
        numbers.next().unwrap(),
      )
    })
    .map(calculate_size)
    .fold(0, |acc, x| acc + x);
}

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() != 2 {
    panic!("Usage: ./solution <input_file>");
  }

  let filename = args.get(1).expect("Cannot get filename");
  let data = fs::read_to_string(filename).expect(&format!("Cannot open file: {}", &filename));

  println!("{:?}", solution(data));
  println!("{:?}", calculate_size((2, 3, 4)));
}
