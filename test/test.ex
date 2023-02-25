
# This is a comment.

fn random(): int {

}

fn print(abc: int) {
  
}

fn main() {
  let a = 10;
  let b = 20;
  let c = random();
  print(a + b + c);
}

fn test(a: string, b: Type, c: int,): int { }

fn test2(a: int) {
  if a == 0 {
    return 0;
  } else if a == 1 {
    return 1;
  } else if a == 2 {
    return 2;
  } else {
    a = 10;
  }

  return test2(a - 1) + a as int;
}
