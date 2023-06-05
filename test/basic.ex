
fn foo() -> int {
  return 1;
}

fn main() {
  let a = foo() + 25;
  let b = &a;
  let c = *b;
}
