
# This is a comment.

struct Test {
  foo: bool;
  bar: int;
  baz: float;
  bazz: string;
}

fn print(x: int) {
  
}

# fn on_event(event: string, cb1: fn(), cb1: fn(string,) -> string) {
# 
# }

fn fib(n: int) -> int {
  if n <= 1 {
    return n;
  }
  return fib(n - 1) + fib(n - 2);
}

fn main() {
  let fib2 = fib;
  let value = 0;

  loop {
    if value == 10 {
      break;
    }
    value = value + 1;
    print(value);
    print(fib2(10) + fib(10));
  }

  let value = 0;

  while value != 10 {
    value += 1;
    print(value);
    print(fib2(10) + fib(10));
  }

  print(1 << 2);
}

# fn main() {
#   print(+fib(10));
# }
# 
# fn test(n: int) {
#   let x: int;
#   test(x * 1);
# }
# 
# fn not_returning() -> int {
#   if true {
#     return 10;
#   }
# }
# 
# fn returning() -> int {
#   if true {
#     return 1;
#   } else if true {
#     return 1;
#   } else {
#     return 2;
#   }
# 
#   fib(10);
# }
