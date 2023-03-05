
# This is a comment.

fn __print_bool(x: bool) {}
fn __print_int(x: int) {}
fn __print_float(x: float) {}
fn __print_string(x: string) {}
fn __print_test(x: Test) {}

struct Test {
  foo: bool;
  bar: int;
  baz: float;
  bazz: string;
  # t2: Test2;
}

# struct Test2 {
#   t1: Test;
# }

fn foo() -> Test {
  return Test {
    bazz: "test",
    bar: 0,
    foo: false,
    baz: 0.1,
  };
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
  let test = foo();
  __print_test(test);
  # __print_bool(test.foo);
  # __print_int(test.bar + 1);
  # __print_float(test.baz);
  # __print_string(test.bazz);
  test.foo = true;
  __print_test(test);

  let test2: Test;
  test2.foo = true;
  (test2.foo) = test2.foo;
  test2.bar = 404;
  # test2.baz = 4.4;
  test2.bazz = "Hi, this is a test";

  __print_test(test2);

  let fib2 = fib;
  let value = 0;

  loop {
    if value == 10 {
      break;
    }
    value = value + 1;
    __print_int(value);
    __print_int(fib2(10) + fib(10));
  }

  let value = 0;

  while value != 10 {
    value += 1;
    __print_int(value);
    __print_int(fib2(10) + fib(10));
  }

  __print_int(1 << 2);
}

# fn main() {
#   __print_int(+fib(10));
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
