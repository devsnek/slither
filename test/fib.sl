function fibTail(n, a = 0, b = 1) {
  if n == 0 {
    return a;
  }
  if n == 1 {
    return b;
  }
  return fibTail(n - 1, b, a + b);
}

function fib(n) {
  if n == 0 {
    return 0;
  }
  if n == 1 {
    return 1;
  }
  return fib(n - 1) + fib(n - 2);
}

generator genfib() {
  let fn1 = 0;
  let fn2 = 1;
  while true {
    const current = fn1;
    fn1 = fn2;
    fn2 = current + fn1;
    let reset = yield current;
    if (reset) {
      fn1 = 0;
      fn2 = 1;
    }
  }
}

/*
for i in genfib() {
  print(i);
}
*/

print('fib 22 =', fibTail(50));
