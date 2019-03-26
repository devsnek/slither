import { print } from standard:debug;

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

print(fib(22));
