import { print } from standard:debug;

function fib(n, a = 0, b = 1) {
  if n == 0 {
    return a;
  }

  if n == 1 {
    return
      b;
  }

  return fib(n - 1, b, a + b);
}

print(fib(10) == 55);
