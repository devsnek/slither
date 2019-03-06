import { print } from standard:debug;
import { a, setA } from './exports.sl';

function fib(n, a = 0, b = 1) {
  if n == 0 {
    return a;
  }

  if n == 1 {
    return b;
  }

  return fib(n - 1, b, a + b);
}

print(fib(20));

print('a is', a);
setA(10);
print('a is', a);

Promise.resolve(5).then((v) => {
  print('promise', v);
});
