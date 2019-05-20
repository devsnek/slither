# slither

A modern scripting runtime

```js
function fib(n, a = 0, b = 1) { // argument initializers
  if n == 0 { // no parenthesis around if or try
    return a;
  }

  if n == 1 {
    return // unambiguous grammar means
      b;   // that this returns b, not null
  }

  // tail recursion
  return fib(n - 1, b, a + b);
}

print(fib(10) == 55);
```

Goals in no particular order
- staged JIT for good performance
- fast and easy networking
- good ffi interface
