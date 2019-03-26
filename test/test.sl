import { print } from standard:debug;

function add(a, b = 1) {
  return a + b;
}

print(add(2));
