import { print } from standard:debug;
import { PI, min, max } from standard:math;

function add(a, b = 1) {
  return a + b;
}

print(PI);
print(min(1, 2, 3));
print(max(1, 2, 3));
