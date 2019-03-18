import { print } from standard:debug;

async function var(a, ...args) {
  print(a, args);
}

var(1, 2);
