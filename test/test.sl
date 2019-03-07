import { print } from standard:debug;

gen function test() {
  let i = 0;
  while i < 10 {
    yield i;
    i += 1;
  }
}

for i in test() {
  print(i);
}
