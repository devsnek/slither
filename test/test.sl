import { print } from standard:debug;

async function test() {
  const y = await Promise.resolve(5);
  print('y is', y);
  return y + 5;
}

print(test);

test().then(print, print);
