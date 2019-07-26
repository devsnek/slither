import { Promise } from standard:async;
import { createTimeout } from standard:timers;

function wait(ms) {
  return new Promise((resolve) => {
    createTimeout(resolve, ms);
  });
}

async function test(i) {
  await wait(1000);
  print('1000 ms later', i);
  await wait(500);
  print('500 ms later', i);
  return `${i} return`;
}

test(1).then(print, print);
test(2).then(print, print);
