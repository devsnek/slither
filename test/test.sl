import { print } from standard:debug;
import { createTimeout } from standard:timers;

print('before');

createTimeout(() => {
  print('in timeout 150');
}, 150);

createTimeout(() => {
  print('in timeout');
}, 100);

createTimeout(() => {
  print('in timeout 2');
}, 100);

Promise.resolve('promise').then(print);

print('after');
