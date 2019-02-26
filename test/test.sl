import { print } from standard:debug;
import { createTimeout } from standard:timers;
import { a, setA } from './exports.sl';

print('a is', a);
setA(10);

createTimeout(() => {
  print('in timeout 150', a);
}, 150);

createTimeout(() => {
  print('in timeout');
}, 100);

createTimeout(() => {
  print('in timeout 2');
}, 100);

Promise.resolve('promise').then(print);

print('after');
