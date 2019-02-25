import { print } from standard:debug;
import { createTimeout } from standard:timers;

print('before');

createTimeout(() => {
  print('in timeout');
}, 100);

print('after');
