import { print } from standard:debug;

const p = new Promise(function(resolve, reject) {
  print('in promise');
  resolve(5);
});

print('owo 1');

p.then(function(v) {
  print('resolution', v);
  throw 10;
}).catch(function(v) {
  print('rejection', v);
});

print('owo 2');
