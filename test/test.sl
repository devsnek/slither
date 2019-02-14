import { print } from standard:debug;

const p0 = new Promise(function(resolve, reject) {
  print('in promise p0');
  resolve(5);
});

print('owo 1');

const p1 = p0.then(function(v) {
  print('in p1', v);
  throw 10;
});

print('owo 2');

const p2 = p1.catch(function(v) {
  print('in p2', v);
});

print('owo 3');
