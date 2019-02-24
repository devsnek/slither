import { print } from standard:debug;

const p = new Promise((resolve, reject) => {
  resolve(5);
});

p.finally((v) => {
  print('finally!', v);
});
