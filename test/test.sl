import { print } from standard:debug;

Promise.resolve(Promise.resolve(5))
  .then((v) => {
    return Promise.resolve(v + 5);
  }).then(print);

print('owo');
