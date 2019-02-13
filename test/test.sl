import { print } from standard:debug;

const p = new Promise(function(resolve, reject) {
  print("in promise");
  resolve(5);
});

print("owo");
