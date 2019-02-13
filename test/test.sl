import { Promise } from standard:async;
import { print } from standard:debug;

/*
let p = new Promise(function(resolve, reject) {
  print('inside promise!');
  resolve(5);
});

print(p);
*/

function X() {
  this.x = 5;
}

X.prototype = {
  meme() {
    return this.x;
  },
};

let x = new X();
print(x.meme());
