import { print } from standard:debug;

gen function test() {
  try {
    yield;
  } catch e {
    print('e is', e);
  }
}

const it = test();
it.next();
it.throw('thrown thing');
