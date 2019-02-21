import { print } from standard:debug;
// import { a, setA } from './a.sl';

const o = {};

o.a = () => {
  return this;
};

o.b = function() {
  return this;
};

print(o.a());
