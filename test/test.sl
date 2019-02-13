import { print } from standard:debug;

const s = Symbol();

const o = {};

o[s] = 5;

print(o[s]);
