import { print } from standard:debug;

print('hi from a');

export let a = 5;

export function setA(v) {
  print('setA', v);
  a = v;
}
