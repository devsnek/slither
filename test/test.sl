import { print } from standard:debug;

const b = [1, 2, 3];

print(b[0]);

b.length = 0;

print(b[0]);
