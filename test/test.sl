import { print } from standard:debug;
import { readFile } from standard:fs;

readFile('./test/exports.sl').then(print);
Promise.resolve('before').then(print);
