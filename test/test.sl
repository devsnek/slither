import { print } from standard:debug;
import { readFile, writeFile, removeFile, getMetadata } from standard:fs;

const x = {
  [1]: 'hi',
  ['owo' + 'meme']: 'bye',
};

print(x);
