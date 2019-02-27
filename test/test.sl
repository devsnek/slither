import { print } from standard:debug;
import { readFile, writeFile, removeFile, getMetadata } from standard:fs;

writeFile('./test/out.txt', 'hello this is\n  a test')
  .then(() => {
    print('write finished!');
    return getMetadata('./test/out.txt');
  })
  .then((m) => {
    print('got metadata', m);
    return readFile('./test/out.txt');
  })
  .then((s) => {
    print('read finished', s);
    return removeFile('./test/out.txt');
  })
  .then(() => {
    print('remove finished');
  });
