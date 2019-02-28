import { print } from standard:debug;
import { readFile, writeFile, removeFile, getMetadata } from standard:fs;

let i = 0;
while true {
  i += 1;
  if i % 2 == 0 {
    print('even', i);
    continue;
  }
  print('odd', i);

  if i > 10 {
    break;
  }
}
