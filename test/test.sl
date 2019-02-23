import { print } from standard:debug;

let i = 1;
while true {
  i += 1;
  print(i);
  if i > 5 {
    break;
  }
}
