import { print } from standard:debug;
import { createInterval } from standard:timers;

(async () => {
  let i = 0;
  for await _ in createInterval(1000) {
    i += 1;
    print('hi!', i);
    if i > 5 {
      break;
    }
  }
  print('done!');
})();
