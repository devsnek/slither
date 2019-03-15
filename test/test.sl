import { print } from standard:debug;
import { connect } from standard:net;

const s = connect('127.0.0.1:8080');

(async () => {
  for await data in s {
    print('data', data);
    s.write(data);
  }
})().catch(print);
