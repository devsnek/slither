import { print } from standard:debug;
import { connect } from standard:net;

const sock = connect('127.0.0.1:8080');

(async () => {
  let i = 0;
  for await data in sock {
    i += 1;
    print('data', data);
    if i > 5 {
      sock.close();
    }
  }
})().catch(print);
