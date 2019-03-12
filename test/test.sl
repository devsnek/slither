import { print } from standard:debug;
import { connect } from standard:net;

const sock = connect('127.0.0.1:8080');

(async () => {
  for await data in sock {
    print('data', data, data[0], data['0']);
  }
})().catch(print);
