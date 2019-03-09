import { print } from standard:debug;
import { listen } from standard:net;

const server = listen('0.0.0.0:8080');

(async () => {
  for await client in server {
    print('new connection', client);
    for await data in client {
      print('data', data);
    }
  }
})();
