import { print } from standard:debug;
import { listen, connect } from standard:net;

async function handle(socket) {
  for await message in socket {
    print('message in socket', message);
    socket.write(message);
  }
}

(async () => {
  const listener = listen('127.0.0.1:8080');
  for await socket in listener {
    handle(socket)
      .catch((e) => print('error', e));
  }
})().catch((e) => {
  print('error', e);
});
