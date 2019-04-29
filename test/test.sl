import { print } from standard:debug;
import { listen, connect } from standard:net;

async function handle(conn) {
  print('owo got connection');
  for await chunk in conn {
    conn.write(chunk);
  }
}

(async () => {
  /*
  const server = listen('0.0.0.0:8080');
  print('listening', server);
  for await conn in server {
    print("owo conn");
    // handle(conn).catch(print);
  }
  print('server deded');
  */

  const c = connect('0.0.0.0:8080');
  print('got c');
  const v = c.next();
  v.catch((e) => { print('e is', e); });
  print('v is', v);
  const t = await v;
  print('t is', t);
})().catch((e) => {
  print('error', e);
});
