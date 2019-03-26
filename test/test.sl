import { print } from standard:debug;

class X {
  constructor() {
    print('constructor!');
    this.y = 5;
    const x = () => this;
    print(x());
  }
}

const x = new X();
print(x);
