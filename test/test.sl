import { print } from standard:debug;

class X {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  z() {
    return this.x + this.y;
  }
}

const x = new X(1, 2);
print(x.z());
