import { print } from standard:debug;

const iterable = {
  [:iterator]: () => {
    let i = 0;
    return {
      next() {
        i += 1;
        if i > 10 {
          return { done: true };
        }
        return { done: false, value: i };
      }
    };
  },
};

let i = 0;
for item in iterable {
  i += item;
}

print(i);
