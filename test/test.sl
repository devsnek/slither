import { print } from standard:debug;

function x(v) {
  return (func) => {
    return () => {
      return v + func();
    };
  };
}

@x('1')
@x('2')
async function owo() {
  return 'owo';
}

print(owo());
