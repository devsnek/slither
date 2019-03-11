import { print } from standard:debug;
import { connect } from standard:net;
import { a, setA } from './exports';
import { b } from '.';

setA(10);

print(a, b);
