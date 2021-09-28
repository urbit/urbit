import { BigInteger } from 'big-integer';

export function max(a: BigInteger, b: BigInteger) {
  return a.gt(b) ? a : b;
}

export function min(a: BigInteger, b: BigInteger) {
  return a.lt(b) ? a : b;
}

