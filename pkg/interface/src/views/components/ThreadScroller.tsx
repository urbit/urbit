import { BigInteger } from 'big-integer';
import React from 'react';
import VirtualScroller, { VirtualScrollerProps } from './VirtualScroller';

import { arrToString } from '@urbit/api/lib/BigIntArrayOrderedMap';
import { FlatGraphNode } from '@urbit/api';

type ThreadScrollerProps = Omit<
  VirtualScrollerProps<BigInteger[], FlatGraphNode>,
  'keyEq' | 'keyToString' | 'keyBunt'
>;

export function keyEq(a: BigInteger[], b: BigInteger[]) {
  const aLen = a.length;
  const bLen = b.length;

  if (aLen === bLen) {
    let i = 0;
    while (i < aLen && i < bLen) {
      if (a[i].eq(b[i])) {
        if (i === aLen - 1) {
          return true;
        }
        i++;
      } else {
        return false;
      }
    }
  }

  return false;
}
const keyBunt = [];

export const ThreadScroller = React.forwardRef<
  VirtualScroller<BigInteger[], FlatGraphNode>,
  ThreadScrollerProps
>((props: ThreadScrollerProps, ref) => {
  return (
    <VirtualScroller<BigInteger[], FlatGraphNode>
      ref={ref}
      {...props}
      keyEq={keyEq}
      keyToString={arrToString}
      keyBunt={keyBunt}
    />
  );
});
