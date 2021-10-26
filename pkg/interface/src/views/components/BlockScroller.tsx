import { GraphNode } from '@urbit/api';
import bigInt, { BigInteger } from 'big-integer';
import React from 'react';
import VirtualScroller, { VirtualScrollerProps } from './VirtualScroller';

type BlockScrollerProps = Omit<
  VirtualScrollerProps<BigInteger, [BigInteger, GraphNode][]>,
  'keyEq' | 'keyToString' | 'keyBunt'
>;

const keyEq = (a: BigInteger, b: BigInteger) => a.eq(b);
const keyToString = (a: BigInteger) => a.toString();

export const BlockScroller = React.forwardRef<
  VirtualScroller<BigInteger, [BigInteger, GraphNode][]>,
  BlockScrollerProps
>((props, ref) => {
  return (
    <VirtualScroller<BigInteger, [BigInteger, GraphNode][]>
      ref={ref}
      {...props}
      keyEq={keyEq}
      keyToString={keyToString}
      keyBunt={bigInt.zero}
    />
  );
});
