import { GraphNode } from '@urbit/api';
import bigInt, { BigInteger } from 'big-integer';
import React from 'react';
import VirtualScroller, { VirtualScrollerProps } from './VirtualScroller';

type GraphScrollerProps = Omit<
  VirtualScrollerProps<BigInteger, GraphNode>,
  'keyEq' | 'keyToString' | 'keyBunt'
>;

const keyEq = (a: BigInteger, b: BigInteger) => a.eq(b);
const keyToString = (a: BigInteger) => a.toString();

export const GraphScroller = React.forwardRef<
  VirtualScroller<BigInteger, GraphNode>,
  GraphScrollerProps
>((props, ref) => {
  return (
    <VirtualScroller
      ref={ref}
      {...props}
      keyEq={keyEq}
      keyToString={keyToString}
      keyBunt={bigInt.zero}
    />
  );
});
