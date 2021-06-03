import { useRef } from 'react';
import { Primitive } from '@urbit/api';

export default function usePreviousValue<T extends Primitive>(value: T): T {
  const prev = useRef<T | null>(null);
  const curr = useRef<T | null>(null);

  if (prev?.current !== curr?.current) {
    prev.current = curr?.current;
  }

  if (curr.current !== value) {
    curr.current = value;
  }

  return prev.current!;
}

