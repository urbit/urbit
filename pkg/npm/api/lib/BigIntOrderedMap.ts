import { BigInteger } from "big-integer";
import { immerable } from 'immer';

interface NonemptyNode<V> {
  n: [BigInteger, V];
  l: MapNode<V>;
  r: MapNode<V>;
}

type MapNode<V> = NonemptyNode<V> | null;

/**
 *  An implementation of ordered maps for JS
 *  Plagiarised wholesale from sys/zuse
 */
export default class BigIntOrderedMap<V> implements Iterable<[BigInteger, V]> {
  private root: MapNode<V> = null;
  [immerable] = true;
  size: number = 0;

  constructor(initial: [BigInteger, V][] = []) {
    initial.forEach(([key, val]) => {
      this.set(key, val);
    });
  }

  /**
   *  Retrieve an value for a key
   */
  get(key: BigInteger): V | null {
    const inner = (node: MapNode<V>): V | null => {
      if (!node) {
        return null;
      }
      const [k, v] = node.n;
      if (key.eq(k)) {
        return v;
      }
      if (key.gt(k)) {
        return inner(node.l);
      } else {
        return inner(node.r);
      }
    };

    return inner(this.root);
  }

  /**
   *  Put an item by a key
   */
  set(key: BigInteger, value: V): void {

    const inner = (node: MapNode<V>): MapNode<V> => {
      if (!node) {
        return {
          n: [key, value],
          l: null,
          r: null,
        };
      }
      const [k] = node.n;
      if (key.eq(k)) {
        this.size--;
        return {
          ...node,
          n: [k, value],
        };
      }
      if (key.gt(k)) {
        const l = inner(node.l);
        if (!l) {
          throw new Error("invariant violation");
        }
        return {
          ...node,
          l,
        };
      }
      const r = inner(node.r);
      if (!r) {
        throw new Error("invariant violation");
      }

      return { ...node, r };
    };
    this.size++;
    this.root = inner(this.root);
  }

  /**
   *  Remove all entries
   */
  clear() {
    this.root = null;
  }

  /**
   *  Predicate testing if map contains key
   */
  has(key: BigInteger): boolean {
    const inner = (node: MapNode<V>): boolean => {
      if (!node) {
        return false;
      }
      const [k] = node.n;

      if (k.eq(key)) {
        return true;
      }
      if (key.gt(k)) {
        return inner(node.l);
      }
      return inner(node.r);
    };
    return inner(this.root);
  }

  /**
   *  Remove value associated with key, returning whether that key
   *  existed in the first place
   */
  delete(key: BigInteger) {
    const inner = (node: MapNode<V>): [boolean, MapNode<V>] => {
      if (!node) {
        return [false, null];
      }
      const [k] = node.n;
      if (k.eq(key)) {
        return [true, this.nip(node)];
      }
      if (key.gt(k)) {
        const [bool, l] = inner(node.l);
        return [
          bool,
          {
            ...node,
            l,
          },
        ];
      }

      const [bool, r] = inner(node.r);
      return [
        bool,
        {
          ...node,
          r,
        },
      ];
    };
    const [ret, newRoot] = inner(this.root);
    if(ret) {
      this.size--;
    }
    this.root = newRoot;
    return ret;
  }

  private nip(nod: NonemptyNode<V>): MapNode<V> {
    const inner = (node: NonemptyNode<V>): MapNode<V> => {
      if (!node.l) {
        return node.r;
      }
      if (!node.r) {
        return node.l;
      }
      return {
        ...node.l,
        r: inner(node.r),
      };
    };
    return inner(nod);
  }

  peekLargest(): [BigInteger, V] | undefined {
    const inner = (node: MapNode<V>): [BigInteger, V] | undefined => {
      if(!node) {
        return undefined;
      }
      if(node.l) {
        return inner(node.l);
      }
      return node.n;
    }
    return inner(this.root);
  }

  peekSmallest(): [BigInteger, V] | undefined {
    const inner = (node: MapNode<V>): [BigInteger, V] | undefined => {
      if(!node) {
        return undefined;
      }
      if(node.r) {
        return inner(node.r);
      }
      return node.n;
    }
    return inner(this.root);
  }

  keys(): BigInteger[] {
    const list = Array.from(this);
    return list.map(([key]) => key);
  }

  forEach(f: (value: V, key: BigInteger) => void) {
    const list = Array.from(this);
    return list.forEach(([k,v]) => f(v,k));
  }

  [Symbol.iterator](): IterableIterator<[BigInteger, V]> {
    let result: [BigInteger, V][] = [];
    const inner = (node: MapNode<V>) => {
      if (!node) {
        return;
      }
      inner(node.l);
      result.push(node.n);
      inner(node.r);
    };
    inner(this.root);

    let idx = 0;
    return {
      [Symbol.iterator]: this[Symbol.iterator],
      next: (): IteratorResult<[BigInteger, V]> => {
        if (idx < result.length) {
          return { value: result[idx++], done: false };
        }
        return { done: true, value: null };
      },
    };
  }
}
