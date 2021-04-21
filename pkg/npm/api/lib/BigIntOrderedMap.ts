import { immerable } from 'immer';
import bigInt, { BigInteger } from "big-integer";

function sortBigInt(a: BigInteger, b: BigInteger) {
  if (a.lt(b)) {
    return 1;
  } else if (a.eq(b)) {
    return 0;
  } else {
    return -1;
  }
}
export default class BigIntOrderedMap<V> implements Iterable<[BigInteger, V]> {
  private root: Record<string, V> = {}
  private cachedIter: [BigInteger, V][] | null = null;
  [immerable] = true;

  constructor(items: [BigInteger, V][] = []) {
    items.forEach(([key, val]) => {
      this.set(key, val);
    });
    this.generateCachedIter();
  }

  get size() {
    return this.cachedIter?.length ?? Object.keys(this.root).length;
  }


  get(key: BigInteger) {
    return this.root[key.toString()] ?? null;
  }

  set(key: BigInteger, value: V) {
    this.root[key.toString()] = value;
    this.cachedIter = null;
  }

  clear() {
    this.cachedIter = null;
    this.root = {}
  }

  has(key: BigInteger) {
    return key.toString() in this.root;
  }

  delete(key: BigInteger) {
    const had = this.has(key);
    if(had) {
      delete this.root[key.toString()];
      this.cachedIter = null;
    }
    return had;
  }

  [Symbol.iterator](): IterableIterator<[BigInteger, V]> {
    let idx = 0;
    const result = this.generateCachedIter();
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

  peekLargest() {
    const sorted = Array.from(this);
    return sorted[0] as [BigInteger, V] | null;
  }

  peekSmallest() {
    const sorted = Array.from(this);
    return sorted[sorted.length - 1] as [BigInteger, V] | null;
  }

  keys() {
    return Object.keys(this.root).map(k => bigInt(k)).sort(sortBigInt)
  }

  private generateCachedIter() {
    if(this.cachedIter) {
      return this.cachedIter;
    }
    const result = Object.keys(this.root).map(key => {
      const num = bigInt(key);
      return [num, this.root[key]] as [BigInteger, V];
    }).sort(([a], [b]) => sortBigInt(a,b));
    this.cachedIter = result;
    return result;
  }
}

