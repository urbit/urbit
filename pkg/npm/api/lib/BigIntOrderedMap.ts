import produce, { immerable, castImmutable, castDraft, setAutoFreeze, enablePatches } from 'immer';
import bigInt, { BigInteger } from "big-integer";

setAutoFreeze(false);

enablePatches();

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
  root: Record<string, V> = {}
  cachedIter: [BigInteger, V][] = null;
  [immerable] = true;

  constructor(items: [BigInteger, V][] = []) {
    items.forEach(([key, val]) => {
      this.set(key, val);
    });
  }

  get size() {
    return Object.keys(this.root).length;
  }


  get(key: BigInteger) {
    return this.root[key.toString()] ?? null;
  }

  gas(items: [BigInteger, V][]) {
    return produce(this, draft => {
      items.forEach(([key, value]) => {
        draft.root[key.toString()] = castDraft(value);
      });
      draft.generateCachedIter();
    }, 
    (patches) => {
      //console.log(`gassed with ${JSON.stringify(patches, null, 2)}`);
    });
  }

  set(key: BigInteger, value: V) {
    return produce(this, draft => {
      draft.root[key.toString()] = castDraft(value);
      draft.cachedIter = null;
    });
  }

  clear() {
    return produce(this, draft => {
      draft.cachedIter = [];
      draft.root = {}
    });
  }

  has(key: BigInteger) {
    return key.toString() in this.root;
  }

  delete(key: BigInteger) {
    const result = produce(this, draft => {
      delete draft.root[key.toString()];
      draft.cachedIter = null;
    });
    return result;
  }

  [Symbol.iterator](): IterableIterator<[BigInteger, V]> {
    let idx = 0;
    let result = this.generateCachedIter();
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
    return Array.from(this).map(([k,v]) => k);
  }

  generateCachedIter() {
    if(this.cachedIter) {
      return [...this.cachedIter];
    }
    const result = Object.keys(this.root).map(key => {
      const num = bigInt(key);
      return [num, this.root[key]] as [BigInteger, V];
    }).sort(([a], [b]) => sortBigInt(a,b));
    this.cachedIter = result;
    return [...result];
  }
}

