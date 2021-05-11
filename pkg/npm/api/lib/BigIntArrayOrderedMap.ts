import produce, { immerable, castImmutable, castDraft, setAutoFreeze, enablePatches } from 'immer';
import bigInt, { BigInteger } from "big-integer";

setAutoFreeze(false);

enablePatches();

function sortBigInt(a: BigInteger[], b: BigInteger[]) {
  if (a.lt(b)) {
    return 1;
  } else if (a.eq(b)) {
    return 0;
  } else {
    return -1;
  }
}

function stringToBigIntArr(str: string) {
  return str.split('/').slice(1).map((ind) => {
    return bigInt(ind);
  });
}

function arrToString(arr: BigInteger[]) {
  let string = '';
  arr.forEach((key) => {
    string = string + `/${key.toString()}`;
  });
}

function sortBigIntArr(a: BigInteger[], b: BigInteger[]) {
  let aLen = a.length;
  let bLen = b.length;

  let i = 0;
  while (i < aLen && i < bLen) {
    if (a[i].lt(b[i])) {
      return 1;
    } else if (a[i].gt(b[i])) {
      return -1;
    } else {
      i++;
    }
  }
  
  return aLen - bLen;
}


export default class BigIntArrayOrderedMap<V> implements Iterable<[BigInteger[], V]> {
  root: Record<string, V> = {}
  cachedIter: [BigInteger[], V][] = null;
  [immerable] = true;

  constructor(items: [BigInteger[], V][] = []) {
    items.forEach(([key, val]) => {
      this.set(key, val);
    });
  }

  get size() {
    return Object.keys(this.root).length;
  }


  get(key: BigInteger[]) {
    return this.root[arrToString(key)] ?? null;
  }

  gas(items: [BigInteger[], V][]) {
    return produce(this, draft => {
      items.forEach(([key, value]) => {
        draft.root[arrToString(key)] = castDraft(value);
      });
      draft.generateCachedIter();
    }, 
    (patches) => {
      //console.log(`gassed with ${JSON.stringify(patches, null, 2)}`);
    });
  }

  set(key: BigInteger[], value: V) {
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

  has(key: BigInteger[]) {
    return arrToString(key) in this.root;
  }

  delete(key: BigInteger[]) {
    const result = produce(this, draft => {
      delete draft.root[arrToString(key)];
      draft.cachedIter = null;
    });
    return result;
  }

  [Symbol.iterator](): IterableIterator<[BigInteger[], V]> {
    let idx = 0;
    let result = this.generateCachedIter();
    return {
      [Symbol.iterator]: this[Symbol.iterator],
      next: (): IteratorResult<[BigInteger[], V]> => {
        if (idx < result.length) {
          return { value: result[idx++], done: false };
        }
        return { done: true, value: null };
      },
    };
  }

  peekLargest() {
    const sorted = Array.from(this);
    return sorted[0] as [BigInteger[], V] | null;
  }

  peekSmallest() {
    const sorted = Array.from(this);
    return sorted[sorted.length - 1] as [BigInteger[], V] | null;
  }

  keys() {
    return Array.from(this).map(([k,v]) => k);
  }

  generateCachedIter() {
    if(this.cachedIter) {
      return [...this.cachedIter];
    }
    const result = Object.keys(this.root).map(key => {
      const num = stringtoBigIntArr(key);
      return [num, this.root[key]] as [BigInteger[], V];
    }).sort(([a], [b]) => sortBigIntArr(a,b));
    this.cachedIter = result;
    return [...result];
  }
}

