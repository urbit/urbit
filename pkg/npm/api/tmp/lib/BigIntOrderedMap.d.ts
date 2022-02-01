import { immerable } from 'immer';
import bigInt, { BigInteger } from 'big-integer';
export declare class BigIntOrderedMap<V> implements Iterable<[BigInteger, V]> {
    root: Record<string, V>;
    cachedIter: [BigInteger, V][] | null;
    [immerable]: boolean;
    constructor(items?: [BigInteger, V][]);
    get size(): number;
    get(key: BigInteger): V;
    gas(items: [BigInteger, V][]): this;
    set(key: BigInteger, value: V): this;
    clear(): this;
    has(key: BigInteger): boolean;
    delete(key: BigInteger): this;
    [Symbol.iterator](): IterableIterator<[BigInteger, V]>;
    peekLargest(): [bigInt.BigInteger, V];
    peekSmallest(): [bigInt.BigInteger, V];
    keys(): bigInt.BigInteger[];
    generateCachedIter(): [bigInt.BigInteger, V][];
}
