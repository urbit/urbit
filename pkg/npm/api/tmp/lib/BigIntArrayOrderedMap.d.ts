import { immerable } from 'immer';
import bigInt, { BigInteger } from 'big-integer';
export declare function stringToArr(str: string): bigInt.BigInteger[];
export declare function arrToString(arr: BigInteger[]): string;
export declare function sortBigIntArr(a: BigInteger[], b: BigInteger[]): number;
export declare class BigIntArrayOrderedMap<V> implements Iterable<[BigInteger[], V]> {
    root: Record<string, V>;
    cachedIter: [BigInteger[], V][] | null;
    [immerable]: boolean;
    reversed: boolean;
    constructor(items?: [BigInteger[], V][], reversed?: boolean);
    get size(): number;
    get(key: BigInteger[]): V;
    gas(items: [BigInteger[], V][]): this;
    set(key: BigInteger[], value: V): this;
    clear(): this;
    has(key: BigInteger[]): boolean;
    delete(key: BigInteger[]): this;
    [Symbol.iterator](): IterableIterator<[BigInteger[], V]>;
    peekLargest(): [bigInt.BigInteger[], V];
    peekSmallest(): [bigInt.BigInteger[], V];
    keys(): bigInt.BigInteger[][];
    generateCachedIter(): [bigInt.BigInteger[], V][];
}
