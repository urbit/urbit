import produce, { immerable, castDraft, setAutoFreeze, enablePatches } from 'immer';
import bigInt from 'big-integer';
setAutoFreeze(false);
enablePatches();
export function stringToArr(str) {
    return str.split('/').slice(1).map((ind) => {
        return bigInt(ind);
    });
}
export function arrToString(arr) {
    let string = '';
    arr.forEach((key) => {
        string = string + `/${key.toString()}`;
    });
    return string;
}
function sorted(a, b, reversed = false) {
    const getSort = sortBigIntArr(a, b);
    if (reversed) {
        return getSort * -1;
    }
    else {
        return getSort;
    }
}
export function sortBigIntArr(a, b) {
    const aLen = a.length;
    const bLen = b.length;
    const aCop = a.slice(0);
    const bCop = b.slice(0);
    aCop.reverse();
    bCop.reverse();
    let i = 0;
    while (i < aLen && i < bLen) {
        if (aCop[i].lt(bCop[i])) {
            return 1;
        }
        else if (aCop[i].gt(bCop[i])) {
            return -1;
        }
        else {
            i++;
        }
    }
    return bLen - aLen;
}
export class BigIntArrayOrderedMap {
    root = {};
    cachedIter = null;
    [immerable] = true;
    reversed = false;
    constructor(items = [], reversed = false) {
        items.forEach(([key, val]) => {
            this.set(key, val);
        });
        this.reversed = reversed;
    }
    get size() {
        return Object.keys(this.root).length;
    }
    get(key) {
        return this.root[arrToString(key)] ?? null;
    }
    gas(items) {
        return produce(this, (draft) => {
            items.forEach(([key, value]) => {
                draft.root[arrToString(key)] = castDraft(value);
            });
            draft.generateCachedIter();
        }, (patches) => {
            // console.log(`gassed with ${JSON.stringify(patches, null, 2)}`);
        });
    }
    set(key, value) {
        return produce(this, (draft) => {
            draft.root[arrToString(key)] = castDraft(value);
            draft.cachedIter = null;
        });
    }
    clear() {
        return produce(this, (draft) => {
            draft.cachedIter = [];
            draft.root = {};
        });
    }
    has(key) {
        return arrToString(key) in this.root;
    }
    delete(key) {
        const result = produce(this, (draft) => {
            delete draft.root[arrToString(key)];
            draft.cachedIter = null;
        });
        return result;
    }
    [Symbol.iterator]() {
        let idx = 0;
        const result = this.generateCachedIter();
        return {
            [Symbol.iterator]: this[Symbol.iterator],
            next: () => {
                if (idx < result.length) {
                    return { value: result[idx++], done: false };
                }
                return { done: true, value: null };
            }
        };
    }
    peekLargest() {
        const sorted = Array.from(this);
        return sorted[0];
    }
    peekSmallest() {
        const sorted = Array.from(this);
        return sorted[sorted.length - 1];
    }
    keys() {
        return Array.from(this).map(([k, v]) => k);
    }
    generateCachedIter() {
        if (this.cachedIter) {
            return [...this.cachedIter];
        }
        const result = Object.keys(this.root).map((key) => {
            return [stringToArr(key), this.root[key]];
        }).sort(([a], [b]) => sorted(a, b, this.reversed));
        this.cachedIter = result;
        return [...result];
    }
}
//# sourceMappingURL=BigIntArrayOrderedMap.js.map