import produce, { immerable, castDraft, setAutoFreeze, enablePatches } from 'immer';
import bigInt from 'big-integer';
setAutoFreeze(false);
enablePatches();
function sortBigInt(a, b) {
    if (a.lt(b)) {
        return 1;
    }
    else if (a.eq(b)) {
        return 0;
    }
    else {
        return -1;
    }
}
export class BigIntOrderedMap {
    root = {};
    cachedIter = null;
    [immerable] = true;
    constructor(items = []) {
        items.forEach(([key, val]) => {
            this.set(key, val);
        });
    }
    get size() {
        if (this.cachedIter) {
            return this.cachedIter.length;
        }
        return this.generateCachedIter().length;
    }
    get(key) {
        return this.root[key.toString()] ?? null;
    }
    gas(items) {
        return produce(this, (draft) => {
            items.forEach(([key, value]) => {
                draft.root[key.toString()] = castDraft(value);
            });
            draft.cachedIter = null;
        }, (patches) => {
            // console.log(`gassed with ${JSON.stringify(patches, null, 2)}`);
        });
    }
    set(key, value) {
        return produce(this, (draft) => {
            draft.root[key.toString()] = castDraft(value);
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
        return key.toString() in this.root;
    }
    delete(key) {
        const result = produce(this, (draft) => {
            delete draft.root[key.toString()];
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
            const num = bigInt(key);
            return [num, this.root[key]];
        }).sort(([a], [b]) => sortBigInt(a, b));
        this.cachedIter = result;
        return [...result];
    }
}
//# sourceMappingURL=BigIntOrderedMap.js.map