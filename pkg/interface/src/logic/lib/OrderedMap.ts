
export class OrderedMap<V> extends Map<number, V>
  implements Iterable<[number, V]> {
  [Symbol.iterator](): IterableIterator<[number, V]> {
    const sorted = Array.from(super[Symbol.iterator]()).sort(
      ([a], [b]) => b - a
    );
    let index = 0;
    return {
      [Symbol.iterator]: this[Symbol.iterator],
      next: (): IteratorResult<[number, V]> => {
        if (index < sorted.length) {
          return { value: sorted[index++], done: false };
        } else {
          return { done: true, value: null };
        }
      }
    };
  }
}
