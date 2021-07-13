export function newApi() {
  const target = () => {};

  const handler = {
    apply: function (target, that, args) {
      return Promise.resolve();
    },
    get: function (target, prop, receiver) {
      const original = target[prop];
      if (prop === 'toString') {
        return () => '[fakeApi]';
      } else if (typeof original === 'function') {
        return target[prop].bind(target);
      } else if (original) {
        return target[prop];
      }
      return newApi();
    }
  };

  return new Proxy(target, handler) as any;
}
