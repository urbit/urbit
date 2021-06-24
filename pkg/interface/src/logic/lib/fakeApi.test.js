import { newApi } from './fakeApi';
describe('API shim', () => {
  it('should allow deep accesses', () => {
    const api = newApi();

    expect(api.foo.bar.baz.toString()).toBe('[fakeApi]');
  });

  it('should return promise on call', () => {
    const api = newApi();
    const method = api.foo.bar.baz;
    const res = method();

    expect('then' in res).toBe(true);
  });
});
