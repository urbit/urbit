import Urbit from '../src';

describe('blah', () => {
  it('works', () => {
    const connection = new Urbit('~sampel-palnet', '+code');
    expect(connection).toEqual(2);
  });
});
