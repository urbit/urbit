import { deSig } from './util';

describe('#deSig', () => {
  describe('when string has a sig', () => {
    it('returns string without the sig', () => {
        const input = '~zod';

        expect(deSig(input)).toEqual('zod');
    });
  });
});
