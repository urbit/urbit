import { Handler } from '@urbit/mock-http-api';
import { mockAllies, mockCharges } from './mock-data';

export const handlers: Handler[] = [
  {
    app: 'treaty',
    path: '/default-ally',
    func: () => '~zod'
  },
  {
    app: 'docket',
    path: '/charges',
    func: () => ({ initial: mockCharges })
  },
  {
    app: 'treaty',
    path: '/allies',
    func: () => ({ ini: mockAllies })
  },
  {
    app: 'hood',
    path: '/kiln/lag',
    func: () => false
  },
  {
    app: 'settings-store',
    path: '/desk/garden',
    func: () => {
      return {
        desk: 'garden'
      };
    }
  }
];
