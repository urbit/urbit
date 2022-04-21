import { Handler } from '@urbit/mock-http-api';
import { mockAllies, mockCharges, mockSubscribeResponse, mockVats } from './mock-data';

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
    app: 'hood',
    path: '/kiln/vats',
    func: () => mockVats
  },
  {
    app: 'settings-store',
    path: '/desk/garden',
    func: () => {
      return {
        desk: 'garden'
      };
    }
  },
  {
    action: 'subscribe',
    app: 'hark-store',
    path: '/notes',
    func: () => mockSubscribeResponse
  },
  {
    action: 'subscribe',
    app: 'hark-store',
    path: '/updates',
    func: () => mockSubscribeResponse
  },
  {
    action: 'subscribe',
    app: 'docket',
    path: '/charges',
    func: () => mockSubscribeResponse
  },
  {
    action: 'subscribe',
    app: 'treaty',
    path: '/treaties',
    func: () => mockSubscribeResponse
  },
  {
    action: 'subscribe',
    app: 'treaty',
    path: '/allies',
    func: () => mockSubscribeResponse
  },
  {
    action: 'subscribe',
    app: 'hood',
    path: '/kiln/vats',
    func: () => mockSubscribeResponse
  },
  {
    action: 'subscribe',
    app: 'settings-store',
    path: '/desk/garden',
    func: () => mockSubscribeResponse
  },
  {
    action: 'subscribe',
    app: 'contact-pull-hook',
    path: '/nacks',
    func: () => mockSubscribeResponse
  },
  {
    action: 'subscribe',
    app: 'contact-store',
    path: '/all',
    func: () => mockSubscribeResponse
  },
  {
    action: 'subscribe',
    app: 'hark-graph-hook',
    path: '/updates',
    func: () => mockSubscribeResponse
  }
];
