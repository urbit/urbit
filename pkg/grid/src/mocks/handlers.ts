import { Handler, SubscriptionHandler, createResponse } from '@tloncorp/mock-http-api';
import mockContacts from './mockContacts';
import { mockAllies, mockCharges, mockTreaties, mockVats } from '../state/mock-data';

const settingsSub = {
  action: 'subscribe',
  app: 'settings-store',
  path: '/desk/garden'
} as SubscriptionHandler;

const contactSub = {
  action: 'subscribe',
  app: 'contact-store',
  path: '/all',
  initialResponder: (req) =>
    createResponse(req, 'diff', {
      'contact-update': {
        initial: {
          'is-public': false,
          rolodex: mockContacts
        }
      }
    })
} as SubscriptionHandler;

const treatySub = {
  action: 'subscribe',
  app: 'treaty',
  path: '/treaty/~litzod/bitcoin'
};

const mockHandlers: Handler[] = [
  settingsSub,
  contactSub,
  treatySub,
  {
    action: 'scry',
    app: 'settings-store',
    path: '/desk/garden',
    func: () => ({
      desk: {
        display: {
          theme: 'auto'
        }
      }
    })
  },
  {
    action: 'scry',
    app: 'hood',
    path: '/kiln/vats',
    func: () => ({
      mockVats
    })
  },
  {
    action: 'scry',
    app: 'hood',
    path: '/kiln/lag',
    func: () => true
  },
  {
    action: 'scry',
    app: 'treaty',
    path: '/default-ally',
    func: () => '~zod'
  },
  {
    action: 'scry',
    app: 'docket',
    path: '/charges',
    func: () => ({
      initial: mockCharges
    })
  },
  {
    action: 'scry',
    app: 'treaty',
    path: '/allies',
    func: () => ({
      ini: mockAllies
    })
  },
  {
    action: 'scry',
    app: 'treaty',
    path: '/treaties',
    func: () => ({
      ini: mockTreaties
    })
  }
] as Handler[];

export default mockHandlers;
