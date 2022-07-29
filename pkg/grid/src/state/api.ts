import Urbit, { PokeInterface, Scry, SubscriptionRequestInterface, Thread } from '@urbit/http-api';
import type UrbitMock from '@tloncorp/mock-http-api';

declare global {
  interface Window {
    ship: string;
    our: string;
  }
}

export const IS_MOCK = import.meta.env.MODE === 'mock';
const URL = (import.meta.env.VITE_MOCK_URL || import.meta.env.VITE_VERCEL_URL) as string;

let client = undefined as unknown as Urbit | UrbitMock;

async function setupAPI() {
  if (IS_MOCK) {
    window.ship = 'finned-palmer';
    window.our = `~${window.ship}`;
    window.desk = 'garden';
    const MockUrbit = (await import('@tloncorp/mock-http-api')).default;
    const mockHandlers = (await import('../mocks/handlers')).default;

    if (!client) {
      const api = new MockUrbit(mockHandlers, URL || '', '');
      api.ship = window.ship;
      api.verbose = true;
      client = api;
    }

    return;
  }

  if (!client) {
    const api = new Urbit('', '', window.desk);
    api.ship = window.ship;
    api.verbose = true;
    client = api;
  }
}

const api = {
  async scry<T>(params: Scry) {
    if (!client) {
      await setupAPI();
    }

    return client.scry<T>(params);
  },
  async poke<T>(params: PokeInterface<T>) {
    if (!client) {
      await setupAPI();
    }

    return client.poke<T>(params);
  },
  async subscribe(params: SubscriptionRequestInterface) {
    if (!client) {
      await setupAPI();
    }

    return client.subscribe(params);
  },
  async thread<Return, T>(params: Thread<T>) {
    if (!client) {
      await setupAPI();
    }

    return client.thread<Return, T>(params);
  },
  async unsubscribe(id: number) {
    if (!client) {
      await setupAPI();
    }

    return client.unsubscribe(id);
  }
} as Urbit | UrbitMock;

export default api;
