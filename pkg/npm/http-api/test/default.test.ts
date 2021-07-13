import Urbit from '../src';
import { Readable } from 'streams';

function fakeSSE(messages = [], timeout = 0) {
  const ourMessages = [...messages];
  const enc = new TextEncoder();
  return new ReadableStream({
    start(controller) {
      const interval = setInterval(() => {
        let message = ':\n';
        if (ourMessages.length > 0) {
          message = ourMessages.shift();
        }

        controller.enqueue(enc.encode(message));
      }, 50);

      if (timeout > 0) {
        setTimeout(() => {
          controller.close();
          interval;
        }, timeout);
      }
    },
  });
}

const ship = '~sampel-palnet';
let eventId = 0;
function event(data: any) {
  return `id:${eventId++}\ndata:${JSON.stringify(data)}\n\n`;
}

function fact(id: number, data: any) {
  return event({
    response: 'diff',
    id,
    json: data,
  });
}

function ack(id: number, err = false) {
  const res = err ? { err: 'Error' } : { ok: true };
  return event({ id, response: 'poke', ...res });
}
const fakeFetch = (body) => () =>
  Promise.resolve({
    ok: true,
    body: body(),
  });

const wait = (ms: number) => new Promise((res) => setTimeout(res, ms));

process.on('unhandledRejection', () => {
  console.error(error);
});

describe('Initialisation', () => {
  let airlock: Urbit;
  let fetchSpy;
  beforeEach(() => {
    airlock = new Urbit('', '+code');
    airlock.debounceInterval = 10;
  });
  afterEach(() => {
    fetchSpy.mockReset();
  });
  it('should poke & connect upon a 200', async () => {
    airlock.onOpen = jest.fn();
    fetchSpy = jest.spyOn(window, 'fetch');
    fetchSpy
      .mockImplementationOnce(() =>
        Promise.resolve({ ok: true, body: fakeSSE() })
      )
      .mockImplementationOnce(() =>
        Promise.resolve({ ok: true, body: fakeSSE() })
      );
    await airlock.eventSource();

    expect(airlock.onOpen).toHaveBeenCalled();
  }, 500);
  it('should handle failures', async () => {
    fetchSpy = jest.spyOn(window, 'fetch');
    fetchSpy
      .mockImplementation(() =>
        Promise.resolve({ ok: false, body: fakeSSE() })
      )
    airlock.onError = jest.fn();
    try {
      await airlock.eventSource();
      wait(100);
    } catch (e) {
      expect(airlock.onError).toHaveBeenCalled();
    }
  }, 200);
});

describe('subscription', () => {
  let airlock: Urbit;
  let fetchSpy: jest.SpyInstance;
  beforeEach(() => {
    eventId = 1;
  });
  afterEach(() => {
    fetchSpy.mockReset();
  });

  it('should subscribe', async () => {
    fetchSpy = jest.spyOn(window, 'fetch');
    airlock = new Urbit('', '+code');
    airlock.onOpen = jest.fn();
    const params = {
      app: 'app',
      path: '/path',
      err: jest.fn(),
      event: jest.fn(),
      quit: jest.fn(),
    };
    const firstEv = 'one';
    const secondEv = 'two';
    const events = (id) => [fact(id, firstEv), fact(id, secondEv)];
    fetchSpy.mockImplementation(fakeFetch(() => fakeSSE(events(1))));

    await airlock.subscribe(params);
    await wait(600);

    expect(airlock.onOpen).toBeCalled();
    expect(params.event).toHaveBeenNthCalledWith(1, firstEv);
    expect(params.event).toHaveBeenNthCalledWith(2, secondEv);
  }, 800);
  it('should poke', async () => {
    fetchSpy = jest.spyOn(window, 'fetch');
    airlock = new Urbit('', '+code');
    airlock.onOpen = jest.fn();
    fetchSpy.mockImplementation(fakeFetch(() => fakeSSE([ack(1)])));
    const params = {
      app: 'app',
      mark: 'mark',
      json: { poke: 1 },
      onSuccess: jest.fn(),
      onError: jest.fn(),
    };
    await airlock.poke(params);
    await wait(300);
    expect(params.onSuccess).toHaveBeenCalled();
  }, 800);

  it('should nack poke', async () => {
    fetchSpy = jest.spyOn(window, 'fetch');
    airlock = new Urbit('', '+code');
    airlock.onOpen = jest.fn();
    fetchSpy.mockImplementation(fakeFetch(() => fakeSSE([ack(1, true)])));
    const params = {
      app: 'app',
      mark: 'mark',
      json: { poke: 1 },
      onSuccess: jest.fn(),
      onError: jest.fn(),
    };
    try {
      await airlock.poke(params);
      await wait(300);
    } catch (e) {
      expect(params.onError).toHaveBeenCalled();
    }
  });
});
