/* eslint-disable no-restricted-globals */
import { fetchEventSource } from '@microsoft/fetch-event-source';
import { registerRoute } from 'workbox-routing';
import { precacheAndRoute } from 'workbox-precaching';

const isDev = import.meta.env && import.meta.env.DEV;

if (!isDev) {
  /* eslint-disable no-underscore-dangle */
  precacheAndRoute(self.__WB_MANIFEST);
} else {
  isDev && console.log('skipping precache in dev');
}

self.addEventListener('install', () => {
  self.skipWaiting();
});

self.addEventListener('activate', (ev) => {
  ev.waitUntil(self.clients.claim());
});

/**
 * EventStream Proxy
 */

// Headers for SSE response
const sseHeaders = {
  'content-type': 'text/event-stream',
  'Transfer-Encoding': 'chunked',
  Connection: 'keep-alive'
};
const encoder = new TextEncoder();
// Map with server connections, where key - url, value - EventSource
const serverConnection = {};
// For each request opens only one server connection and use it for next requests with the same url
async function getServerConnection(url) {
  return new Promise((resolve, reject) => {
    if (!serverConnection.url) {
      serverConnection.url = url;
    }

    if (!serverConnection.source) {
      const listeners = [];
      const source = new AbortController();
      fetchEventSource(serverConnection.url || url, {
        credentials: 'include',
        accept: '*',
        headers: {
          'Content-Type': 'application/json'
        },
        signal: source.signal,
        openWhenHidden: true,
        onopen: () => {
          serverConnection.source = source;
          serverConnection.listeners = listeners;
          serverConnection.count = 0;
          serverConnection.eventId = 0;
          resolve(serverConnection);
        },
        onmessage: (event) => {
          const parsedData = JSON.parse(event.data);
          isDev && console.log(event.data);

          // if (!('ok' in parsedData)) {
          serverConnection.eventId = parseInt(parsedData.id, 10);
          // }

          const responseData = encodeEvent(event);
          const handlers = serverConnection.listeners.slice();
          handlers.forEach(({ handle }) => {
            handle(responseData);
          });
        },
        onerror: () => {
          reject(new Error('failed to initiate stream'));
        },
        onclose: () => {
          const handlers = serverConnection.listeners.slice();
          handlers.forEach(({ close }) => {
            close();
          });
          clearConnection();
        }
      });
    } else {
      resolve(serverConnection);
    }
  });
}

registerRoute(
  ({ request }) => {
    const { headers, url } = request;
    return headers.get('Accept') === 'text/event-stream' && url.indexOf('?id=') !== -1;
  },
  async ({ url }) => {
    const id = url.searchParams.get('id');
    url.searchParams.delete('id');
    try {
      const connection = await getServerConnection(url.href);
      isDev && console.log(connection);

      const stream = new ReadableStream({
        start: (controller) => {
          controller.enqueue(encodeEvent({ data: 'hello!' }));
          connection.count += 1;
          isDev && console.log('current', connection.count, id);

          connection.listeners.push({
            id,
            handle: (responseData) => controller.enqueue(responseData),
            close: () => controller.close()
          });
        }
      });

      isDev && console.log('initiating stream', serverConnection.url);
      return new Response(stream, { headers: sseHeaders });
    } catch (error) {
      return new Response(error?.message, {
        status: 500
      });
    }
  }
);

const handlerMap = {
  GET_CHANNEL: getChannel,
  CLOSE_STREAM: closeStream,
  PROXY_STREAM: proxyStream
};

self.addEventListener('message', async (event) => {
  if (!event.data) {
    return;
  }

  const handler = handlerMap[event.data.type];

  if (handler) {
    await handler(event.data);
  }
});

function getChannel() {
  self.clients
    .matchAll({
      includeUncontrolled: true,
      type: 'window'
    })
    .then((clients) => {
      const message = {
        type: 'CURRENT_CHANNEL',
        eventId: serverConnection.eventId,
        channel: serverConnection.url
      };

      isDev && console.log(message);
      if (clients && clients.length) {
        clients.forEach((client) => client.postMessage(message));
      }
    });
}

async function closeStream({ url, id }) {
  const connection = await getServerConnection(url);

  if (connection.count <= 1) {
    clearConnection(connection.url);
  } else {
    const index = connection.listeners.findIndex((item) => item.id === id);
    let listener = connection.listeners[index];
    if (!listener) {
      return;
    }

    connection.listeners.splice(index, 1);
    connection.count -= 1;
    listener.close();
    listener = null;

    isDev && console.log({ url, count: connection.count, listeners: connection.listeners });
  }
}

async function proxyStream({ id, clientId, url }) {
  const connection = await getServerConnection(url);
  if (connection.listeners.find((listener) => listener.id === id)) {
    return;
  }

  isDev && console.log(connection);
  connection.count += 1;
  isDev && console.log('current', connection.count, id);

  connection.listeners.push({
    id,
    handle: (responseData) => {
      self.clients.get(clientId).then((client) => {
        if (!client) {
          return;
        }

        const message = {
          type: 'PROXY_MESSAGE',
          id,
          payload: responseData
        };

        isDev && console.log('sending proxy message', message);
        client.postMessage(message);
      });
    },
    close: () => {}
  });

  isDev && console.log('proxied stream attached', serverConnection.url, id);
  self.clients.get(clientId).then((client) => {
    client.postMessage({
      type: 'STREAM_PROXIED',
      id
    });
  });
}

function clearConnection(url) {
  serverConnection.source.abort();

  delete serverConnection.url;
  delete serverConnection.source;
  delete serverConnection.listeners;
  delete serverConnection.count;

  if (!url) {
    isDev && console.log('root connection closed');
    return;
  }

  fetch(url, {
    body: JSON.stringify([{ action: 'delete' }]),
    method: 'PUT'
  });
  isDev && console.log('closing root connection', url);
}

function encodeEvent({ data, event, retry, id }) {
  const responseText = sseChunkData(data, event, retry, id);
  return encoder.encode(responseText);
}

// Function for formatting message to SSE response
function sseChunkData(data, event, retry, id) {
  return `${Object.entries({ event, id, data, retry })
    .filter(([, value]) => ![undefined, null].includes(value))
    .map(([key, value]) => `${key}: ${value}`)
    .join('\n')}\n\n`;
}
