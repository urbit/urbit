// Used for filtering matches based on status code, header, or both
import { CacheableResponsePlugin } from 'workbox-cacheable-response';
// Used to limit entries in cache, remove entries after a certain period of time
import { ExpirationPlugin } from 'workbox-expiration';
import { registerRoute } from 'workbox-routing';
import {
  CacheFirst, NetworkFirst,
  StaleWhileRevalidate
} from 'workbox-strategies';

//  generate a different sw for every build, to bust cache properly
const hash = process.env.LANDSCAPE_SHORTHASH;

self.addEventListener('install', (ev) => {
  self.skipWaiting();
  console.log('registed sw', hash);
});

self.addEventListener('activate', (ev) => {
  ev.waitUntil(self.clients.claim());
});

// Cache page navigations (html) with a Network First strategy
registerRoute(
  // Check to see if the request is a navigation to a new page
  ({ request }) => request.mode === 'navigate',
  // Use a Network First caching strategy
  new NetworkFirst({
    // Put all cached files in a cache named 'pages'
    cacheName: 'pages',
    plugins: [
      // Ensure that only requests that result in a 200 status are cached
      new CacheableResponsePlugin({
        statuses: [200]
      })
    ]
  })
);

// Cache CSS, JS, and Web Worker requests with a Stale While Revalidate strategy
registerRoute(
  // Check to see if the request's destination is style for stylesheets, script for JavaScript, or worker for web worker
  ({ request }) =>
    request.destination === 'style' ||
//    request.destination === 'script' ||
    request.destination === 'worker',
  // Use a Stale While Revalidate caching strategy
  new StaleWhileRevalidate({
    // Put all cached files in a cache named 'assets'
    cacheName: 'assets',
    plugins: [
      // Ensure that only requests that result in a 200 status are cached
      new CacheableResponsePlugin({
        statuses: [200]
      })
    ]
  })
);

registerRoute(
  ({ url }) => url.orign === 'https://noembed.com',
  new CacheFirst({
    cacheName: 'embeds',
    plugins: [
      // Ensure that only requests that result in a 200 status are cached
      new CacheableResponsePlugin({
        statuses: [200]
      }),
      // Don't cache more than 150 items, and expire them after 30 days
      new ExpirationPlugin({
        maxEntries: 150,
        maxAgeSeconds: 60 * 60 * 24 * 30 // 30 Days
      })
    ]
  })
);

// Cache images with a Cache First strategy
registerRoute(
  // Check to see if the request's destination is style for an image
  ({ request }) => request.destination === 'image',
  // Use a Cache First caching strategy
  new CacheFirst({
    // Put all cached files in a cache named 'images'
    cacheName: 'images',
    plugins: [
      // Ensure that only requests that result in a 200 status are cached
      new CacheableResponsePlugin({
        statuses: [200]
      }),
      // Don't cache more than 50 items, and expire them after 30 days
      new ExpirationPlugin({
        maxEntries: 50,
        maxAgeSeconds: 60 * 60 * 24 * 30 // 30 Days
      })
    ]
  })
);

/**
 * EventStream Proxy
 */

// Headers for SSE response
const sseHeaders = {
  'content-type': 'text/event-stream',
  'Transfer-Encoding': 'chunked',
  'Connection': 'keep-alive'
};
const encoder = new TextEncoder();
// Map with server connections, where key - url, value - EventSource
const serverConnection = {};
// For each request opens only one server connection and use it for next requests with the same url
function getServerConnection (url) {
  if (!serverConnection.url) {
    serverConnection.url = url;
  }

  if (!serverConnection.source || serverConnection.source.readyState === 2) {
    const source = new EventSource(serverConnection.url || url);
    const listeners = [];

    source.onmessage = (event) => {
      const parsedData = JSON.parse(event.data);
      serverConnection.eventId = parseInt(parsedData.id, 10);

      const responseData = encodeEvent(event);
      const handlers = serverConnection.listeners.slice();
      handlers.forEach(({ handle }) => {
        handle(responseData);
      });
    };

    serverConnection.source = source;
    serverConnection.listeners = listeners;
    serverConnection.count = 0;
    serverConnection.eventId = 0;
  }

  return serverConnection;
};

registerRoute(
  ({ request }) => {
    const { headers } = request;
    return headers.get('Accept') === 'text/event-stream';
  },
  ({ url }) => {
    const id = url.searchParams.get('id');
    url.searchParams.delete('id');

    const stream = new ReadableStream({
      start: (controller) => {
        controller.enqueue(encodeEvent({ data: 'hello!' }));

        const connection = getServerConnection(url.href);
        connection.count += 1;
        console.log('current', connection.count, id);

        connection.listeners.push({
          id,
          handle: responseData => controller.enqueue(responseData),
          close: () => controller.close()
        });
      }
    });

    console.log('initiating stream', serverConnection.url);
    return new Response(stream, { headers: sseHeaders });
  }
);

const handlerMap = {
  GET_CHANNEL: getChannel,
  CLOSE_STREAM: closeStream
};

self.addEventListener('message', (event) => {
  if (!event.data) {
    return;
  }

  handlerMap[event.data.type](event.data);
});

function getChannel() {
  self.clients.matchAll({
    includeUncontrolled: true,
    type: 'window'
  }).then((clients) => {
    const message = {
      type: 'CURRENT_CHANNEL',
      eventId: serverConnection.eventId,
      channel: serverConnection.url
    };

    console.log(message);
    if (clients && clients.length) {
      clients.forEach(client => client.postMessage(message));
    }
  });
}

function closeStream({ url, id }) {
  const connection = getServerConnection(url);

  if (connection.count <= 1) {
    clearConnection();
  } else {
    const index = connection.listeners.findIndex(item => item.id === id);
    let listener = connection.listeners[index];
    if (!listener) {
      return;
    }

    connection.listeners.splice(index, 1);
    connection.count -= 1;
    listener.close();
    listener = null;

    console.log({ url, count: connection.count, listeners: connection.listeners });
  }
}

function clearConnection(url) {
  serverConnection.source.close();

  delete serverConnection.url;
  delete serverConnection.source;
  delete serverConnection.listeners;
  delete serverConnection.count;

  fetch(url, {
    body: JSON.stringify([{ action: 'delete' }]),
    method: 'PUT'
  });
  console.log('closing', url);
}

function encodeEvent({ data, type, retry, lastEventId }) {
  const responseText = sseChunkData(data, type, retry, lastEventId);
  return encoder.encode(responseText);
}

// Function for formatting message to SSE response
function sseChunkData(data, event, retry, id) {
  return Object.entries({ event, id, data, retry })
              .filter(([, value]) => ![undefined, null].includes(value))
              .map(([key, value]) => `${key}: ${value}`)
              .join('\n') + '\n\n';
}
