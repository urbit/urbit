import { ProxyStreamMessage } from 'src';

const sw = self as ServiceWorkerGlobalScope & typeof globalThis;

interface ProxyMessage {
  type: 'PROXY_MESSAGE';
  id: string;
  clientId: string;
  payload: string;
}

interface StreamMap {
  [key: string]: ReadableStreamController<any>;
}

interface EventToEncode {
  data: string;
  event?: string;
  retry?: string;
  id?: string;
}

export function proxyStreams() {
  const proxiedStreams: StreamMap = {};
  const encoder = new TextEncoder();

  function encodeEvent({ data, event, retry, id }: EventToEncode) {
    const responseText = sseChunkData(data, event, retry, id);
    return encoder.encode(responseText);
  }

  // Function for formatting message to SSE response
  function sseChunkData(
    data: string,
    event: string,
    retry: string,
    id: string
  ) {
    return `${Object.entries({ event, id, data, retry })
      .filter(([, value]) => ![undefined, null].includes(value))
      .map(([key, value]) => `${key}: ${value}`)
      .join('\n')}\n\n`;
  }

  function proxyMessage(msg: ProxyMessage) {
    const stream = proxiedStreams[msg.id];
    stream.enqueue(msg.payload);
  }

  sw.addEventListener('message', async (event) => {
    if (!event.data) {
      return;
    }

    if (event.data.type === 'PROXY_MESSAGE') {
      proxyMessage(event.data);
    }
  });

  self.addEventListener('fetch', (event: FetchEvent) => {
    const { headers, url } = event.request;
    if (
      headers.get('Accept') !== 'text/event-stream' ||
      url.indexOf('?id=') === -1
    ) {
      return;
    }

    const parsedUrl = new URL(url);
    const id = parsedUrl.searchParams.get('id');

    const stream = new ReadableStream({
      start: (controller) => {
        controller.enqueue(encodeEvent({ data: 'hello!' }));
        const msg: ProxyStreamMessage = {
          type: 'PROXY_STREAM',
          id,
          clientId: event.clientId,
          url: parsedUrl.href,
        };

        proxiedStreams[id] = controller;

        setTimeout(() => {
          sw.clients.get(event.clientId).then((client) => {
            client.postMessage(msg);
          });
        }, 0);
      },
    });

    console.log('initiating proxied stream', id);
    const response = new Response(stream, {
      headers: {
        'content-type': 'text/event-stream',
        'Transfer-Encoding': 'chunked',
        Connection: 'keep-alive',
      },
    });
    return event.respondWith(response);
  });
}
