require('event-target-polyfill');
require('yet-another-abortcontroller-polyfill');
require('cross-fetch/polyfill');
require('fast-text-encoding');
require('web-streams-polyfill');

global.ReadableStream = require('web-streams-polyfill').ReadableStream; 
  
