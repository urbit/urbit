# Urbit Connector

This project allows you to connect to an [Urbit](https://urbit.org) ship via a JavaScript application.

## Example

Check out the `example` directory for examples of how to use this code.

1. Open `example/index.html` in your browser and follow the instructions there, or
2. With a ship running in the same fashion as indicated in the file above, run `node example/index.js`

The code for either of these can be found in `src/example/browser.js` or `src/example/node.js`, depending on your context.

## Design

This library is designed to be useful for node applications that communicate with an urbit running either on the local computer or on a remote one.

The majority of its methods are asynchronous and return Promises. This is due to the non-blocking nature of JavaScript. If used in a React app, response handlers should be bound with `this` to `setState` after a message is received.

## NOTE
You must enable CORS requests on your urbit for this library to work in browser context. Use `+cors-registry` to see domains which have made requests to your urbit, and then approve the needed one, e.g. `|cors-approve http://zod.arvo.network`.
