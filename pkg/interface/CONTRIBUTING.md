## Introduction

Thanks for your interest in contributing to the Urbit interface. This section
specifically focuses on Landscape development. Landscape lets you integrate your
ship with front-end web applications accessed through the browser. It has a core
set of applications that accept contributions.

Related to Landscape is [Gall][gall], the Arvo vane that controls userspace
applications. Landscape applications will usually make good use of Gall, but
it's not strictly required if a Landscape application is not interacting with
ships directly.

## Starting the dev environment

From this directory, go to the config folder and copy `urbitrc-sample` to
`urbitrc`.

You should see the following:

```
module.exports = {
  URBIT_PIERS: [
    "/Users/user/ships/zod/home",
  ],
  herb: false,
  URL: 'http://localhost:80'
};
```

Change the URL to your livenet ship (if making front-end changes) or keep it the
same (if [developing on a local development ship][local]). Then, from the root
of the repository

```bash
npm i
npm run bootstrp
cd pkg/interface && npm run start
```

The dev server will start at `http://localhost:9000`. Sign in as you would
normally. Landscape will refresh automatically as you make changes.

#### Multi ship environments

If you are testing across multiple ships at once, and you would like to be able
to run the development server against all of the ships simulataneously, then do
the following.

Add an object under the `FLEET` key to your urbitrc.
```javascript
module.exports = {
  URL: 'http://localhost:80',
  FLEET: {
    'zod': 'http://localhost:8080',
    'bus': 'http://localhost:8081',
    'nus': 'http://localhost:8082'
  }
};

```

The dev environment will attempt to match the subdomain against the keys of this
object, and if matched will proxy to the corresponding URL. For example, the
above config will proxy `zod.localhost:9000` to `http://localhost:8080`,
`bus.localhost:9000` to `http://localhost:8081` and so on and so forth. If no
match is found, then it will fallback to the `URL` property.

## Linting

The Urbit interface uses Eslint to lint the JavaScript code. To install the
linter and for usage through the command, do the following:

```bash
$ cd ./pkg/interface
$ npm ci
$ npm run lint
```

To use the linter, run npm scripts

```bash
$ npm run lint # lints all files in `interface`
$ npm run lint-file ./src/apps/chat/**/*.js # lints all .js files in `interface/chat`
$ npm run lint-file ./src/chat/app.js # lints a single chosen file
```

## Creating your own applications

If you'd like to create your own application for Landscape, the easiest way to
get started is using the [create-landscape-app][cla] repository template. It
provides a brief wizard when you run it with `npm start`, and has good
documentation for its everyday use -- just create a repo [using its
template][template], install and then start it, and you'll soon be up and
running.

[cla]: https://github.com/urbit/create-landscape-app
[template]: https://github.com/urbit/create-landscape-app/generate
[gall]:https://urbit.org/docs/learn/arvo/gall/
[local]: /CONTRIBUTING.md#fake-ships
