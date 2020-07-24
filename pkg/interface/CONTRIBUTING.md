## Introduction

Thanks for your interest in contributing to the Urbit interface. This section
specifically focuses on Landscape development. Landscape lets you integrate your
ship with front-end web applications accessed through the browser. It has a core
set of applications that accept contributions.

Related to Landscape is [Gall][gall], the Arvo vane that controls userspace
applications. Landscape applications will usually make good use of Gall, but
it's not strictly required if a Landscape application is not interacting with
ships directly.

## Contributing to Landscape applications

To begin developing on Landscape, find the `urbitrc-sample` file found
at `urbit/pkg/interface/config/urbitrc-sample`. Copy it as `urbitrc`.
Open it using your preferred code editor and you should see the following:

```
module.exports = {
  URBIT_PIERS: [
    "/Users/user/ships/zod/home",
  ],
  herb: false,
  URL: 'http://localhost:80'
};
```

This file is the configuration file for your front-end development environment.
Let's walk through it.

The first line, listing piers, specifies which piers to copy the JS files into.
By default, the development environment won't copy files into any pier, even if
you've set the pier in `urbitrc`.

If you want to copy the JS files into your ship, as it would run in a regular
user environment, uncomment these lines in
`pkg/interface/config/webpack.dev.js`:

```javascript
// uncomment to copy into all piers
//
// return Promise.all(this.piers.map(pier => {
//   const dst = path.resolve(pier, 'app/landscape/js/index.js');
//   copyFile(src, dst).then(() => {
//     if(!this.herb) {
//       return;
//     }
//     pier = pier.split('/');
//     const desk = pier.pop();
//     return exec(`herb -p hood -d '+hood/commit %${desk}' ${pier.join('/')}`);
//   });
// }));
```

And then set your pier in `urbitrc` (ensure it ends in `/home`). The `herb`
option in your `urbitrc` will automatically commit the changes to your ship if
you have herb installed (see `pkg/herb`).

For most developers, if you are making changes to Landscape without any back-end
changes on the Urbit ship itself, and you have an Urbit ship running already,
you don't have to boot a development ship. You can simply set up the dev server
for the development environment and point it at your running ship.

To do this, set the `URL` property in your urbitrc and replace it with the URL
of the urbit that you are testing on. For example, a development ship by default
lives at `localhost:80` so our `urbitrc` would have:

```javascript
module.exports = {
  URL: 'http://localhost:80'
}
```

Then get everything installed:

```
## go to urbit's interface directory and install the required tooling
cd urbit/pkg/interface
npm install

## Start your development server
npm run start
```

You can then access a hot reloaded version of the interface at
`http://localhost:9000`.

If you set the URL to your running ship, like
`http://sampel-palnet.arvo.network`, then you can use your actual ship while
running local-only development changes.

As previously stated, if your changes require back-end development (front-end
and Gall changes, for example), or you just want an empty development
environment, you'll want to create a development ship.

### Creating a development ship

[nix](https://github.com/NixOS/nix) and `git-lfs` should be installed at this
point, and have been used to `make build` the project.

First follow the
[instructions](https://urbit.org/using/develop/#creating-a-development-ship) for
fake `~zod` initialization.

Once your fake ship is running and you see
```
~zod:dojo>
```
in your console, be sure to 'mount' your ship's working state (what we call
'desks') to your local machine via the `|mount %` command. This will ensure that
code you modify locally can be committed to your ship and initialized.

To set up urbit's Javascript environment, you'll need node (ideally installed
via [nvm](https://github.com/nvm-sh/nvm)) and webpack, which will be installed
via node.

If you want to copy the code into your ship, perform the following steps:

```
## go to urbit's interface directory and install the required tooling
cd urbit/pkg/interface
npm install

## Build the JS code
npm run build:dev
```

If you want to run the JavaScript code in a dev server, you can simply set the
URL in your `urbitrc` to `localhost:80` and `npm run start` instead.

If you set your pier in `urbitrc`, and uncommented the code in the webpack
config, then once the build process is running, commit on your ship to copy the
changed JS code in:

```
|commit %home
```

Your urbit should take a moment to process the changes, and will emit a `>=`.
Refreshing your browser will display the newly-rendered interface.

Once you are done editing code, and wish to commit changes to git, stop your
process. Do not commit compiled code, but submit the source code
for review.

Please also ensure your pull request fits our standards for [Git
hygiene][contributing].

[contributing]: /CONTRIBUTING.md#git-practice
[arvo]: /pkg/arvo
[interface]:/pkg/interface

## Linting

The Urbit interface uses Eslint to lint the JavaScript code. To install the
linter and for usage through the command, do the following:

```bash
$ cd ./pkg/interface
$ npm install
$ npm run lint
```

To use the linter, run npm scripts

```bash
$ npm run lint # lints all files in `interface`
$ npm run lint-file ./src/apps/chat/**/*.js # lints all .js files in `interface/chat`
$ npm run lint-file ./src/chat/app.js # lints a single chosen file
```

### Gall

Presently, Gall documentation is still in [progress][gall], but a good
reference. For examples of Landscape apps that use Gall, see the code for
[Chat][chat] and [Publish][publish].

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
[chat]: /pkg/arvo/app/chat-view.hoon
[publish]: /pkg/arvo/app/publish.hoon