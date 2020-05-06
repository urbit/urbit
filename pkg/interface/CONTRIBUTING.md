## Introduction

Thanks for your interest in contributing to the Urbit interface. This section
specifically focuses on Landscape development. Landscape lets you integrate your
ship with front-end web applications accessed through the browser. It has a core
set of applications that accept contributions.

Related to Landscape is [Gall][gall], the Arvo vane that controls userspace
applications. Landscape applications will usually make good use of Gall, but
it's not strictly required if a Landscape application is not interacting with
ships directly.

Create a development ship, then once your ship is running, mount to Unix with
`|mount %`. This will create a folder named 'home' in your pier in Unix. The
'home' desk contains the working state of your ship -- like a Git repository,
when you want to make a change to it, `|commit %home`.

## Contributing to Landscape applications

[nix](https://github.com/NixOS/nix) and `git-lfs` should be installed at
this point, and have been used to `make build` the project.

Designing interfaces within urbit/urbit additionally requires that the [instructions](https://urbit.org/using/develop/#creating-a-development-ship) for fake `~zod` initialization have been followed. 

Once your fake ship is running and you see
```
~zod:dojo>
```
in your console, be sure to 'mount' your ship's working state (what we call 'desks') to your local machine via the 
`|mount %` command. This will ensure that code you modify locally can be 
committed to your ship and initialized.

To begin developing Urbit's frontend, you'll need to sync your 
currently-running fake ship with the urbit/urbit repo's code. Find the 
`urbitrc-sample` file found at `urbit/pkg/interface/urbitrc-sample` (in this folder). Open it 
using your preferred code editor and you should see the following:

```
module.exports = {
  URBIT_PIERS: [
    "/Users/user/ships/zod/home",
  ]
};
```

Edit the path between quotes `/Users/user/ships/zod/home` with wherever your 
fake ship is located on your machine. This zod location path *must* end in `../home` to correctly intitalize 
any code you write. Any code edited within the `urbit/urbit`will now be able to be synced to your running 
ship, and previewed in the browser. Save this file in the same directory with the name `urbitrc`

To set up urbit's Javascript environment, you'll need node (ideally installed
via [nvm](https://github.com/nvm-sh/nvm)) and gulp, which will be installed 
via node.

Perform the following steps to get the above set up for urbit's apps:

```
## go to urbit's interface directory and install the required tooling
cd urbit/pkg/interface
npm install
npm install -g gulp

## assuming you are still in `urbit/pkg/interface`,
## open a single app directory, and watch it for changes
cd contacts/
gulp watch
```

Any changes made to any files within the `/contacts` directory will now 
trigger a gulp rebuild when saved. To sync these changes to your running 
ship, enter dojo and input the following:

```
|commit %home
```

Your urbit should take a moment to process the changes, and will emit a
`>=`. Refreshing your browser will display the newly-rendered interface.

Once you are done editing code, and wish to commit changes to git, stop
`gulp watch` and run `gulp bundle-prod` to ensure you are only 
committing 1 minified line of compiled js and not 3000+.

An additional note:

As compiled Javascript is not present in the urbit/urbit repository,
you'll need to run `.sh/build-interface` in order to see changes that
have been committed to any given branch you might be working on. It's
always a good idea to run the above command before starting development
to ensure you can see collaborators' changes.

Please also ensure your pull request fits our standards for
[Git hygiene][contributing].

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
$ npm run lint-file ./chat/**/*.js # lints all .js files in `interface/chat`
$ npm run lint-file ./chat/src/index.js # lints a single chosen file
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
[gall]: https://urbit.org/docs/learn/arvo/gall/
[chat]: /pkg/arvo/app/chat.hoon
[publish]: /pkg/arvo/app/publish.hoon
