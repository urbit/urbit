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

If you'd like to contribute to the core set of Landscape applications in this
repository, clone this repository and start by creating an `urbitrc` file in
this folder, [pkg/interface][interface]. You can find an `urbitrc-sample` here
for reference. Then `cd` into the application's folder and `npm install` the
dependencies, then `gulp watch` to watch for changes.

On your development ship, ensure you `|commit %home` to apply your changes.
Once you're done and ready to make a pull request, running `gulp bundle-prod`
will make the production files and deposit them in [pkg/arvo][arvo]. Create a
pull request with both the production files, and the source code you were
working on in the interface directory.

Please also ensure your pull request fits our standards for
[Git hygiene][contributing].

[contributing]: /CONTRIBUTING.md#git-practice
[arvo]: /pkg/arvo
[interface]:/pkg/interface

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
