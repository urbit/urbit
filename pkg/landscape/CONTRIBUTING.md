## Introduction

Thanks for your interest in contributing to Landscape. This section
specifically focuses on Landscape development. Landscape lets you integrate
your ship with front-end web applications accessed through the browser. It has
a core set of applications that accept contributions.

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
repository, clone this repository and then run `make landscape`.  You can then
`cd` into an application's folder (chat, clock, etc.) and then `gulp watch` to
watch for changes.

Whenever you make changes, you can run `sh/load-arvo $PIER` in order to copy
them into your running develppment ship (where `$PIER` is the location of your
pier).  Once you're done, create a pull request for your source code changes in
the landscape directory.

Please also ensure your pull request fits our standards for
[Git hygiene][contributing].

[contributing]: /CONTRIBUTING.md#git-practice
[arvo]: /pkg/arvo
[landscape]:/pkg/landscape

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
