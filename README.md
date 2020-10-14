# Urbit

[Urbit](https://urbit.org) is a personal server stack built from scratch. It
has an identity layer (Azimuth), virtual machine (Vere), and operating system
(Arvo).

A running Urbit "ship" is designed to operate with other ships peer-to-peer.
Urbit is a general-purpose, peer-to-peer computer and network.

This repository contains:

- The [Arvo OS][arvo]
- [herb][herb], a tool for Unix control of an Urbit ship
- Source code for [Landscape's web interface][land]
- Source code for the [vere][vere] virtual machine.

For more on the identity layer, see [Azimuth][azim]. To manage your Urbit
identity, use [Bridge][brid].

[arvo]: https://github.com/urbit/urbit/tree/master/pkg/arvo
[azim]: https://github.com/urbit/azimuth
[brid]: https://github.com/urbit/bridge
[herb]: https://github.com/urbit/urbit/tree/master/pkg/herb
[land]: https://github.com/urbit/urbit/tree/master/pkg/interface
[vere]: https://github.com/urbit/urbit/tree/master/pkg/urbit

## Install

To install and run Urbit, please follow the instructions at
[urbit.org/using/install][start].  You'll be on the live network in a
few minutes.

If you're interested in Urbit development, keep reading.

[start]: https://urbit.org/using/install/

## Development

[![Build Status](https://travis-ci.org/urbit/urbit.svg?branch=master)][trav]

Urbit uses [Nix][nix] to manage builds.  On Linux and macOS you can install Nix
via:

```
curl -L https://nixos.org/nix/install | sh
```

The Makefile in the project's root directory contains useful phony targets for
building, installing, testing, and so on.  You can use it to avoid dealing with
Nix explicitly.

To build the Urbit virtual machine binary, for example, use:

```
make build
```

The test suite can similarly be run via a simple:

```
make test
```

Note that some of the Makefile targets need access to pills tracked via [git
LFS][git-lfs], so you'll also need to have those available locally:

```
git lfs install
git lfs pull
```

[trav]: https://github.com/urbit/urbit.git
[nix]: https://nixos.org/nix/
[git-lfs]: https://git-lfs.github.com

## Contributing

Contributions of any form are more than welcome!  Please take a look at our
[contributing guidelines][cont] for details on our git practices, coding
styles, how we manage issues, and so on.

For instructions on contributing to Landscape, see [its][lcont] guidelines.

You might also be interested in joining the [urbit-dev][list] mailing list.

[list]: https://groups.google.com/a/urbit.org/forum/#!forum/dev
[cont]: https://github.com/urbit/urbit/blob/master/CONTRIBUTING.md
[lcont]: https://github.com/urbit/urbit/blob/master/pkg/interface/CONTRIBUTING.md