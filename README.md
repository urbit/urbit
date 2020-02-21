# Urbit

A personal server operating function.

> The Urbit address space, Azimuth, is now live on the Ethereum blockchain. You
> can find it at [`0x223c067f8cf28ae173ee5cafea60ca44c335fecb`][azim] or
> [`azimuth.eth`][aens]. Owners of Azimuth points (galaxies, stars, or planets)
> can view or manage them using [Bridge][brid], and can also use them to boot
> [Arvo][arvo], the Urbit OS.

[azim]: https://etherscan.io/address/0x223c067f8cf28ae173ee5cafea60ca44c335fecb
[aens]: https://etherscan.io/address/azimuth.eth
[brid]: https://github.com/urbit/bridge
[arvo]: https://github.com/urbit/urbit/tree/master/pkg/arvo

## Install

To install and run Urbit, please follow the instructions at
[urbit.org/docs/getting-started/][start].  You'll be on the live network in a
few minutes.

If you're interested in Urbit development, keep reading.

[start]: https://urbit.org/docs/getting-started/

## Development

[![Build Status](https://travis-ci.org/urbit/urbit.svg?branch=master)][trav]

Urbit uses [Nix][nix] to manage builds.  On Linux and macOS you can install Nix
via:

```
curl https://nixos.org/nix/install | sh
```

The Makefile in the project's root directory contains useful phony targets for
building, installing, testing, and so on.  You can use it to avoid dealing with
Nix explicitly.

To build Urbit, for example, use:

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

You might also be interested in:

- joining the [urbit-dev][list] mailing list.
- [applying to Hoon School][mail], a course we run to teach the Hoon
  programming language and Urbit application development.

[list]: https://groups.google.com/a/urbit.org/forum/#!forum/dev
[mail]: mailto:support@urbit.org
[cont]: https://github.com/urbit/urbit/blob/master/CONTRIBUTING.md
