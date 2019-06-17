# Urbit

A personal server operating function.

> The Urbit address space, Azimuth, is now live on the Ethereum blockchain. You
> can find it at [`0x223c067f8cf28ae173ee5cafea60ca44c335fecb`][azim] or
> [`azimuth.eth`][aens]. Owners of Azimuth points (galaxies, stars, or planets)
> can view or manage them using [Bridge][brid], and can also use them to boot
> [Arvo][arvo], the Urbit OS.

[azim]: https://etherscan.io/address/0x223c067f8cf28ae173ee5cafea60ca44c335fecb
[aens]: https://etherscan.io/address/azimuth.eth
[brid]: https://github.com/urbit/bridge/releases
[arvo]: https://github.com/urbit/arvo/

## Install

To install and run Urbit, please follow the instructions at
[urbit.org/docs/getting-started/][start].  You'll be on the live network in a
few minutes.

If you're interested in Urbit development, keep reading.

[start]: https://urbit.org/docs/getting-started/

## Development

[![Build Status](https://travis-ci.org/urbit/urbit.svg?branch=master)][trav]

Urbit uses [Nix][nix] to manage builds.  On Linux and OS X you can install Nix
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

[trav]: https://github.com/urbit/urbit.git
[nix]: https://nixos.org/nix/

## Contributing

Contributions of any form are more than welcome! If something doesn't seem
right, and there is no issue about it yet, feel free to open one.

If you're looking to get involved, there are a few things you can do:

- Join the [urbit-dev][list] mailing list.
- [Ask us about Hoon School][mail], a course we run to teach the Hoon
  programming language and Urbit application development.
- Check out [good contributor issues][good].
- Reach out to [support@urbit.org][mail] to say hi and ask any questions you
  might have.

Once you've got your bearings, have a look at [CONTRIBUTING.md][cont] for some
pointers on setting up your development environment.

[list]: https://groups.google.com/a/urbit.org/forum/#!forum/dev
[mail]: mailto:support@urbit.org
[good]: https://github.com/urbit/urbit/labels/good%20contributor%20issue
[cont]: https://github.com/urbit/urbit/blob/master/CONTRIBUTING.md
