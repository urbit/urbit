# `rust-mixed-with-c`

This was an experiment to see what it looks like to combine C and Rust
in a single project, and then it turned into a full-blow nixification
of the all Urbit code.

Check out `.travis.yml` to see how this works in CI.

## Quick Start

```bash
curl https://nixos.org/nix/install | sh
  - nix-build
  - ./sh/cachix
  - nix-shell --pure --command ./sh/vere-tests
```

## Shared Build Caches

First, get `CACHIX_AUTH_TOKEN` from another dev, and then run the
following commands. You should only need to do this once.

```bash
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix authtoken "$CACHIX_AUTH_TOKEN" >/dev/null
cachix use urbit
```

In order to build everything an push results to the shared cache, run:

```bash
./sh/cachix
```

## Open Questions About Monorepo layout

- Should nix build scripts live *in* the packages
  (i.e. `pkg/urbit/default.nix`) or outside of it
  (nix/pkgs/urbit/default.nix)? What are the pros and cons of moving
  them into the packages?

  - PRO Setting up the `nix-shell` flow is slightly cleaner.
  - PRO Everything in one place, less fragmentation.
  - CON Nix build scripts become part of the build. Changes to the build
        scripts trigger rebuilds (or you can filter them out, but that adds
        a little bit of complexity to every build)
  - CON Packages get messier. Five additional files in the root of
        each package: `default.nix`, `release.nix`, `shell.nix`,
        `builder.sh`, `release.sh`.

## Open Questions About the Bootstrapping Pills

The pill that's used for bootstrapping in CI is stored in
`bin/pill/*.pill`. This doesn't need to be updated constantly. In my
mind, I'm imagining that it would get updated for each release, or
updated whenever we make a breaking change to the kernel. This deserves
further discussion.

## CI TODOs

- Save 14s by using the travis cache to cache the `cachix` install.

  https://github.com/travis-ci/travis-ci/issues/6604
