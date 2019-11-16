# Changelog

## v0.2.3 (2019-04-29)

Added:

- Add flag `--no-ensure-executables` to stop `stack2nix` from ensuring
  necessary executables exist #164
- Allow extra cabal2nix flags #156

Bug Fixes:

- When writing nix expressions to file (`-o` flag), force UTF-8 encoding #164

## v0.2.2 (2019-01-17)

Bug fixes:

- LTS-13 compatibility
- Proper fix for bin-package-db #118
- Add package flags #148
- Upgrade stack to 1.9 #133

## v0.2.1 (2018-09-04)

Bug fixes:

- null bin-package-db for GHC 7.10 #118
- Bump cabal2nix and remove upper bound #120
- Parse mac operating system as osx or darwin #123
- Don't use `src = ./.` #121
- Pass --compiler to calls to cabal2nix #115

## v0.2 (2018-07-24)

Major changes:

- Use full stackage snapshot instead of relying on the build plan #83
- Get rid of hnix and rely on Derivation type from cabal2nix
- Use nix to provision executables if missing #83
- Use GHC version that belongs to the LTS #84
- ghc-options in stack.yaml are now passed to generated Nix exprs #96
- Support --bench #97

Other enhancements:

- Support --platform to set targeting system generation #79
- Use cabal2nix and stack as haskell libraries instead of relying on executable parsing #75
- Add --verbose flag #78
- Be able to pin down hackage snapshot #75
- Optimize cabal2nix calls by reusing HackageDB #75
- Rewrite tests in hspec to reduce dependencies #83
- Make stack.yaml filename configurable #90
- Add option to disable indentation #89
- When cloning git, also checkout submodules #108

Bug fixes:

- Be able to override GHC core packages #51
- Cleanup concurrency #33
- Add --haddock #38
- Add --test #35
- Support Stack subdirs #10
- Correct version parsing #67
- Silence git stdout output not to leak into Nix #91

## v0.1.3.0 (2017-07-27)

Bug fixes:

- Apply only Nix overrides without version fixes #26

## v0.1.2.0 (2017-06-22)

Bug fixes:

- Minor stack2nix.cabal improvements

## v0.1.1.0 (2017-06-22)

Initial public release.
