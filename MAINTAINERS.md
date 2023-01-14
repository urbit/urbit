# Maintaining

## Overview

We use a three-stage release pipeline. Each stage of the release pipeline has
its own dedicated branch and corresponding testing moon. Features and bug fixes
progress through each stage--and are subject to testing along the way--until
they're eventually released to the live network. This pipeline automates our
release process, making it much easier to quickly and reliably ship code. It's
also simple to reason about.

## Branches and Moons

The branches and their corresponding moons that comprise the stages of the
release pipeline are:

----------------------------------------------------------------------------------------------
 Branch     |            Moon          |   Target audience   |            Contains
----------------------------------------------------------------------------------------------
`develop`   | `~binnec-dozzod-marzod`  | Kernel developers   | Latest `develop` branch commit
`release`   | `~marnec-dozzod-marzod`  | Early Adopters      | Latest `release` branch commit
`release`   | `~doznec-dozzod-marzod`  | App Developers      | Latest release candidate
`master`    | `~zod`                   | Everyone else       | Latest release

**WARNING**: If you lack the requisite skills to troubleshoot and fix kernel issues, you should not sync from develop/~binnec. If you're not prepared to breach your ship in response to an issue stemming from an early release, do not use pre-release moons.

`develop` is the default branch in the repo, which means that all new pull
requests target it by default. The general flow of a new feature or bug fix
through the pipeline is:

```console
feature branch ---->   develop ---->  release ---------> master
                          |              |                 |
                    deployed to    deployed to         deployed to
                    ~binnec moon ~marnec/~doznec moon  network
```

If an issue arises in the course of testing the `release` branch (because more
people are using `marnec` than `binnec`), a PR can be opened to target
`release`. If that's the case, the `master` needs to be merged back into
`develop` after `release` merges into `master` to ensure that `develop` gets the
fix.

## Release Workflow

Developers work on feature branches built against `develop`. While doing this,
they continually merge in changes from `develop` to their feature branch. When
their feature is ready (and they've tested it), they open a pull request. After
code review approval and passing tests, their feature can merge into `develop`.
Every merge into `develop` immediately triggers a deploy to the `binnec` moon.
If your merge breaks `binnec` it's your responsibility to fix it. 

Once a week on Tuesday, a `release` branch is cut off of `develop`. This release
gets deployed to `marnec` to be tested for the rest of the week. Any fixes that
have to go into the release can go straight into the release branch. New work
that didn't make the release continues on feature branches against `develop`
(eventually merging there). After initial testing on `marnec`, a release
candidate is tagged and merges into `~doznec` where early adopters and app
developers can pick it up and test or update their apps for a new kelvin. If
it's a new kelvin, we also send an email to urbit-dev with instructions for
testing the breaking changes.

Then on the next Tuesday the release branch merges into master and tagged using
the tag instructions below, we create a GitHub release (marked latest) using
that tag on `master` which documents the changes that went into the release. In
the Github UI you can get the changelog by selecting the tag prior to it from
the previous release when creating the new release. Then the release is deployed
to the broader network via `zod`. Master is then merged back into `develop`
where any fixes that went straight to release get picked up. Lastly, a new
release branch is cut from `develop` and the process begins again.

### Tagging

If it's a very trivial hotfix that you know isn't going to break anything, tag
it as `urbit-os-vx.y`.  Here 'x' is the major version and 'y' is an OTA patch
counter.

Use an annotated tag, i.e.

```
git tag -a urbit-os-vx.y
```

The tag format should look something like this:

```
urbit-os-vx.y

This release will be pushed to the network as an over-the-air update.

Release notes:

  [..]

Contributions:

  [..]
```

You can get the "contributions" section by the shortlog between the last release
and this release:

```
git shortlog --no-merges LAST_RELEASE..
```
