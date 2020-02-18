# Maintainers' Guide

## Hotfixes

Here lies an informal guide for making hotfix releases and deploying them to
the network.

Take [this PR][1], as an example.  This constituted a great hotfix.  It's a
single commit, targeting a problem that existed on the network at the time.
Here's it should be released and deployed OTA.

[1]: https://github.com/urbit/urbit/pull/2025

### If the thing is acceptable to merge, merge it to master

Unless it's very trivial, it should probably have a single "credible looking"
review from somebody else on it.

You should avoid merging the PR in GitHub directly.  Instead, use the
`sh/merge-with-custom-msg` script -- it will produce a merge commit with
message along the lines of:

```
Merge branch FOO (#PR_NUM)

* FOO:
  bar: ...
  baz: ...

Signed-off-by: SIGNER <signer@example.com>
```

We do this as it's nice to have the commit log information in the merge commit,
which GitHub's "Merge PR" button doesn't do (at least by default).
`sh/merge-with-custom-msg` performs some useful last-minute urbit-specific
checks, as well.

You might want to alias `sh/merge-with-custom-msg` locally, to make it easier
to use.  My .git/config contains the following, for example:

```
[alias]
        mu = !sh/merge-with-custom-msg
```

so that I can type e.g. `git mu origin/foo 1337`.

### Apply the changes to this era's release branch

For now, the release branch corresponds to the `vx.y` part of the most recent
Vere release (i.e., `urbit vx.y.z`).  At the time of writing, we're on v0.10
(and I'll use this branch as a running example):

If the branch doesn't yet exist, just create it via:

```
git checkout -b v0.10 master
```

If you can get away with merging master to v0.10 without pulling in any
superfluous or non-OTA-able commits, feel free to do that.  Otherwise, you'll
want to cherry pick the commits like so:

```
git cherry-pick -x TARGET_COMMITS
```

Use the `-x` flag to `git-cherry-pick`, because this will indicate in the
commit message where the things originally came from.

A useful technique is to cherry-pick merge commits on master directly.  Take
following commit, for example:

```
commit 769996d09
Merge: 171fcbd26 8428f0ab1
Author: Jared Tobin <jared@tlon.io>
Date:   Sun Feb 2 19:11:04 2020 +0400

    Merge branch 'liam-fitzgerald/langserver-doc-autocomplete' (#2204)

    * liam-fitzgerald/langserver-doc-autocomplete:
      language-server: magic-spoon hover, autocomplete
      language-server: build ford prelude
      language-server: dynamically compute subject
      language-server: revive rune/symbol completion
      language-server: add completion JSON parsers

    Signed-off-by: Jared Tobin <jared@tlon.io>
```

rather than cherry-picking the individual commits, one could just use the
following while on the release branch:

```
git cherry-pick -x -m 1 769996d09
```

you can check the man page for `git-cherry-pick(1)` for details here.

Create Landscape or alternative pill builds, if or as appropriate (i.e., if
anything in Landscape changed -- don't trust any compiled JS/CSS that's
included in the commit).

You should always create a solid pill, in particular, as it's convenient for
tooling to be able to boot directly from a given release.  If you're making a
Vere release, just play it safe and update all the pills.

### Tag the resulting commit

What you should do here depends on the type of release being made.

First, for Arvo releases:

If it's a very trivial hotfix that you know isn't going to break
anything, tag it as `arvo.yyyy.mm.dd`.  Use an annotated tag, i.e.

```
git tag -a arvo.yyyy.mm.dd
```

The tag format should look something like this:

```
arvo.yyyy.mm.dd

This release contains Arvo changes that will be pushed to the live
network as an over-the-air update.

Release notes:

  [..]

Contributions:

  [..]
```

You can get the "contributions" section by the shortlog between the
last release and this release:

```
git log --pretty=short LAST_RELEASE.. | git shortlog
```

I originally tried to curate this list somewhat, but now just paste it
verbatim.  If it's too noisy, yell at your colleagues to improve their commit
messages.

Try to include a high-level summary of the changes in the "release notes"
section.  You should be able to do this by simply looking at the git log and
skimming the commit descriptions (or perhaps copying some of them in verbatim).
If the commit descriptions are too poor to easily do this, then again, yell at
your fellow contributors to make them better in the future.

If it's *not* a trivial hotfix, you should probably make any number of release
candidate tags (e.g. `arvo.yyyy.mm.dd.rc1`, `arvo.yyyy.mm.dd.rc2`, ..), test
them, and after you confirm one of them is good, tag the release as
`arvo.yyyy.mm.dd`.

For Vere releases:

Tag the release as `vx.y.z`.  The tag format should look something
like this:

```
urbit vx.y.z

This release contains Vere changes, so users should update their
binaries.

This is not a breaching release, so users should not create new
piers.

Release notes:

  [..]

Contributions:

  [..]
```

The same schpeel re: release candidates applies here.

Do not include implicit Arvo changes in Vere releases.  This used to be done,
historically, but shouldn't be any longer.  If there are Arvo and Vere changes
to be released, make two releases.

### Deploy the update

For Arvo updates, this means copying the files into ~zod's %base desk.  The
changes will be synced to /~zod/kids and then propagated through other galaxies
and stars to the rest of the network.

For consistency, I download the release tarball and then rsync the files in:

```
$ wget https://github.com/urbit/urbit/archive/arvo.yyyy.mm.dd.tar.gz
$ tar xzf arvo.yyyy.mm.dd.tar.gz
$ herb zod -p hood -d "+hood/mount /=base="
$ rsync -zr --delete urbit-arvo.yyyy.mm.dd/pkg/arvo/ zod/base
$ herb zod -p hood -d "+hood/commit %base"
```

For Vere updates, this means simply shutting down each desired ship, installing
the new binary, and restarting the pier with it.

### Announce the update

Post an announcement to urbit-dev.  The tag annotation, basically, is fine here
-- I usually add the %base hash (for Arvo releases) and the release binary URLs
(for Vere releases).  Check the urbit-dev archives for examples of these
announcements.

