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

### Prepare a release commit

You should create Landscape or alternative pill builds, if or as appropriate
(i.e., if anything in Landscape changed -- don't trust any compiled JS/CSS
that's included in the commit), and commit these in a release commit.

You should always create a solid pill, in particular, as it's convenient for
tooling to be able to boot directly from a given release.

If you're making a Vere release, just play it safe and update all the pills.

### Tag the resulting commit

What you should do here depends on the type of release being made.

First, for Urbit OS releases:

If it's a very trivial hotfix that you know isn't going to break
anything, tag it as `urbit-os-vx.y.z`.  Here 'x' refers to the product version
(e.g. OS1, OS2..), 'y' to the continuity era in that version, and 'z' to an
OTA patch counter.  So for a hotfix version, you'll just want to increment 'z'.

Use an annotated tag, i.e.

```
git tag -a urbit-os-vx.y.z
```

The tag format should look something like this:

```
urbit-os-vx.y.z

This release will be pushed to the network as an over-the-air update.

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
candidate tags (e.g. `urbit-os-vx.y.z.rc1`, `urbit-os-vx.y.z.rc2`, ..), test
them, and after you confirm one of them is good, tag the release as
`urbit-os-vx.y.z`.

For Vere releases:

Tag the release as `urbit-vx.y.z`.  The tag format should look something like
this:

```
urbit-vx.y.z

Note that this Vere release will by default boot fresh ships using an Urbit OS
va.b.c pill.

Release binaries:

(linux64)
https://bootstrap.urbit.org/urbit-vx.y.z-linux64.tgz

(macOS)
https://bootstrap.urbit.org/urbit-vx.y.z-darwin.tgz

Release notes:

  [..]

Contributions:

  [..]
```

The same schpeel re: release candidates applies here.

Note that the release notes indicate which version of Urbit OS the Vere release
will use by default when booting fresh ships.  Do not include implicit Urbit OS
changes in Vere releases; this used to be done, historically, but shouldn't be
any longer.  If there are Urbit OS and Vere changes to be released, make two
separate releases.

### Deploy the update

(**Note**: the following steps are automated by some other Tlon-internal
tooling.  Just ask `~nidsut-tomdun` for details.)

For Urbit OS updates, this means copying the files into ~zod's %base desk.  The
changes will be synced to /~zod/kids and then propagated through other galaxies
and stars to the rest of the network.

For consistency, I create a release tarball and then rsync the files in.

```
$ wget https://github.com/urbit/urbit/archive/urbit-os-vx.y.z.tar.gz
$ tar xzf urbit-os-vx.y.z.tar.gz
$ herb zod -p hood -d "+hood/mount /=base="
$ rsync -zr --delete urbit-urbit-os-vx.y.z/pkg/arvo/ zod/base
$ herb zod -p hood -d "+hood/commit %base"
```

For Vere updates, this means simply shutting down each desired ship, installing
the new binary, and restarting the pier with it.

### Announce the update

Post an announcement to urbit-dev.  The tag annotation, basically, is fine here
-- I usually add the %base hash (for Urbit OS releases) and the release binary
URLs (for Vere releases).  Check the urbit-dev archives for examples of these
announcements.

