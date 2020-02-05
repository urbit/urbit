# Maintainers' Guide

## Hotfixes

Here lies an informal guide for making hotfix releases and deploying them to
the network.

Take [this recent PR][1], as an example.  This constituted a great hotfix.
It's a single commit, targeting a problem that existed on the network at the
time.  Here's it should be released and deployed OTA.

[1]: https://github.com/urbit/urbit/pull/2025

### If the thing is acceptable to merge, merge it to master

Unless it's very trivial, it should probably have a single "credible looking"
review from somebody else on it.

You can just merge the PR in GitHub.  As I, `~nidsut-tomdun`, am a l33t
h4x0r, I use a custom merge commit format, gotten by:

```
git merge --no-ff --signoff --log BRANCH
```

with the commit message:

```
Merge branch FOO (#PR_NUM)

* FOO:
  bar: ...
  baz: ...

Signed-off-by: Jared Tobin <jared@tlon.io>
```

All this extra wankery is hardly required, but IMO it's nice to have the
commit log information in the merge commit, which GitHub's "Merge PR" button
doesn't do (at least by default).

The script at `sh/merge-with-custom-message` can be used to make this simple(r)
to do.  I use `git mu` as an alias for it, locally.

### Apply the changes to this era's release branch

This corresponds to the 'vx.y' part of the most recent 'urbit vx.y.z' release.
At the time of writing, we're on v0.10 (and I'll use this branch as a running
example):

If the branch doesn't yet exist, just create it via:

```
git checkout -b v0.10 master
```

If you can get away with merging master to v0.10 without pulling in any
superfluous commits, feel free to do that.  Otherwise, you'll want to cherry
pick the commits like so:

```
git cherry-pick -x TARGET_COMMITS
```

Use the `-x` flag to `git-cherry-pick`, because this will indicate in the
commit message where the things originally came from.

Create Landscape or alternative pill builds, if or as appropriate (i.e., if
anything in Landscape changed -- don't trust the compiled JS/CSS that's
included in the commit).

You may also want to create a brass pill, in particular, as it's convenient for
tooling to be able to boot directly from a given release.

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
git log --pretty=short --no-merges \
  LAST_RELEASE..v0.10 | git shortlog
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
candidate tags (e.g. `arvo.yyyy.mm.dd.rc-1`, `arvo.yyyy.mm.dd.rc-2`, ..), test
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

You should probably avoid putting both Arvo and Vere changes into Vere
releases.

### Deploy the update

For Arvo updates, this means copying the files into ~zod's %base desk.  For
consistency, I download the release tarball and then rsync the files in:

```
$ wget https://github.com/urbit/urbit/archive/arvo.yyyy.mm.dd.tar.gz
$ tar xzf arvo.yyyy.mm.dd.tar.gz
$ herb zod -p hood -d "+hood/mount /=base="
$ rsync -zr --delete urbit-arvo.yyyy.mm.dd/pkg/arvo/ zod/base
$ herb zod -p hood -d "+hood/commit %base"
```

For Vere updates, this means shutting down each desired ship, installing the
new binary, and restarting the pier with it.

### Announce the update

Post an announcement to urbit-dev.  The tag annotation, basically, is fine here
-- I usually add the %base hash (for Arvo releases) and the release binary URLs
(for Vere releaes).  Check the urbit-dev archives for examples of these
announcements.

