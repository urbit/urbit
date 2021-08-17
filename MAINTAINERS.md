# Maintainers' Guide

## Branch organization

The essence of this branching scheme is that you create "release branches" of
independently releasable units of work.  These can then be released by their
maintainers when ready.

### Master branch

Master is what's released on the network.  Deployment instructions are in the
next section, but tagged releases should always come from this branch.

### Feature branches

Anyone can create feature branches.  For those with commit access to
urbit/urbit, you're welcome to create them in this repo; otherwise, fork the
repo and create them there.

Usually, new development should start from master, but if your work depends on
work in another feature branch or release branch, start from there.

If, after starting your work, you need changes that are in master, merge it into
your branch.  If you need changes that are in a release branch or feature
branch, merge it into your branch, but understand that your work now depends on
that release branch, which means it won't be released until that one is
released.

### Release branches

Release branches are code that is ready to release.  All release branch names
should start with `release/`.

All code must be reviewed before being pushed to a release branch.  Thus,
feature branches should be PR'd against a release branch, not master.

Create new release branches as needed.  You don't need a new one for every PR,
since many changes are relatively small and can be merged together with little
risk.  However, once you merge two branches, they're now coupled and will only
be released together -- unless one of the underlying commits is separately put
on a release branch.

Here's a worked example.  The rule is to make however many branches are useful,
and no more.  This example is not prescriptive, the developers making the
changes may add, remove, or rename branches in this flow at will.

Suppose you (plural, the dev community at large) complete some work in a
userspace app, and you put it in `release/next-userspace`.  Separately, you make
a small JS change.  If you PR it to `release/next-userspace`, then it will only
be released at the same time as the app changes.  Maybe this is fine, or maybe
you want this change to go out quickly, and the change in
`release/next-userspace` is relatively risky, so you don't want to push it out
on Friday afternoon.  In this case, put the change in another release branch,
say `release/next-js`.  Now either can be released independently.

Suppose you do further work that you want to PR to `release/next-userspace`, but
it depends on your fixes in `release/next-js`.  Simply merge `release/next-js`
into either your feature branch or `release/next-userspace` and PR your finished
work to `release/next-userspace`.  Now there is a one-way coupling:
`release/next-userspace` contains `release/next-js`, so releasing it will
implicitly release `release/next-js`.  However, you can still release
`release/next-js` independently.

This scheme extends to other branches, like `release/next-kernel` or
`release/os1.1` or `release/ford-fusion`.  Some branches may be long-lived and
represent simply the "next" release of something, while others will have a
definite lifetime that corresponds to development of a particular feature or
numbered release.

Since they are "done", release branches should be considered "public", in the
sense that others may depend on them at will.  Thus, never rebase a release
branch.

When cutting a new release, you can filter branches with `git branch --list
'release/*'` or by typing "release/" in the branch filter on Github.  This will
give you the list of branches which have passed review and may be merged to
master and released.  When choosing which branches to release, make sure you
understand the risks of releasing them immediately.  If merging these produces
nontrivial conflicts, consider asking the developers on those branches to merge
between themselves.  In many cases a developer can do this directly, but if it's
sufficiently nontrivial, this may be a reviewed PR of one release branch into
another.

### Non-OTAable release branches

In some cases, work is completed which cannot be OTA'd as written.  For example,
the code may lack state adapters, or it may not properly handle outstanding
subscriptions.  It could also be code which is planned to be released only upon
a breach (network-wide or rolling).

In this case, the code may be PR'd to a `na-release/` branch.  All rules are the
same as for release branches, except that the code does not need to apply
cleanly to an existing ship.  If you later write state adapter or otherwise make
it OTAable, then you may PR it to a release branch.

### Other cases

Outside contributors can generally target their PRs against master unless
specifically instructed.  Maintainers should retarget those branches as
appropriate.

If a commit is not something that goes into a release (eg changes to README or
CI), it may be committed straight to master.

If a hotfix is urgent, it may be PR'd straight to master.  This should only be
done if you reasonably expect that it will be released soon and before anything
else is released.

If a series of commits that you want to release is on a release branch, but you
really don't want to release the whole branch, you must cherry-pick them onto
another release branch.  Cherry-picking isn't ideal because those commits will
be duplicated in the history, but it won't have any serious side effects.


## Hotfixes

Here lies an informal guide for making hotfix releases and deploying them to
the network.

Take [this PR][1], as an example.  This constituted a great hotfix.  It's a
single commit, targeting a problem that existed on the network at the time.
Here's how it should be released and deployed OTA.

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

If you're making a Vere release, just play it safe and update all the pills.

For an Urbit OS release, after all the merge commits, make a release with the
commit message "release: urbit-os-v1.0.xx".  This commit should have up-to-date
artifacts from pkg/interface and a new solid pill.  If neither the pill nor the
JS need to be updated (e.g if the pill was already updated in the previous merge
commit), consider making the release commit with --allow-empty.

If anything in `pkg/interface` has changed, ensure it has been built and
deployed properly.  You'll want to do this before making a pill, since you want
the pill to have the new files/hash.  For most things, it is sufficient to run
`npm install; npm run build:prod` in `pkg/interface`.

However, if you've made a change to Landscape's JS, then you will need to build
a "glob" and upload it to bootstrap.urbit.org.  To do this, run `npm install;
npm run build:prod` in `pkg/interface`, and add the resulting
`pkg/arvo/app/landscape/index.[hash].js` to a fakezod at that path (or just create a
new fakezod with `urbit -F zod -B bin/solid.pill -A pkg/arvo`).  Run
`:glob|make`, and this will output a file in `fakezod/.urb/put/glob-0vXXX.glob`.

Upload this file to bootstrap.urbit.org, and modify `+hash` at the top of
`pkg/arvo/app/glob.hoon` to match the hash in the filename of the `.glob` file.
Amend `pkg/arvo/app/landscape/index.html` to import the hashed JS bundle, instead
of the unversioned index.js. Do not commit the produced `index.js` and
make sure it doesn't end up in your pills (they should be less than 10MB each).

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
git shortlog LAST_RELEASE..
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

Ensure the Vere release is marked as the 'latest' release and upload the two
`.tgz` files to the release as `darwin.tgz` and `linux64.tgz`;
this allows us to programmatically retrieve the latest release at
[urbit.org/install/mac/latest/](https://urbit.org/install/mac/latest) and
[urbit.org/install/linux64/latest](https://urbit.org/install/linux64/latest),
respectively.

The same schpeel re: release candidates applies here.

Note that the release notes indicate which version of Urbit OS the Vere release
will use by default when booting fresh ships.  Do not include implicit Urbit OS
changes in Vere releases; this used to be done, historically, but shouldn't be
any longer.  If there are Urbit OS and Vere changes to be released, make two
separate releases.

### Deploy the update

(**Note**: the following steps are automated by some other Tlon-internal
tooling.  Just ask `~nidsut-tomdun` for details.)

For Urbit OS updates, this means copying the files into ~zod's %home desk.  The
changes should be merged into /~zod/kids and then propagated through other galaxies
and stars to the rest of the network.

For consistency, I create a release tarball and then rsync the files in.

```
$ wget https://github.com/urbit/urbit/archive/urbit-os-vx.y.z.tar.gz
$ tar xzf urbit-os-vx.y.z.tar.gz
$ herb zod -p hood -d "+hood/mount /=home="
$ rsync -zr --delete urbit-urbit-os-vx.y.z/pkg/arvo/ zod/home
$ herb zod -p hood -d "+hood/commit %home"
$ herb zod -p hood -d "+hood/merge %kids our %home"
```

For Vere updates, this means simply shutting down each desired ship, installing
the new binary, and restarting the pier with it.

#### Continuous deployment

A subset of release branches are deployed continuously to the network. Thus far
this only includes `release/next-js`, which deploys livenet-compatible
JavaScript changes to select QA ships. Any push to master will automatically
merge master into `release/next-js` to keep the streams at parity.

### Announce the update

Post an announcement to urbit-dev.  The tag annotation, basically, is fine here
-- I usually add the %base hash (for Urbit OS releases) and the release binary
URLs (for Vere releases).  Check the urbit-dev archives for examples of these
announcements.
