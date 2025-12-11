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
| Branch    | Moon                    | Target Audience   | Contains                       |
|:---------:|:-----------------------:|:-----------------:|:------------------------------:|
| `develop` | `~binnec-dozzod-marzod` | Kernel Developers | Latest `develop` branch commit |
| `release` | `~marnec-dozzod-marzod` | Early Adopters    | Latest `release` branch commit |
| `release` | `~doznec-dozzod-marzod` | App Developers    | Latest release candidate       |
| `master`  | `~zod`                  | Everyone Else     | Latest release                 |

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

When we branch release to deploy to `~marnec`, we need to tag it as a release candidate (RC), like `urbit-os-vx.y-rc1`.  Here 'x' is the major version and 'y' is an OTA patch counter.  After this any change that goes into release gets a new tag that increments the rc.

After we ship a release to the live network, add a tag that is not a release candidate, like `urbit-os-vx.y`, to the master branch, since that's what was released.

#### Applying the Tag Locally

Use an annotated tag with the `-a` git argument.  Make sure to follow
the naming convention for RCs and live releases, described above.


To add a tag to the local repo, run this:

```
git tag -a <tagname>
```

This will bring up an editor, where you should add the release notes,
which should look like this:

```
<tagname>

This release will be pushed to the network as an over-the-air update.

Release notes:

  [..]

Contributions:

  [..]
```

To fill in the "contributions" section, copy in the shortlog between the last release and this release, obtained by running this command:

```
git shortlog --no-merges LAST_RELEASE..
```

#### Pushing the Tag to the Main Repo

Once you have added a tag, push it to the main repository using the
following command:

```
git push origin <tagname>
```

## Releases

- [ ] Create a pull request from the relevant release branch (with the format `release/urbit-os-vX.XX`) to `master`.
- [ ] ssh into `~zod` 
- [ ] Check to ensure that nobody else is ssh'd into `~zod`, by running `screen -ls` and verifying no sessions are attached.
- [ ] Attach to the screen session using `screen -x`
- [ ] Ensure that the release candidate was correctly propagated through the prerelease moons
  - Use `-read %z ~SHIP %DESK da+now /` to check desk hashes
  - [ ] Run the above check on the following [SHIP DESK] pairs: [~marnec-dozzod-marzod %base], [~marnec-dozzod-marzod %kids], [~doznec-dozzod-marzod %base], [~doznec-dozzod-marzod %kids] — **they should all match**
- [ ] Install the contents of the `%kids` desk on `~doznec-dozzod-marzod` into our `%base`: `|merge %base ~doznec-dozzod-marzod %kids, =gem %only-that`
- [ ] Check that `~zod` has updated to the latest release.  For a Kelvin release, you can run `zuse` in the Dojo.  Each non-Kelvin release might its own way of checking whether the update has completed, possibly through checking the `%cz` hash of the `%base` desk matches the hash on `~marnec-dozzod-marzod` by comparing the outputs of `+vat %base` on both ships.
- [ ] Merge `~zod`'s `%base` desk into its `%kids` desk to distribute the new code to the network: `|merge %kids our %base, =gem %only-that`
- [ ] Before exiting the screen session on `~zod`, make sure the screen session is not left in copy mode for a long period of time, since that will disrupt `~zod`'s operation.

### Release Communications

- [ ] Tag the commit that went onto the live network as a release, using GitHub's "Releases" interface.  See the "Tagging" section of this document for details.
- [ ] Update (add a response) the mailing list post to include the base hash of the new release, and indicate that this has now been deployed to the network.
- [ ] Tweet from the `@zodisok` Twitter account linking to the GitHub release.
- [ ] Post links to the release in the Urbit Community Development channel and a channel in the UF public group.

### Post-Release Git Cleanup

- [ ] Merge `master` back into `develop`.
- [ ] Cut a new release branch from `develop`.  The branch should have the format `release/urbit-os-vX.XX`

### Release Next Release Candidate

We'll now need to **IMMEDIATELY** deploy the new release candidate to the pre-release moon(s). Otherwise PRs merged during this window will bypass the testing period on `~binnec` and go straight to the release candidate.

- On `~marnec-dozzod-marzod`:
  - [ ] `|merge %base ~binnec-dozzod-marzod %kids, =gem %only-that` to update `~marnec` with the contents of the GH release branch
  - [ ] `|merge %kids our %base, =gem %only-that` to OTA the release candidate to subscribers
- When ready to deploy the release candidate to App Developers, on `~doznec-dozzod-marzod`:
  - [ ] `|merge %base ~marnec-dozzod-marzod %kids, =gem %only-that` to update `~doznec` with the release candidate
  - [ ] `|merge %kids our %base, =gem %only-that` to OTA the release candidate to subscribers

### Post-Release Checks
- [ ] Check that `~marzod` and other distribution stars are receiving the update by running `|ames-sift ~zod` and `|ames-verb %rcv %ges`.  You should see lots of packets from `~zod`.  Once you have confirmed packets are flowing, run `|ames-sift` and `|ames-verb` with no arguments to reset the verbosity state.
- [ ] Check that planets are receiving the update.  They should start updating within an hour or so.
- [ ] Monitor the Urbit Community Help channel, UF public group channels, and Twitter to make 

### Post-Release Artifacts
After waiting at least 24 hours after the release to the network, make and distribute a pill.
- [ ] Find a ship on the network (for now, use `~halbex-palheb`, which runs the UF public group) whose sources for `%base` and the standard app desks are mainline, not devstream.
- [ ] Ensure the `%cz` hashes of all desks match those on the distribution ships.
- [ ] Make a pill by running `.multi-vX-XX/pill +solid %base %garden %webterm %landscape %groups %talk` (replacing `X-XX` with the appropriate version numbers, in this and later steps).
- [ ] Boot a fakezod off that pill to make sure the pill is viable.
- [ ] Upload the pill to `bootstrap.urbit.org` using the Google Cloud SDK by running: `gsutil cp /path/to/pier/.urb/put/multi-vX-XX.pill gs://bootstrap.urbit.org/urbit-vX.XX.pill` -- note that it should be `vX.XX`, not `vX-XX` as in the original pill filename.
- [ ] Boot a ship with the latest binary and check that it downloads the pill from `https://bootstrap.urbit.org/urbit-vX.XX.pill` where you just uploaded it.
