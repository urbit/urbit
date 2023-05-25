## Workflow

Before beginning any unit of work, you should have a GitHub issue detailing the
scope of the work. This could be an issue someone else filed and has been
assigned to you (or you've assigned to yourself) or a new issue you filed
specifically for this unit of work. As much as possible, discussion of the work
should take place in the issue. When this is not possible, please update the
issue with relevant details from any offline conversations. Each issue should
provide a clear and thorough history of the work from inception to completion.

## Issues

The [GitHub tracker][issu] is our canonical source of truth around issues, bugs,
performance problems and feature requests.  If you encounter any issues when
developing on Urbit, feel free to submit a report about it here.

A good bug report, description of a crash, etc., should ideally be
*reproducible*, with clear steps as to how another developer can replicate and
examine your problem.  That said, this isn't always possible -- some bugs depend
on having created a complicated or unusual state, or can otherwise simply be
difficult to trigger again.

Your issue should thus at a minimum be *informative*.  The best advice here is
probably "don't write bad issues," where "bad" is a matter of judgment and
taste.  Issues that the maintainers don't judge to be sufficiently useful or
informative may be closed.

Feature requests are welcome, but they should include sufficient detail and
explanation, as well as a discussion of perceived benefits one could expect from
them.  "It would be cool if.." probably does not, in itself, constitute a good
feature request; instead, try to be specific about what you're requesting, and
what your desired feature would accomplish.

### Commits

Commits should generally be relevant, atomic, and have descriptions formatted in
the following manner:

>  component: short description
>
>  long description

The 'component' is a short prefix of what area of the codebase the commit
applies to.  If a commit patches `%gall`, for example, the description should be
prefixed by 'gall'.  If it touches `:aqua`, it should be prefixed by 'aqua'. If
it touches multiple components, then separate these by commas, e.g. "gall, aqua,
ph" -- but note that this may be a warning that too many changes are being
packed into a single commit.  The 'component' and 'short description' combined
should be no more than 50 characters.

Every individual commit should at a minimum be in a compiling and runnable state.
Broken commits or commits simply marked "wip" are not allowed. If you need to
clean up the commits in your branch, you can soft reset to an earlier state and
recommit with better metadata (or if the change is small enough, squash to one
good commit at the end). 

A lengthier description is encouraged, but is not always strictly required.  You
should use the longer description to give any useful background on or motivation
for the commit, provide a summary of what it does, link to relevant issues,
proposals, or other commits, and so on.

Here is an example of our commit format, taken from a commit in the history:

>  zuse: remove superfluous 'scup' and 'culm' types.
>
>  %zuse includes definitions for 'scup' and 'culm', both of which are
>  superfluous.  'scup' is simply (pair ship desk) and is used only in the
>  definition of 'culm', a tagged union in which three of the four branches are
>  commented out (i.e. are unused).
>
>  This commit deletes 'scup' and 'culm' and refactors what little code made use
>  of them.

Note that the short description is prefixed by `zuse:`, which is what the commit
touches.  Otherwise it just includes a summary of the change.

Here's another example:

>  build: give arvo a high priority
>
>  0bdced981e4 introduced the 'arvo-ropsten' derivation.  Attempting to install
>  both 'arvo' and 'arvo-ropsten' via nix-env will result in a priority error;
>  this assigns a higher priority to 'arvo' to resolve the conflict.
>
>  Fixes #1912.

Note that it cites a previous relevant commit, `0bdced981e4`, in its summary,
and also points at the issue that it resolves.

When we say commits should be "atomic", we mean with respect to some distinct
logical unit, e.g. a type definition used across many files, or a single file,
or just a single function in a single file.  Commits should be atomic at the
level of *code*, not of entire features.  You don't have to squash your commits
into a single one that captures everything you're trying to do -- the history
will never make for pleasant bedtime reading, so focus instead on making your
commits useful for tools like `git-blame` and `git-bisect`.

Your contribution must apply cleanly to `develop` in order to be considered
mergeable.  You may want to regularly [rebase your changes][reba] onto
`develop` in order to both clean up any intermediate "development" commits you
make and to ensure that you're up to date.

### Pull Requests and Merges

When your work is ready for review, open a pull request, making sure to link to
the tracking issue in the description, which should be formatted as follows
(where `<N>` is the number of this work's tracking issue):

```
### Description

Resolves #<N>.

Thoroughly describe the changes made.

### Related

Reference any related issues, links, papers, etc. here.
```

Tests will run automatically via GitHub Actions when you open a pull request or
push new commits to an existing pull request.

Once you've collected and addressed feedback, tests are passing, and your PR has
been approved, merge the pull request.

**Note**: If you are merging into develop, you *must* be syncing OTAs from
`~binnec-dozzod-marzod` which gets the tip of develop deployed to it. If
your merge breaks `binnec` it's your responsibility to alert people and
fix it. Your PR is shipped when it's successfully been deployed to 
`~binnec` and picked up by your personal ship. If you're merging on behalf
of an external developer, this is also your responsibility.

If you properly included the "Resolves
#N." directive in the pull request description, merging will automatically close
the tracking issue associated with the pull request.

## Code style

Hoon will be a less familiar language to many contributors.  We've published
some [style guidelines for Hoon][hoon], but above all you should try to mimic
the style of the code around you.  With regards to the style used throughout the
codebase: the more recently the code was written, the more standard and accepted
its style is likely to be.

### Kernel Development and Pills

Urbit bootstraps itself from a pill (you can see it being fetched from
`bootstrap.urbit.org` on boot).  This is the compiled version of the kernel
(which you can find in the `sys` directory of [Arvo][arvo]), along with a
complete copy of the Arvo source.

You can find the latest solid pill, as well as the latest so-called *brass* 
and *ivory* pills, in the `bin/` directory at the repository root.

Any contribution that touches the kernel (i.e., anything in `pkg/arvo/sys`),
should be accompanied by an updated [solid pill](#the-kernel-and-pills).  Pills
are tracked in the repository via [git LFS][git-lfs].

```
$ git lfs install
$ git lfs pull
```

[git-lfs]: https://git-lfs.github.com

The +solid command is used to write the compiled kernel to a file.

```
> .urbit/pill +solid
```

When the compilation finishes, your pill will be found in the `[pier]/.urb/put/`
directory as `urbit.pill`.

You can boot a new ship from your local pill with `-B`:

```
$ urbit -F zod -B path/to/urbit.pill -c my-fake-zod
```

Release pills, i.e. those corresponding to vere releases, are cached at
`https://bootstrap.urbit.org` and are indexed by the vere version number, e.g.
`urbit-0.8.2.pill`.

Whenever you make a contribution to the kernel, please create a new solid pill
via:

```
sh/update-solid-pill
```

You should include the updated pill in the same commit that updates the source.

## Development Environment

Although you likely have an identity on the live network, developing on the live
network is high-risk and largely unnecessary. Instead, standard practice is to
work on a fake ship. Fake ships use deterministic keys derived from the ship's
address, don't communicate on the live network, and can communicate with other
fake ships over the local loopback.

### Boot a New Fake Ship

To boot a new fake ship, pass the `-F` flag and a valid Urbit ship name to
`urbit`:

```console
$ bazel build :urbit
$ ln -s bazel-bin/pkg/vere/urbit urbit
$ ./urbit -F <ship>
```

By default, booting a fake ship will use the same pill that livenet ships use,
which leads to a non-trivial boot time on the order of tens of minutes. However,
using a development specific "solid" pill reduces this time to a couple minutes.

To boot using the solid pill, download or create one as described in the Kernel
Development and Pills section above and then run the following:

```console
$ ./urbit -F <ship> -B solid.pill
```

### Launch an Existing Fake Ship

To launch an existing fake ship, supply the pier (the ship directory), which is
simply the name of the ship[^1], to `urbit`:

```console
$ ./urbit <ship>
```

[^1]: Unless you specified the pier name using the `-c` flag.


[list]: https://groups.google.com/a/urbit.org/forum/#!forum/dev
[repo]: https://github.com/urbit/urbit
[reba]: https://git-rebase.io/
[issu]: https://github.com/urbit/urbit/issues
[hoon]: https://urbit.org/docs/hoon/reference/style
[main]: https://github.com/urbit/urbit/tree/master/pkg/arvo#maintainers
