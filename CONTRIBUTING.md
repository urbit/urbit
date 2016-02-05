# Contributing to urbit

Thank you for your interest in contributing to urbit.

## Development practice

You may have an identity on the live network, but doing all your
development on the live network would be cumbersome and unnecessary.
Standard practice in urbit development is to work on a fake `~zod`. A
fake `~zod` will get its initial files from the `urb/zod/` directory
rather than trying to sync them over the network, which is invaluable
for working in Hoon. Also, a fake `~zod` or any fake urbit instances you
start do not talk to the live network, but to a fake network that exists
only on your computer.

To start a fake `~zod`, the command is:

    bin/urbit -F -I zod -c [pier directory]

To resume one that was already created, just as on the live network,
remove `-c` (but leave the rest of the options there). `-F` uses the
fake network, and `-I` starts an "imperial" instance - that is, an 8-bit
galaxy.

## Kernel development

Working on either C or non-kernel Hoon should not bring any surprises,
but the Hoon kernel (anything under `urb/zod/arvo/`) is bootstrapped
from `urbit.pill` in `urb/`, and must be manually recompiled if any
changes are made. The command to manually recompile the kernel and
install the new kernel is `|reset` in `dojo`.  This rebuilds from the
`arvo` directory in the `home` desk in `%clay`.  Currently, `|reset`
does not reload apps like `dojo` itself, which will still reference the
old kernel. To force them to reload, make a trivial edit to their main
source file (under the `app` directory) in `%clay`.

If you do any kernel development, be sure to read the section below about
pills.

## Git practice

Since we use the GitHub issue tracker, it is helpful to contribute via a
GitHub pull request. If you already know what you are doing, skip down
to the Style section.

Start by cloning the repository on your work machine:

    git clone https://github.com/urbit/urbit

And, additionally, fork the repository on GitHub by clicking the "Fork"
button. Add your fork as a remote:

    git remote add [username] https://github.com/[username]/urbit

and set it as the default remote to push to:

    git config --local remote.pushDefault [username]

This is good practice for any project that uses git. You will pull
upstream branches from urbit/urbit and push to your personal urbit fork
by default.

Next, check out `master`, which is the mainline development branch, and
base a new branch on it to do your work on:

    git checkout master
    git checkout -b [branch name]

Now you are free to do your work on this branch. When finished, you may
want to clean up your commits:

    git rebase -i master

Then you can push to your public fork with `git push` and make a pull
request via the GitHub UI.

After your changes are merged upstream, you can delete your branch (via
github UI or `git push :[branch]` remotely, and with `git branch -d`
locally).

## Style

The urbit project uses two-space indentation and avoids tab characters.
In C code, it should not be too difficult to mimic the style of the code
around you, which is just fairly standard K&R with braces on every
compound statement. One thing to watch out for is top-level sections in
source files that are denoted by comments and are actually indented one
level.

Hoon will be a less familiar language to many contributors. Some of our
less obvious stylistic rules are:

-   Keep your source files 80 characters or less wide. Many urbit
    developers use 80 character terminals/tmux panes/&c.
-   Tab characters are actually a syntax error, so be extra sure your
    editor is not inserting any. Trailing whitespace is *usually* not a
    syntax error, but avoiding it is encouraged.
-   The kernel convention is that line comments start at column 57 with
    the `::` followed by 2 spaces. This leaves 20 characters for the
    comment.  Outside the kernel, things are less strict.
-   Tall arms within a core are conventionally separated by empty comments
    (just `::`) at the same indentation level as the initial `++` or `+-`.
    The last arm in a core is not followed by an empty comment, because it
    is visually closed by the `--` that closes the core. The empty comment
    is also sometimes omitted in data structure definitions.

## The kernel and pills

urbit bootstraps itself using a binary blob called `urbit.pill`, which
we do indeed keep in version control. This creates some special
requirements. If you are not changing anything in the kernel (everything
under `urb/zod/arvo/`) then you can skim this section (please do not
skip it entirely, though).  If you *are* working there, then this
section is critically important!

The procedure for creating `urbit.pill` is often called "soliding". It
is somewhat similar to `|reset`, but instead of replacing your running
kernel, it writes the compiled kernel to a file. The command to solid
is, on a fakezod:

    .urbit/pill +solid

When the compilation finishes, your `urbit.pill` will be found in the
`[pier]/.urb/put/` directory. Copy it into `urb/` and add it to your
commit.

The requirement here is that every commit that changes the kernel must
come with an `urbit.pill` built from the same code in `urb/zod/arvo/`
for that commit. (Only changing the actual Hoon code counts, so a change
to a jet with no corresponding Hoon change does not require a new pill.)
This is so that checking out an arbitrary revision and starting up a
fakezod actually works as expected. However you do this is fine, but I
like to do it as part of my committing process - just before `git
commit`, I fire up a new fakezod. This will use the previous
`urbit.pill`, but the kernel code in `%clay` will be copied from
`urb/zod/arvo/`, so `+solid` will compile it. Then I copy `urbit.pill`
into `urb/` and make my commit.

If you rebase or interactive rebase your commits, be sure to preserve
this property on all the commits you end up with. If multiple people
were collaborating on your branch, you may end up with conflicts in
`urbit.pill` and have to merge the branch into itself to resolve them.
Just do the same procedure to create a new, merged pill before
committing the merge. Otherwise, just make sure to use the correct
`urbit.pill` for each commit.

## Debug urbit with `gdb`

Follow the build instructions in README.md but run `make` with argument `DEBUG=yes`:

(If you've already built urbit first run `make clean`.)

    make DEBUG=yes

Run `gdb`, while loading `bin/urbit` and its symbol table:

    gdb bin/urbit

Set a breakpoint on `main()` (optional):

    break main

Run your urbit comet `mycomet`:

    run mycomet

Continue from the breakpoint on `main()`:

    continue

## What to work on

If you are not thinking of contributing with a specific goal in mind,
the GitHub issue tracker is the first place you should look for ideas.
Issues are tagged with a priority and a difficulty. A good place to
start is on either a low-difficulty issue or a low-priority issue.
Higher priority issues are likely to be assigned to someone - if this is
the case, then contacting that person to coordinate before starting to
work is probably a good idea.

There is also a "help wanted" tag for things that we are especially
eager to have outside contributions on. Check here first!

## Staying in touch

The urbit developers communicate on urbit itself. Joining the
`~doznec/urbit-meta` channel on `talk` is highly recommended.
Subscribing to `urbit-dev` on Google Groups is also recommended, since
this is where continuity breach notifications are sent.

You can also contact one of the following people:

-   Philip Monk

    email: philip.monk@tlon.io

    urbit: `~wictuc-folrex`

    GitHub: [@philipcmonk](https://github.com/philipcmonk/)

-   Raymond Pasco

    email: ray@the.ug

    urbit: `~ramtev-wisbyt`

    GitHub: [@juped](https://github.com/juped/)
