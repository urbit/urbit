# Contributing to urbit

Thank you for your interest in contributing to urbit.

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

Next, check out `test`, which is the mainline development branch, and
base a new branch on it to do your work on:

    git checkout -b [branch name] test

Now you are free to do your work on this branch. When finished, you may
want to clean up your commits:

    git rebase -i test

Then you can push to your public fork with `git push` and make a pull
request via the GitHub UI. Make sure you request to merge your branch
into `test`, not `master`.

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

## What to work on

If you are not thinking of contributing with a specific goal in mind,
the GitHub issue tracker is the first place you should look for ideas.
Issues are tagged with a priority and a difficulty. A good place to
start is on either a low-difficulty issue or a low-priority issue.
Higher priority issues are likely to be assigned to someone - if this is
the case, then contacting that person to coordinate before starting to
work is probably a good idea.

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
