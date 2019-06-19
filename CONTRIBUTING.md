# Contributing to Urbit

Thank you for your interest in contributing to urbit.

See [urbit.org/docs/getting-started][start] for basic orientation and usage
instructions.

[start]: https://urbit.org/docs/getting-started/#arvo

## Fake ships

You may have an identity on the live network, but doing all your development on
the live network would be cumbersome and unnecessary.  Standard practice in
urbit development is to work on a fake `~zod`.  Fake ships use deterministic
keys (derived from the ship address) and don't talk to the live network. They
can talk to each other over the local loopback.

To start a fake ship, simply specify the name with `-F`:

```
$ urbit -F zod
```

You can also pass a name for the *pier* (or ship directory):

```
$ urbit -F zod -c my-fake-zod
```

To resume a fake ship, just pass the name of the pier:

```
$ urbit my-fake-zod
```

## Git practice

Since we use the GitHub issue tracker, it is helpful (though not required) to
contribute via a GitHub pull request. If you already know what you are doing,
skip down to the Style section.

Start by cloning the repository on your work machine:

```
$ git clone https://github.com/urbit/urbit
```

And, additionally, fork the repository on GitHub by clicking the "Fork"
button. Add your fork as a remote:

```
$ git remote add [username] https://github.com/[username]/urbit
```

and set it as the default remote to push to:

```
$ git config --local remote.pushDefault [username]
```

This is good practice for any project that uses git. You will pull
upstream branches from urbit/urbit and push to your personal urbit fork
by default.

Next, start a new branch to do your work on.  Normally you'll want to use the
`master` branch as your starting point:

```
$ git checkout -b [branch name] master
```

Now you are free to do your work on this branch. When finished, you may
want to clean up your commits:

```
$ git rebase -i master
```

Then you can push to your public fork with `git push` and make a pull request
via the GitHub UI.

After your changes are merged upstream, you can delete your branch (via github
UI or `git push :[branch]` remotely, and with `git branch -d` locally).

## Style

The urbit project uses two-space indentation and avoids tab characters.
In C code, it should not be too difficult to mimic the style of the code
around you, which is just fairly standard K&R with braces on every
compound statement. One thing to watch out for is top-level sections in
source files that are denoted by comments and are actually indented one
level.

Hoon will be a less familiar language to many contributors. More details are
forthcoming; for now, the `%ford` vane (in [`sys/vane/ford.hoon`][ford]) is
some of the highest quality code in the kernel.

[ford]: https://github.com/urbit/arvo/blob/master/sys/vane/ford.hoon

## Kernel development

Working on either C or non-kernel Hoon should not bring any surprises, but the
Hoon kernel (anything under `sys/` in [urbit/arvo][arvo]) is bootstrapped from
a so-called *pill*, and must be recompiled if any changes are made. This should
happen automatically when you make changes, but if it doesn't, the command to
manually recompile and install the new kernel is `|reset` in `dojo`.  This
rebuilds from the `sys` directory in the `home` desk in `%clay`.

Currently, `|reset` does not reload apps like `dojo` itself, which will still
reference the old kernel. To force them to reload, make a trivial edit to their
main source file (under the `app` directory) in `%clay`.

[arvo]: https://github.com/urbit/arvo

## The kernel and pills

Urbit bootstraps itself using a binary blob called a pill (you can see it being
fetched from `bootstrap.urbit.org` on boot).  This is the compiled version of
the kernel (which you can find in the `sys` directory of [urbit/arvo][arvo]),
along with a complete copy of the `urbit/arvo` repository as source.

The procedure for creating a pill is often called "soliding." It is somewhat
similar to `|reset`, but instead of replacing your running kernel, it writes
the compiled kernel to a file. The command to solid is:

```
> .urbit/pill +solid
```

When the compilation finishes, your pill will be found in the
`[pier]/.urb/put/` directory as `urbit.pill`.

You can boot a new ship from your local pill with `-B`:

```
$ urbit -F zod -B path/to/urbit.pill my-fake-zod
```

Ordinarily, `https://bootstrap.urbit.org/latest.pill` will be updated
to match whatever's on the HEAD of `master` in the `urbit/arvo` repository.
Older pills are indexed by the first 10 characters of the `git` SHA1 of the
relevant commit, i.e. as `git-[sha1].pill`.  The continuous integration build
of the `urbit/arvo` repository uploads these pills for any successful build of
a commit or pull request that affects the `sys/` directory.

You can boot from one of these pills by passing the path to an Arvo working
copy with `-A` (and `-s` for *search*):

```
$ git clone https://github.com/urbit/arvo
$ urbit -F zod -A path/to/arvo -s my-fake-zod
```

## What to work on

If you are not thinking of contributing with a specific goal in mind, the
GitHub issue tracker is the first place you should look for ideas.  Issues are
occasionally tagged with a priority and a difficulty; a good place to start is
on a low-difficulty or low-priority issue.  Higher-priority issues are likely
to be assigned to someone -- if this is the case, then contacting that person
to coordinate before starting to work is probably a good idea.

There is also a "help wanted" tag for things that we are especially eager to
have outside contributions on. Check here first!

## Staying in touch

Questions or other communications about contributing to Urbit can go to
[support@urbit.org][mail].

[mail]: mailto:support@urbit.org

