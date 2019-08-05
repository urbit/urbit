# Contributing to Urbit

Thank you for your interest in contributing to Urbit.

If you're new, see [Getting Started](https://urbit.org/docs/getting-started) in the Urbit documentation for basic orientation and usage instructions.

Urbit development has two general concerns: the kernel and userspace development streams. Interested in [Arvo and its vanes](https://urbit.org/docs/learn/arvo/), [Vere](https://urbit.org/docs/learn/vere/), [or the Hoon language specification itself](https://github.com/urbit/urbit/blob/master/pkg/arvo/sys/hoon.hoon)? You'll be working with the kernel. Interested in building a Landscape or Gall app on Urbit? Then you're in userspace.

First, the basics of Urbit development practices.

### Using a fake (or "development") ship

You may have an identity on the live network, but doing all your development on the live network would be cumbersome and unnecessary.  Standard practice in Urbit development is to work on a fake `~zod`.  Fake ships (sometimes just called "development ships") use deterministic keys (derived from the ship address) and don't talk to the live network. They *can* talk to each other over the local loopback.

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

### Git practice

Since we use the GitHub issue tracker, it is helpful (though not required) to contribute via a GitHub pull request. If you already know what you are doing, skip down to the Style section.

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

### Style

The Urbit project uses two-space indentation and avoids tab characters. In C code, it should not be too difficult to mimic the style of the code around you, which is just fairly standard K&R with braces on every compound statement. One thing to watch out for is top-level sections in source files that are denoted by comments and are actually indented one
level.

Hoon will be a less familiar language to many contributors. You can get started learning it using our documentation [here](https://urbit.org/docs/learn/hoon/) or take lessons at [Hoon School](https://urbit.org/hoonschool/). If you're feeling intrepid, the `%ford` vane (in [`pkg/arvo/sys/vane/ford.hoon`][ford]) is some of the highest quality code in the kernel.

[ford]: https://github.com/urbit/urbit/blob/master/pkg/arvo/sys/vane/ford.hoon

## Kernel development

The state of an urbit is a pure function of its event log. As it must start somewhere, it's started with a bootstrap sequence. During this process the full Arvo instance is brought into operation, and the ship is 'booted'. This includes incorporating the Nock formula, then the Hoon compiler-as-Nock, on the Hoon compiler-as-source, on the Arvo kernel-as-source. From there, ship identity, entropy, Arvo vanes and the file system are also initialized.

We call the recipe for a bootstrap sequence a *pill*. Anything in the Hoon kernel (under [`pkg/arvo/sys/`][sys]) must be recompiled if any changes are made.

This should happen automatically when you make changes, but if it doesn't, the command to manually recompile and install the new kernel is `|reset` in `dojo`.  This rebuilds from the `sys` directory in the `home` desk in `%clay`.

Currently, `|reset` does not reload apps like `dojo` itself, which will still reference the old kernel. To force them to reload, make a trivial edit to their main source file (under the `app` directory) in `%clay`.

[arvo]: https://github.com/urbit/urbit/tree/master/pkg/arvo
[sys]: https://github.com/urbit/urbit/tree/master/pkg/arvo/sys

### The kernel and pills

The pill an Urbit ship bootstraps itself with (seen fetched from `bootstrap.urbit.org` on boot) is usually the *solid* pill, the compiled version of the kernel (which you can find in the `sys` directory of [Arvo][arvo]), along with a complete copy of the Arvo source.

The full sequence is called the *brass* pill. Both can be found in the [bin](https://github.com/urbit/urbit/tree/master/bin) directory. Pills are also cached in version control via [git LFS][git-lfs]. Ensure git LFS is set up for the repository:

```
$ git lfs init
$ git lfs pull
```

[git-lfs]: https://git-lfs.github.com

Pills are also cached at `https://bootstrap.urbit.org` and are indexed by the first 10 characters of the `git` SHA1 of the relevant commit, i.e. as `git-[sha1].pill`.  The continuous integration build uploads these pills for any successful build of a commit or pull request that affects the `pkg/arvo/sys/` directory.

You can boot from one of these pills by passing the path to an Arvo working copy with `-A` (and `-s` for *search*):

```
$ git clone https://github.com/urbit/urbit
$ urbit -F zod -sA urbit/pkg/arvo -c my-fake-zod
```

### Creating a pill

The procedure for creating a pill is often called "soliding." It is somewhat similar to `|reset`, but instead of replacing your running kernel, it writes the compiled kernel to a file. The command to solid is:

```
> .urbit/pill +solid
```

When the compilation finishes, your pill will be found in the `[pier]/.urb/put/` directory as `urbit.pill`.

You can boot a new ship from your local pill with `-B`:

```
$ urbit -F zod -B path/to/urbit.pill my-fake-zod
```

## Userspace development

Userspace has two parts: Landscape application development and Gall application development. Landscape lets you integrate your front-end web applications into a consistent user interface accessed through the browser. It also has a core set of applications that accept contributions.

Gall is the Arvo vane that controls userspace applications. More complicated Landscape applications will make extensive use of Gall, but it's not required if your Landscape application is not interacting with the ship.

Create a development ship, then once your ship is running, mount to Unix with `|mount %`. This will create a folder named 'home' in your pier in Unix. The 'home' desk contains the working state of your ship -- like a Git repository, when you want to make a change to it, `|commit %home`.

### Landscape

If you'd like to contribute to the core set of Landscape applications in this repository, clone this repository and start by creating an `urbitrc` file in the [pkg/interface](https://github.com/urbit/urbit/tree/master/pkg/interface) folder. You can find an `urbitrc-sample` there for reference. Then `cd` into the application's folder and `npm install` the dependencies, and `gulp watch` to watch for changes.

On your development ship, ensure you `|commit %home` to confirm your changes. Once you're done and ready to make a pull request, running `gulp bundle-prod` will make the production files and deposit them in [pkg/arvo](https://github.com/urbit/urbit/tree/master/pkg/arvo). Create a pull request with both the production files, and the source code you were working on in the interface directory.

#### Creating your own applications

If you'd like to create your own application for Landscape, the easiest way to get started is using the [create-landscape-app](https://github.com/urbit/create-landscape-app) repository template. It provides a brief wizard when you run it with `npm start`, and has good documentation for its everyday use -- just create a repo [using its template](https://github.com/urbit/create-landscape-app/generate), install and then start it, and you'll soon be up and running.

### Gall

Presently, Gall documentation is still in [progress](https://urbit.org/docs/learn/arvo/gall/), but a good reference. For examples of Landscape apps that use Gall, see the code for [Chat](https://github.com/urbit/urbit/blob/master/pkg/arvo/app/chat.hoon) and [Publish](https://github.com/urbit/urbit/blob/master/pkg/arvo/app/publish.hoon).


## What to work on

If you are not thinking of contributing with a specific goal in mind, the [GitHub issue tracker](https://github.com/urbit/urbit/issues) is the first place you should look for ideas.  Issues are occasionally tagged with a priority and a difficulty; a good place to start is
on a low-difficulty or low-priority issue.  Higher-priority issues are likely to be assigned to someone -- if this is the case, then contacting that person to coordinate before starting to work is probably a good idea.

There is also a "help wanted" tag for things that we are especially eager to have outside contributions on. Check here first!

## Staying in touch

Questions or other communications about contributing to Urbit can go to [support@urbit.org][mail].

[mail]: mailto:support@urbit.org

