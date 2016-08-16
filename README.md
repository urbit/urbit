# Urbit

Urbit is a clean-slate system software stack defined as a deterministic
computer. An encrypted P2P network, `%ames`, runs on a functional
operating system, Arvo, written in a strict, typed functional language,
Hoon, which compiles itself to a combinator interpreter, Nock, whose
spec gzips to 340 bytes.

What is this for? Most directly, Urbit is designed as a personal cloud
server for self-hosted web apps. It also uses HTTP APIs to manage data
stuck in traditional web applications.

More broadly, Urbit's network tackles identity and security problems
which the Internet can't easily address. Programming for a deterministic
single-level store is also a different experience from Unix programming,
regardless of language.

## Getting involved

If you're interested in following Urbit, you can:

-   Read our documentation at [urbit.org](http://urbit.org/docs)
-   Subscribe to our newsletter at [urbit.org](http://urbit.org).
-   Check out the
    [urbit-dev](https://groups.google.com/forum/#!forum/urbit-dev)
    mailing list.
-   Follow [@urbit \_](https://twitter.com/urbit\_) on Twitter.
-   Email us: [`urbit@urbit.org`](mailto:urbit@urbit.org). We're happy
    to answer questions and chat.

## Code of conduct

Everyone involved in the Urbit project needs to understand and respect
our code of conduct, which is: "don't be rude."

# Install

Urbit can be installed on most Unix systems. There is no Windows port.
Windows is a wonderful OS, we just haven't gotten to it yet. Use a VM.

## Configure swap if needed

Urbit wants to map 2GB of memory when it boots up. We won't necessarily
use all this memory, we just want to see it. On a normal modern PC or
Mac, this is not an issue. On some small cloud virtual machines (Amazon
or Digital Ocean), the default memory configuration is smaller than
this, and you need to manually configure a swapfile.

To add swap to a DO droplet, [read
this](https://www.digitalocean.com/community/tutorials/how-to-add-swap-on-ubuntu-14-04).
To add swap on an Amazon instance, [read
this](http://stackoverflow.com/questions/17173972/how-do-you-add-swap-to-an-ec2-instance)

Don't spend a lot of time tweaking these settings; the simplest thing is
fine.

## Install as a package

<!--
waiting on makefile changes.
### OS X - Homebrew

    brew install --HEAD homebrew/head-only/urbit
-->

### Ubuntu or Debian

Third-party packages are available, at:

    https://github.com/yebyen/urbit-deb

Urbit is only supported on Jessie onward (but outbound HTTPS requests
only work on Stretch; I wish we knew why; help us!)

## Hand-build from source

First, install all external dependencies. Then, make.

### Dependencies

Urbit depends on:

    gcc (or clang)
    gmp
    libsigsegv
    openssl
    automake
    autoconf
    ragel
    cmake
    re2c
    libtool
    libssl-dev (Linux only)
    ncurses (Linux only)

#### Ubuntu or Debian

    sudo apt-get install libgmp3-dev libsigsegv-dev openssl libssl-dev libncurses5-dev git make exuberant-ctags automake autoconf libtool g++ ragel cmake re2c

#### Fedora

    sudo dnf install gcc gcc-c++ git gmp-devel openssl-devel openssl ncurses-devel libsigsegv-devel ctags automake autoconf libtool ragel cmake re2c

#### AWS

    sudo yum --enablerepo epel install gcc gcc-c++ git gmp-devel openssl-devel ncurses-devel libsigsegv-devel ctags automake autoconf libtool cmake re2c

#### OS X - Homebrew

    brew install git gmp libsigsegv openssl libtool autoconf automake cmake

#### OS X - Macports

    sudo port install git gmp libsigsegv openssl autoconf automake cmake

Although `automake`/`autoconf`/`libtool` are generally installed by
default, some have reported needing to uninstall and reinstall those
three packages, at least with Homebrew. Your mileage may vary.

#### FreeBSD

    pkg install git gmake gmp libsigsegv openssl automake autoconf ragel cmake re2c libtool

#### Archlinux

    pacman -S gcc gmp libsigsegv openssl automake autoconf ragel cmake re2c libtool ncurses

### Download and make

Clone the repo:

    git clone https://github.com/urbit/urbit.git

`cd` to the directory you just created:

    cd urbit

Run `make`:

    make

(On FreeBSD, use `gmake` instead.)

The executable is `bin/urbit`.

Fetch the latest bootstrapping pill:

    curl -o urbit.pill http://bootstrap.urbit.org/latest.pill

You can use your preferred way to fetch things from the web here. Urbit
looks for a pill named `urbit.pill` in the current directory by default,
so if you have anything different, you'll need to tell Urbit where it is
(on first launch only) with the `-B` command-line option.

# Start

An urbit is a persistent server on the `%ames` P2P network. You'll
create one of these servers now.

If you have an invitation, it's a planet like `~fintud-macrep` and a
ticket like `~fortyv-tombyt-tabsen-sonres`. Run

    urbit -w fintud-macrep -t fortyv-tombyt-tabsen-sonres

(You can leave the `~` on, but it annoys some Unix shells.)

If you don't have an invitation, pick a nickname for your comet, like
`mycomet`. Urbit will randomly generate a 128-bit plot:

    urbit -c mycomet

Either way, creating your urbit will take some time. Some of this time
involves creating keys; some of it involves downloading code over Urbit
itself. Go get a cup of coffee. Or a beer.

Wait until you see a prompt, something like

      ~fintud-macrep:dojo>

Your urbit is launched! Ladies and gentlemen, we are floating in space.

In either case you'll notice that we created a directory in unix called
either `mycomet` or `fintud-macrep`. All of your state (a log and
checkpoint) lives in that directory. You can move it around, but don't
delete it!

### Restarting

If you want to shut your Urbit down you can `ctrl-d` from the `dojo>`
prompt. To use Urbit normally after creating your planet or comet
there's no need for the `-w -t` or `-c` options.

Instead:

    urbit fintud-macrep

or

    urbit mycomet

### Continuity breaches / network reboots

Urbit is still young, and we reboot the network from time to time. Urbit
can upgrade itself over the air, but at this stage it's easier to bring
in breaking api changes this way. Since our crypto is stateful we call
this a 'continuity breach'.

If you have files in your Urbit you'd like to backup first make a copy
of your pier. Then you'll need to shut down your urbit (with `^d`) and:

    git pull origin master
    make clean; make
    curl -o urbit.pill http://bootstrap.urbit.org/latest.pill

Then start over at the top of this section.

# Talk

We use Urbit to talk to each other and coordinate about building the
system. To join the main Urbit talk station use `^x` to switch to the
`talk()` prompt, then run:

    ;join ~binzod/urbit-meta

# Learn more

Urbit's own official planet `~winsen-pagdel` hosts the public docs
[here](http://urbit.org/docs). The docs also live in their own repo,
[`urbit/docs`](http://github.com/urbit/docs).

If this is your first time using Urbit, we'd recommend starting
[here](http://urbit.org/docs/using/admin/).

# Contributing

The first step in contributing to urbit is to come and join us on
`:talk`.

For more detailed instructions check out
[`CONTRIBUTING.md`](https://github.com/urbit/urbit/blob/master/CONTRIBUTING.md).
