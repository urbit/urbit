Urbit
=====

Urbit is a clean-slate system software stack defined as a
deterministic computer. An encrypted P2P network, `%ames`, runs on a
functional operating system, Arvo, written in a strict, typed
functional language, Hoon, which compiles itself to a combinator
interpreter, Nock, whose spec gzips to 340 bytes.

What is this for? Most directly, Urbit is designed as a personal
cloud server for self-hosted web apps. It also uses HTTP APIs to
manage data stuck in traditional web applications.

More broadly, Urbit's network tackles identity and security problems
which the Internet can't easily address. Programming for a
deterministic single-level store is also a different experience from
Unix programming, regardless of language.

Install
=======

Urbit is designed to run on any Unix box with an internet connection.  Debian
(jessie), OS X, FreeBSD and Fedora all work well.  

Urbit is alpha software.  It’s not yet completely stable, its crypto hasn’t
been audited, and there are plenty of rough edges.  Urbit is lots of fun to play
with, but not quite ready for your important or sensitive data.

## Build from source

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

Which can usually be installed with the following one-liners:

    # Mac OS X [Homebrew]
    brew install git gmp libsigsegv openssl libtool autoconf automake cmake

    # Mac OS X [Macports]
    sudo port install git gmp libsigsegv openssl autoconf automake cmake

    # Ubuntu or Debian
    sudo apt-get install libgmp3-dev libsigsegv-dev openssl libssl-dev libncurses5-dev git make exuberant-ctags automake autoconf libtool g++ ragel cmake re2c

    # Fedora
    sudo dnf install gcc gcc-c++ git gmp-devel openssl-devel openssl ncurses-devel libsigsegv-devel ctags automake autoconf libtool ragel cmake re2c

    # FreeBSD
    pkg install git gmake gmp libsigsegv openssl automake autoconf ragel cmake re2c libtool

    # Arch
    pacman -S gcc gmp libsigsegv openssl automake autoconf ragel cmake re2c libtool ncurses

    # AWS
    sudo yum —enablerepo epel install gcc gcc-c++ git gmp-devel openssl-devel ncurses-devel libsigsegv-devel ctags automake autoconf libtool cmake re2c

### Setting up swap

Urbit wants to map 2GB of memory when it boots up.  We won’t
necessarily use all this memory, we just want to see it.  On a
normal modern PC or Mac, this is not an issue.  On some small
cloud virtual machines (Amazon or Digital Ocean), the default
memory configuration is smaller than this, and you need to
manually configure a swapfile.

Digital Ocean has a post on adding swap [here](https://www.digitalocean.com/community/tutorials/how-to-add-swap-on-ubuntu-14-04).  For Amazon there’s a StackOverflow thread [here](http://stackoverflow.com/questions/17173972/how-do-you-add-swap-to-an-ec2-instance).

Don’t spend a lot of time tweaking these settings; the simplest
thing is fine.

### Clone and make

Once your dependencies are installed the rest is easy:

    git clone https://github.com/urbit/urbit
    cd urbit
    make
    curl -o urbit.pill https://bootstrap.urbit.org/latest.pill

After running `make`, your Urbit executable lives at `bin/urbit`.  The `.pill` file is a compiled binary of Arvo that Urbit uses to bootstrap itself.

# Getting started

On disk your Urbit is an append-only log and a checkpoint.  Or, in simpler terms, a directory where we keep all of your Urbit’s state.  We call this a ‘pier’.

## Initialize

When you first start an Urbit we create this pier directory and write to it.

To start with a planet (`~fintud-macrep`) and ticket (`~fortyv-tombyt-tabsen-sonres`):

    bin/urbit -w fintud-macrep -t fortyv-tombyt-tabsen-sonres

This will create a directory `fintud-macrep/` and begin the initialization process for that planet.  Be patient, it can take a few minutes.  

Without a planet anyone can create a comet:

    bin/urbit -c comet

This will create a directory `comet/` and start up a random 64-bit comet.  You can specify any name you like.

## Orientation

When your Urbit is finished booting you should see a `dojo>` prompt.  Welcome!  

By default your Urbit is quite bare bones.  Let's take a very short stroll through what's running by default:

### Shell — `:dojo`

The command prompt in front of you is a simple shell and Hoon REPL.  Let's make sure it works:

    ~fitund-macrep:dojo> (add 2 2)

Should produce:

    > (add 2 2)
    4

Good.

### Messaging — `:talk`

Use `^X` to change your prompt.  Let's join the main Urbit chat channel:

    ~fitund-macrep:talk() ;join ~doznec/urbit-meta

This may take a bit as `:talk` is stateful so your Urbit will download some backlog.  `/urbit-meta` is the place to ask questions, get help and talk about anything Urbit related.

### Web — `:tree`

Urbit is also accessible through the web.  Try opening:

    http://localhost:8080/

in your favorite browser.  You'll be prompted to use `+code`.  This page was generated by `:tree`, the basic Urbit web interface.  You can also access your Urbit through `http://fintud-macrep.urbit.org` (where `fintud-macrep` is your planet name).  

### Filesystem — `%clay`

`%clay`, the Urbit filesystem, isn't mounted to unix by default.  Switch to the `:dojo` prompt and run:

    ~fitund-macrep:dojo> |mount %

This should produce:

    > |mount %
    >=

which indicates the command was processed.

`|mount %` will cause a `home/` directory to appear inside your pier folder.  Changes to these files are automatically synced into your Urbit.

Learn more
==========

If this is your first time using Urbit, and you've followed the instructions above, we'd recommend continuing the sequence [here](http://urbit.org/docs/using/admin/).

### Docs
[urbit.org/docs](http://urbit.org/docs/) is the public documentation site.  

The source is also in its own repo: [urbit/docs](http://github.com/urbit/docs).

### About

[urbit.org/posts](http://urbit.org/docs/) contains some essays on the theory and future of urbit.

### Keep in touch

- Check out the [urbit-dev](https://groups.google.com/forum/#!forum/urbit-dev) mailing list for long-form discussion.
- Follow [@urbit_](https://twitter.com/urbit\_) on Twitter.
- Email [urbit@urbit.org](mailto:urbit@urbit.org).  We're happy to answer questions and chat.


Code of conduct
===============

Everyone involved in the Urbit project needs to understand and
respect our code of conduct, which is: "don't be rude."


Contributing
============

The first step in contributing to urbit is to come and join us on
`:talk`.

For more detailed instructions check out
[`CONTRIBUTING.md`](https://github.com/urbit/urbit/blob/master/CONTRIBUTING.md).
