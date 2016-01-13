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

Getting involved
----------------

If you're interested in following Urbit, you can:

-   Read our documentation at [urbit.org](http://urbit.org/docs)
-   Subscribe to our newsletter at [urbit.org](http://urbit.org).
-   Check out the
    [urbit-dev](https://groups.google.com/forum/#!forum/urbit-dev)
    mailing list.
-   Follow [@urbit_](https://twitter.com/urbit\_) on Twitter.
-   Hit us up by email, `urbit@urbit.org`. We're nice!

Code of conduct
---------------

Everyone involved in the Urbit project needs to understand and
respect our code of conduct, which is: "don't be rude."

Documentation
=============

In-progress documentation can be found at
[urbit.org/docs](http://urbit.org/docs).

These docs ship with your urbit. If you're running one locally you
can access them at

    http://localhost:8080/home/docs

Assuming you're running on port 8080. The port is printed on startup.

Install
=======

Urbit can be installed on most Unix systems. There is no Windows
port. Windows is a wonderful OS, we just haven't gotten to it yet.
Use a VM.

Configure swap if needed
------------------------

Urbit wants to map 2GB of memory when it boots up.  We won't
necessarily use all this memory, we just want to see it.  On a
normal modern PC or Mac, this is not an issue.  On some small
cloud virtual machines (Amazon or Digital Ocean), the default
memory configuration is smaller than this, and you need to
manually configure a swapfile.

To add swap to a DO droplet:

https://www.digitalocean.com/community/tutorials/how-to-add-swap-on-ubuntu-14-04

To add swap on an Amazon instance:

http://stackoverflow.com/questions/17173972/how-do-you-add-swap-to-an-ec2-instance

Don't spend a lot of time tweaking these settings; the simplest
thing is fine.

Install as a package
--------------------

### OS X - Homebrew

    brew install --HEAD homebrew/head-only/urbit

### Ubuntu or Debian

Third-party packages are available, at:

    https://github.com/yebyen/urbit-deb

Urbit is only supported on Jessie onward (but outbound HTTPS requests only work on Stretch; I wish we knew why; help us!)

Hand-build from source
----------------------

First, install all external dependencies. Then, make.

### Dependencies

urbit depends on:

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

### Download and make

Clone the repo:

    git clone git://github.com/urbit/urbit.git

`cd` to the directory you just created:

    cd urbit

Run `make`:

    make

(On FreeBSD, use `gmake` instead.)

The executable is `bin/urbit`. Install it somewhere, or just use it
where it is.

Launch
======

An urbit is a persistent server on the `%ames` P2P network. You'll
create one of these servers now.

If you have an invitation, it's a planet like `~fintud-macrep` and a
ticket like `~fortyv-tombyt-tabsen-sonres`. Run

    urbit -w fintud-macrep -t fortyv-tombyt-tabsen-sonres

(You can leave the `~` on, but it annoys some Unix shells.)

If you don't have an invitation, pick a nickname for your comet, like
`mycomet`. Urbit will randomly generate a 128-bit plot:

    urbit -c mycomet

Either way, creating your urbit will take some time. Some of this
time involves creating keys; some of it involves downloading code
over Urbit itself. Go get a cup of coffee. Or a beer.

Wait until you see a prompt, something like

      ~fintud-macrep:talk()

or

      ~fintud-macrep:dojo>

Your urbit is launched! Ladies and gentlemen, we are floating in
space.

### Relaunch

To use Urbit normally after creating your planet or comet:

    urbit fintud-macrep

or

    urbit mycomet

Docs
====

Your urbit is your personal web server. The best place to read its
docs is by pointing your browser at it.

Urbit prints the HTTP port it's serving when it starts up:

    http: live (insecure) on 8080

8080 is the default. When it's not available we use 8081. Then 8082, and so on. If you're running on AWS or another cloud
service, this port may be firewalled; go to the firewall
configuration to open it.

(*Always run any urbit HTTP server which is even semi-serious inside a reliable, battle-proven frontline server like nginx.*)

Urbit's own official planet `~winsen-pagdel` is also bound to just
plain `urbit.org`, and hosts the public docs
[here](http://urbit.org/docs). (They are, of course, the same as
those that ship with your urbit.)

Assuming your Urbit is on `localhost:8080`, your copy of the docs are at

    http://localhost:8080/home/docs

To continue getting setup, start here:

    http://localhost:8080/home/docs/user/start

Contributing
============

The first step in contributing to urbit is to come and join us on
`:talk`.

For more detailed instructions check out
[`CONTRIBUTING.md`](https://github.com/urbit/urbit/blob/master/CONTRIBUTING.md).
