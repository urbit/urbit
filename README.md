urbit
=====

Urbit is a new computing environment designed from scratch.

At present urbit is under heavy development.  It's not useful for
any external purpose.  Documentation is completely inadequate.
Anyone can use the interpreter, of course, but the network is
invitation-only.

If you're interested in keeping in touch or following the project
you can:

-   Enter your email address at [urbit.org](http://urbit.org).
-   Subscribe to the
    [urbit-dev](https://groups.google.com/forum/#!forum/urbit-dev)
    mailing list.
-   Follow [@urbit_](https://twitter.com/urbit\_) on Twitter.
-   Get in touch with us via email, <urbit@urbit.org>

All of the source code is distributed under the MIT license.

Packages
--------

#### Ubuntu or Debian

Third-party package files are maintained at https://github.com/yebyen/urbit-deb. Urbit is only supported on Jessie onward.

#### OS X - Homebrew

    brew install --HEAD homebrew/head-only/urbit

Dependencies
------------
urbit depends on:

    gcc
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

*note: http requests are not supported on debian jessie
due to an ssl issue*

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

Build
-----

Clone the repo:

    git clone git://github.com/urbit/urbit.git

`cd` to the directory you just created:

    cd urbit

Just run `make`:

    make

(On FreeBSD, use `gmake` instead.)

Run (with a network invitation)
---

If you have a planet named `~fintud-macrep` and a ticket
`~fortyv-tombyt-tabsen-sonres`, run 

    bin/urbit -w fintud-macrep -t fortyv-tombyt-tabsen-sonres

Your pier (all Urbit state, log and checkpoint) will be in
`./fintud-macrep`.  To restart Urbit:

    bin/urbit fintud-macrep

Run (without a network invitation)
---

Run

    bin/urbit -c mypier

Urbit will create a comet in `mypier`.  To restart:

    bin/urbit mypier

Basics
------

`^v` will switch between the task manager and the focussed process. `^x`
will switch between processes.

To start a process that is not yet started, run `*proc` from the task
manager.

To connect your console to a process that has already been started, run
`+proc` from the task manager. Note that the process must be one that
supports console access, such as dojo and talk.

`^d` will exit the pier from the task manager. No matter how you shut
your urbit down you'll be returned to exactly the same state as when you
turned it off.

Talk
----

If you're on the network, you can use its most basic application, `talk`.

If `talk` is not running, start it with `*talk` from the task manager.

Use `^x` to switch to the `talk` prompt and then run

    ;join ~doznec/urbit-meta

to join the `urbit-meta` channel. People in there are helpful.

There are three main `talk` commands:

    ;join ~urbit-name/channel

`;join` subscribes your main feed to a remote channel.

    ;<number>

`;<number>` activates a previous message number, like a URL that got
clipped.

    ;<target>

`;<target>` sets the target for your messages, such as `;~urbit-name`
for a private message.

Filesystem Sync
---------------

Our filesystem, `%clay`, does not automatically sync to unix. If you want
to get files in and out of urbit, you'll need to setup a mount point.
Since each mount point is always watching for changes you may also want
to unmount from time to time.

The syntax is as follows (from `dojo`):

    |mount <clay-path> <mount-name>

    |unmount <mount-name>

or:

    |unmount <clay-path>

Sysadmin
--------

For the most part, updates to the system are automatically synced to
your urbit across the network.

When we make updates to the interpreter you should be able to shut down
your urbit and:

    git pull origin master
    make
    bin/urbit pier

where pier is the name of your pier.

While the network is still young from time to time we reboot the entire
universe. We call this a 'continuity breach' since we're breaking the
continuity of our crypto.

When this happens you'll need to back up your data and start a fresh
pier. Your original ticket will still work.

Contributing
------------

The first step in contributing to urbit is to come and join us on `:talk`. 

For more detailed instructions check out 
[`contributing.md`](https://github.com/urbit/urbit/blob/master/CONTRIBUTING.md),.
