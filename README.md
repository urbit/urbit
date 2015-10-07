Urbit
=====

Urbit is a new computing environment designed from scratch.

At present urbit is under heavy development. It's not useful for any
external purpose. Documentation is completely inadequate. Anyone can use
the interpreter, of course, but the network is invitation-only.

If you're interested in keeping in touch or following the project you
can:

-   Enter your email address at [urbit.org](http://urbit.org).
-   Subscribe to the
    [urbit-dev](https://groups.google.com/forum/#!forum/urbit-dev)
    mailing list.
-   Follow [@urbit_](https://twitter.com/urbit\_) on Twitter.
-   Get in touch with us via email, <urbit@urbit.org>

All of the source code is distributed under the MIT license.

Install from packages
--------------------

#### OS X - Homebrew

    brew install --HEAD homebrew/head-only/urbit

#### Ubuntu or Debian

Third-party package files are maintained at
https://github.com/yebyen/urbit-deb. 

Urbit is only supported on jessie onward (but outgoing HTTPS
requests only work on stretch).

Install by hand
---------------

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

*note: http requests are not supported on debian jessie due to an ssl
issue*

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
-------------------------------

Your invitation is a planet (32-bit urbit), with a name like
`~fintud-macrep`, and a ticket (sekrit code), like
`~fortyv-tombyt-tabsen-sonres`.  Run

    bin/urbit -w fintud-macrep -t fortyv-tombyt-tabsen-sonres

Your pier (all Urbit state, log and checkpoint) will be in
`./fintud-macrep`.  You can move it, and it's portable.  Doing
`rm -r fintud-macrep/.urb/chk` will delete the checkpoint,
meaning all your events need to be recomputed, but making the
image smaller.

To quit Urbit (without destroying any data, since Urbit is a
database): ^D.

To start ~fintud-macrep again, omit the `-w` and `-t` flags:

    bin/urbit fintud-macrep

Run (without a network invitation)
----------------------------------

    bin/urbit -c mypier

Urbit will create a comet (128-bit urbit) in `mypier`.

To quit Urbit (without destroying any data, since Urbit is a
database): ^D.  Note that you can kill your urbit process as
nastily as you want, 

To start your comet again, omit the `-c` flag:

    bin/urbit mypier

Getting started 
---------------

Your urbit is a Web server, so the best place to read about it 
is in your browser.

Urbit prints the HTTP port it's serving when it starts up:

    http: live (insecure) on 8080

8080 is the default.  If you're running on AWS or another cloud
service, this port may be firewalled; go to the firewall
configuration to open it.

Learn these two control keys first:  
Press ^v 

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

If you're on the network, you can use its most basic application,
`talk`.

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

The Urbit filesystem, `%clay` doesn't automatically sync to unix. If
you'd like to browse the contents of your Urbit from unix you'll need to
use `|mount`.

`|mount` creates a mirror in unix of files from `%clay` that is always
watching for changes. To sync your entire `home/` desk run:

    |mount %

you should see a directory called `home/` get created inside your pier.
When you change files in this directory the changes will get synced into
your urbit.

Since each mount point is always watching for changes you may also want
to unmount from time to time.

The full syntax is as follows (from `dojo`):

    |mount <path-in-clay> [<mount-name>]

The `<mount-name>` is optional and defaults to the last part of 
the `<path-in-clay>`.

    |unmount <mount-name>

or:

    |unmount <path-in-clay>

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

The first step in contributing to urbit is to come and join us on
`:talk`.

For more detailed instructions check out
[`contributing.md`](https://github.com/urbit/urbit/blob/master/CONTRIBUTING.md),.
