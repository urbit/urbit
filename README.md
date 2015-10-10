# Urbit

Urbit is a clean-slate system software stack.  It's patent-free
and MIT licensed.  It runs its own encrypted P2P network over UDP.

At present urbit is under heavy development. It's not useful for
any external purpose. Documentation is completely inadequate.
Anyone can try the interpreter, of course, but the network is
officially invitation-only.

If you're interested in keeping in touch or following the project
you can:

-   Give us your email address at [urbit.org](http://urbit.org).
-   Check out the
    [urbit-dev](https://groups.google.com/forum/#!forum/urbit-dev)
    mailing list.
-   Follow [@urbit_](https://twitter.com/urbit\_) on Twitter.
-   Hit us up by email, <urbit@urbit.org>.  We're nice!

Install a package
-----------------

#### OS X - Homebrew

    brew install --HEAD homebrew/head-only/urbit

#### Ubuntu or Debian

Third-party packages are available, at:

    https://github.com/yebyen/urbit-deb

Urbit is only supported on Jessie onward (but outgoing HTTPS
requests only work on Stretch; I wish we knew why; if you have an
idea why or just think you can help, please let us know).

Install by hand
---------------

First, install all our external dependencies.  Then, make.

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

### Build instructions

Clone the repo:

    git clone git://github.com/urbit/urbit.git

`cd` to the directory you just created:

    cd urbit

Run `make`:

    make

(On FreeBSD, use `gmake` instead.)

The executable is `bin/urbit`.  Install it somewhere, or just use
it in place.

Create your urbit
---------------

Urbit is a semi-decentralized P2P network.  So you may or may not
have an invitation.

If you have an invitation, it's a *ticket* that lets you create
a 32-bit Urbit plot, aka *planet*.  If you don't have an
invitation, you have to create a 128-bit plot, aka *comet*.

As a comet, you're not necessarily a bad person.  But you could
be anyone, so you have zero reputation.  You have no official
access to any Urbit services.  Any connectivity you may enjoy
could be shut off at any time.  And probably will be.

If you have an invitation, it's a planet like `~fintud-macrep`
and a ticket like `~fortyv-tombyt-tabsen-sonres`.  Run

    urbit -w fintud-macrep -t fortyv-tombyt-tabsen-sonres

If you don't, pick a nickname for your comet, like `mycomet`.
Urbit will randomly generate a 128-bit address, but 

    urbit -c mycomet

Either way, creating your urbit will take some time.  Go get
a cup of coffee.  Some of this time involves creating keys;
some of it involves downloading code over Urbit itself.

Note that Urbit is a P2P network that runs over random UDP ports;
some firewalls may not like it.  Urbit without connectivity isn't
useless, but it can't boot without the network.

But however horribly hacked, the `urbit` process can only read
and write inside the `fintud-macrep` or `mycomet` directory,
which we call your *pier*.  A pier is portable; any Urbit install
on any OS can execute the same pier.

In the pier directory is a set of user-level mount points.  Mount
points are synced Dropbox style, with Unix file changes
autocommitted to the Urbit revision control system (`%clay`) and
vice versa.  You edit Urbit code with vim and emacs, or whatever.

Also within the pier is a system directory, `.urb`, which
contains an event log (`egz.hope`), a checkpoint (`.chk`), and
I/O directories for uploads and downloads (`put` and `get`).
You can compact the pier by deleting the checkpoint, although
that means Urbit needs to re-execute its entire event history.
This will take some time.  Go have a beer.

When it's created (and before it calls `chroot()`), your urbit 
creates a passcode file, with a name like `~posluc-darnup`, 
in `~/.urbit`.  If you want to encrypt the checkpoint and log,
delete this file; Urbit will prompt you for its contents.  Please
be warned that Urbit is not at present secure in any way!

Wait until you see a prompt, like

      ~fintud-macrep:talk>

and then press ^D to quit.   Your urbit is born.

Execute 
-------

To restart your urbit, run with the pier name:

    urbit fintud-macrep
    urbit mycomet

Your Urbit is a database, at least in theory.  You can kill the
process however you like, and it won't lose state.  In theory.
In practice, this works better on OS X than Linux.  Also, don't
let your filesystem run out of disk!

Out of the box, your urbit is running two default appliances,
`:dojo` (a shell or REPL) and `:talk`.  Switch between them with
`^X`.  Note that all apps share an output log, but `^X` switches
the prompt.

`^D` from any default appliance exits the urbit process.

Learn more
----------

Your urbit is a web server, so the best place to read about it 
is in your browser.

Urbit prints the HTTP port it's serving when it starts up:

    http: live (insecure) on 8080

8080 is the default.  If you're running on AWS or another cloud
service, this port may be firewalled; go to the firewall
configuration to open it.  In a last resort, you can use our
server, doznec.urbit.org.

Or just talk
------------

Use `^X` to get into `:talk`, and 
From `:talk`, 

    ~

Doing more
----------

To test the dojo, run 
Doing more


From either of the core 

directory.  Your pier (all Urbit state, log and checkpoint) will
be in `./fintud-macrep`.  The format is portable.  Doing `rm -r
fintud-macrep/.urb/chk` will delete the checkpoint, meaning all
your events need to be recomputed, but making the image smaller.


    bin/urbit fintud-macrep

Run (without a network invitation)
----------------------------------

To create a comet (128-bit urbit) in the Unix directory 
`mycomet`:

    bin/urbit -c mycomet

This will take a little while.  Go smoke a bowl.

This process can only read and write files within `mycomet`.

To quit Urbit (without destroying any data, since Urbit is a
database): ^D.

To start your comet again, omit the `-c` flag:

    bin/urbit mypier


Learn these two control keys first: ^D to quit Urbit (from either
of the two core applications), 

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
