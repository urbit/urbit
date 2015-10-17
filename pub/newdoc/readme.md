# Urbit README

Urbit is a clean-slate system software stack defined as a
deterministic computer.  An encrypted P2P network, Ames, runs on
a functional operating system, Arvo, written in a strict, typed
functional language, Hoon, which compiles itself to a combinator
interpreter, Nock, whose spec is 340 bytes compressed.

Urbit's state is a pure, frozen function of its event history.
In practice it uses a memory checkpoint and an append-only log.
Every event is a transaction; Urbit is an ACID database and a
single-level store.  Code within Urbit has no direct access to
the underlying OS, but the Urbit state function defines I/O:

    |=([system-event this-state] [system-actions next-state])

Urbit is designed as a personal cloud server for self-hosted web
applications.  It's also good at using web APIs to manage any
data you may have stuck in traditional web applications.  And of
course it can communicate over its own encrypted P2P network,
which it already uses for code update and persistent chat.

Urbit is patent-free and MIT licensed.  Anyone can run the VM, of
course, but the Urbit network is officially invitation-only.  Not
that we're antisocial -- just that we're under construction.

Right now, Urbit's only practical use is to host and develop
itself.  Its performance is awful.  Its documentation is
inadequate.  Its keys are test keys.  Its servers melt down on a
regular basis.  We reserve the right to reboot the whole network
-- and we will.

If you're interested in following Urbit, you can:

-   Read more documentation at [urbit.org](http://urbit.org) XX doc
-   Give us your email address at [urbit.org](http://urbit.org).
-   Check out the
    [urbit-dev](https://groups.google.com/forum/#!forum/urbit-dev)
    mailing list.
-   Follow [@urbit_](https://twitter.com/urbit\_) on Twitter.
-   Hit us up by email, <urbit@urbit.org>.  We're nice!

# Code of conduct

Everyone involved in the Urbit project needs to understand and 
respect our code of conduct, which is: "don't be rude."

# Installing Urbit

Urbit can be installed on most Unix systems.  There is no Windows
port.  Windows is a great OS, we just haven't gotten to it yet.
Use a VM.

## Installing as a package

### OS X - Homebrew

    brew install --HEAD homebrew/head-only/urbit

### Ubuntu or Debian

Third-party packages are available, at:

    https://github.com/yebyen/urbit-deb

Urbit is only supported on Jessie onward (but outbound HTTPS
requests only work on Stretch; I wish we knew why; help us!)

## Hand-build from source

First, install all external dependencies.  Then, make.

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

The executable is `bin/urbit`.  Install it somewhere, or just use
it where it is.  Urbit chroots, so it's more secure if setuid root:

    sudo chmod +s bin/urbit

## Launch your urbit 

An urbit is a persistent server on a P2P network.  An address on
this network is a 128-bit number, or "plot."  Every server on the
Urbit network (we just call them "urbits") has one unique plot.

Since Urbit is designed as a personal server, an Urbit plot is
both a network address and a digital identity.  There is no
human-meaningful name layer like the DNS.  To make urbits as
memorable as possible, we type them in a phonemic syntax with one
byte per syllable, like `~harlyx-rocsev` for `0x510b.9441`.

Urbit address space is cryptographic property, like Bitcoin, but
Urbit doesn't use a blockchain.  Plots are digital land, not
digital currency; you own your urbit cryptographically, but you
don't get it by mining.  In land, transfers are infrequent and
not frictionless.  (Right now, all keys are test keys, and there
are no transfers, only initial invitations.)

Urbit is semi-decentralized: it overlays a 64-bit hierarchical
structure on the low end of a 128-bit fingerprint namespace.  If
you have an invitation, it's a *ticket* that lets you create a
32-bit urbit, aka *planet*.  If you don't have an invitation, you
have to create a 128-bit urbit, aka *comet*.

As a comet, you're not necessarily a bad person.  But you could
be anyone, so you have zero reputation.  You have no official
access to any Urbit services.  Any connectivity you may enjoy
could be shut off at any time, and it probably will be.  If the
Internet has proven one thing, it's that positive default
reputation and an infinite identity supply don't mix.

If you have an invitation, it's a planet like `~fintud-macrep`
and a ticket like `~fortyv-tombyt-tabsen-sonres`.  Run

    urbit -w fintud-macrep -t fortyv-tombyt-tabsen-sonres

If you don't have an invitation, pick a nickname for your comet,
like `mycomet`.  Urbit will randomly generate a 128-bit plot:

    urbit -c mycomet

Either way, creating your urbit will take some time.  Go get
a cup of coffee.  Some of this time involves creating keys;
some of it involves downloading code over Urbit itself.

Since Urbit is a P2P network that runs over random UDP ports,
some firewalls may not like it.  Urbit without connectivity isn't
useless, but it can't boot without the network.

If run as `root`, the `urbit` process can only read and write
inside the `fintud-macrep` or `mycomet` directory, which we call
your *pier*.  A pier is portable; any Urbit install on any OS can
execute the same pier.

In the pier directory is a set of user-level mount points.  Mount
points are synced Dropbox style, with Unix file changes
autocommitted to the Urbit revision control system (`%clay`) and
vice versa.  You edit Urbit code with vim and emacs, or whatever.

Also within the pier is a system directory, `.urb/`, which
contains an event log (`egz.hope`), a checkpoint (`.chk`), and
I/O directories for uploads and downloads (`put` and `get`).
You can compact the pier by deleting the checkpoint, although
that means Urbit needs to re-execute its entire event history.
This will take some time.  Go have a beer.

Also in `.urb` is a file like `code.~rosrev-dinnul`.  This is
your passcode; all data in the pier [XX: not yet the checkpoint]
is encrypted with it.  For extra security, print out or memorize
the contents of this file, then delete it; Urbit will prompt for
the passcode on startup.

When it's created (and before it calls `chroot()`), your urbit 
creates a passcode file, with a name like `~posluc-darnup`, 
in `~/.urbit`.  If you want to encrypt the checkpoint and log,
delete this file; Urbit will prompt you for its contents.  Please
be warned that Urbit is not at present secure in any way!

Wait until you see a prompt, either

      ~fintud-macrep:talk() 
or
      ~fintud-macrep:dojo> 

and then press ^D to quit.   Your urbit is launched.

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

To jump straight into the Urbit experience, join the global
station `urbit-meta`:

    ~fintud-macrep:talk() ;join ~doznec/urbit-meta

Please remember our code of conduct: "don't be rude."  Also,
`urbit-meta` is politically correct and safe for work.  Please,
no rude, un-PC or NSFW content.

Learn more
----------

Your urbit is a web server, so the best place to read about it 
is in your browser.

Urbit prints the HTTP port it's serving when it starts up:

    http: live (insecure) on 8080

8080 is the default.  If you're running on AWS or another cloud
service, this port may be firewalled; go to the firewall
configuration to open it.  In a last resort, you can use our
server, `doznec.urbit.org`.

But assuming it's `localhost:8080`, the docs are at

    http://localhost:8080/pub/fab/tree/doc

