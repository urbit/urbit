---
next: true
sort: 2
title: Install
---

# Installation guide

Urbit can be installed on most Unix systems.  There is no Windows
port.  Windows is a wonderful OS, we just haven't gotten to it yet.
Use a VM.

## Configure the OS

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

## Install as a package

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
