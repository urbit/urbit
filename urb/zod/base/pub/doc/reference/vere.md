<div class="short">

vere
====

vere is the Urbit virtual machine.

        bin/vere -c ship

------------------------------------------------------------------------

Options
-------

### `-b`

Batch create

------------------------------------------------------------------------

### `-c`

Create

Creates a new pier. Takes a folder name, such as `pier`.

    bin/vere -c pier

------------------------------------------------------------------------

### `-d`

Daemon

------------------------------------------------------------------------

### `-D`

Dry compute

dry compute the north and/or south events

------------------------------------------------------------------------

### `-f`

Fuzz testing

------------------------------------------------------------------------

### `-k`

Kernel version

------------------------------------------------------------------------

### `-l`

Raft port

------------------------------------------------------------------------

### `-L`

Localhost.

Routes all networking over `0.0.0.0` and checks the keys.

    bin/vere -L -I ~del -c fz

------------------------------------------------------------------------

### `-M`

Memory madness

------------------------------------------------------------------------

### `-n`

Unix hostname

------------------------------------------------------------------------

### `-p`

Specify the [`%ames`](/doc/arvo/ames) udp listening port.

    bin/vere -p 42665

It can sometimes help if you get a port pointed at you and run vere with
`-p` to specify the [`%ames`](/doc/arvo/ames) udp listening port. VMs
and [docker](http://www.docker.com/) containers and the like tend to put
up some pretty effective barriers to
[NAT](http://en.wikipedia.org/wiki/Network_address_translation) [hole
punching](http://en.wikipedia.org/wiki/TCP_hole_punching).

------------------------------------------------------------------------

### `-P`

Profile

------------------------------------------------------------------------

### `-q`

Quite

Inverse of [`-v`](#-v)

See also: [`:verb`](/doc/arvo/util#verb))

------------------------------------------------------------------------

### `-r`

Raft flotilla

Also needs the [`-l`](#-l) option set.

------------------------------------------------------------------------

### `-F`

Fake

Routes all networking over `0.0.0.0` and doesn't check any keys. This
allows you to start any carrier.

    bin/vere -F -I ~zod -c zod

You get an isolated network with just yourself but you can [`:ticket`]()
other ships or start other ships or start other carriers.

------------------------------------------------------------------------

### `-I`

Imperial

Takes a carrier name, such as `~zod`.

    bin/vere -F -I ~zod -c zod

------------------------------------------------------------------------

### `-v`

Verbose

Inverse of [`-q`](#-q)

See also: [`:verb`](reference/arvo/util.md#verb).

    bin/vere -v mypier

------------------------------------------------------------------------

### `-X`

Skip last event

    bin/vere -Xwtf mypier

------------------------------------------------------------------------

Tips
====

Inability to mmap 2Gb with `MAP_FIXED`
--------------------------------------

It's probably because of
[ASLR](http://en.wikipedia.org/wiki/Address_space_layout_randomization)
(some shared library got its data mapped in the middle of your address
space). If so, applying

    bash setarch `uname -m` -R ./bin/vere

should help.

</div>
