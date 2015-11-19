---
hide: true
next: true
sort: 10
title: Launch
---

# Launch procedure

An urbit is a persistent server on the Urbit P2P network.
You'll create one of these servers now.  To understand what
you're building, you need to know a little about the network.

## Launch instructions

If you have an invitation, it's a planet like `~fintud-macrep`
and a ticket like `~fortyv-tombyt-tabsen-sonres`.  Run

    urbit -w fintud-macrep -t fortyv-tombyt-tabsen-sonres

(You can leave the `~` on, but it annoys some Unix shells.)

If you don't have an invitation, pick a nickname for your comet,
like `mycomet`.  Urbit will randomly generate a 128-bit plot:

    urbit -c mycomet

Either way, creating your urbit will take some time.  Some of
this time involves creating keys; some of it involves downloading
code over Urbit itself.   Go get a cup of coffee, and/or absorb
the miscellaneous true facts below:

## Network architecture

An Urbit address is a number, or *plot*, under 2^128.  Every
server on the Urbit network has one unique plot.

Since Urbit is designed as a personal server, a plot is both a
network address and a digital identity.  There is no additional
human-meaningful name layer like the DNS.  Plots will never be
meaningful; but to make them as memorable as possible, we type
them in a phonemic syntax with one byte per syllable, like
`~harlyx-rocsev` for `0x510b.9441`. 

Urbit address space is cryptographic property, like Bitcoin, but
Urbit doesn't use a blockchain.  Plots are digital land, not
digital currency; you own your urbit cryptographically, but you
don't get it by mining.  In land, transfers are infrequent and
not frictionless.  (Right now, all keys are test keys, and there
are no transfers, only initial invitations.)

Urbit is semi-decentralized: it overlays a 64-bit hierarchical
structure on the low end of a 128-bit fingerprint namespace.  If
you have an invitation, it's a *ticket* that lets you create a
32-bit plot, aka *planet*.  If you don't have an invitation, you
have to create a 128-bit plot, aka *comet*.

As a comet, you're not necessarily a bad person.  But you could
be anyone, so you have zero reputation.  You have no official
access to any Urbit services.  Any connectivity you may enjoy
could be shut off at any time, and it probably will be.  If the
Internet has proven one thing, it's that positive default
reputation and effectively infinite number of identities don't mix.

## Substrate interactions

Urbit doesn't run on bare chips and wires, at least not at
present.  It runs as a Unix process and sends UDP packets.

Since Urbit is a P2P network that runs over random UDP ports,
some firewalls may not like it.  Urbit without connectivity still
works as an interpreter, but it can't launch without the network.

If run as `root`, the `urbit` process can only read and write
inside the `fintud-macrep` or `mycomet` directory, which we call
your *pier*.  A pier is portable; any Urbit install on any OS can
execute the same pier.  (But don't *ever* run the same pier or
plot on two computers at once.)

In the pier directory is a set of user-level mount points.  Mount
points are synced Dropbox style, with Unix file changes
autocommitted to the Urbit revision control system (`%clay`) and
vice versa.  (Urbit does not have its own editor -- you edit
Urbit code either with an Unix editor on a mounted file, or with
a Web editor from your browser.)

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
the passcode on startup.  Please be warned that Urbit is not at
present secure in any way!

## Complete launch procedure

Wait until you see a prompt, either

      ~fintud-macrep:talk() 
or

      ~fintud-macrep:dojo> 

and then press ^D to quit.   

Your urbit is launched!  Ladies and gentlemen, we are floating in space.
