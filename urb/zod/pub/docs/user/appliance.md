---
next: true
sort: 5
title: Appliance handbook
---

# Appliance handbook

You've built and launched your urbit.  How do you control it
securely?  Three ways: through the Unix console, over the Web,
or via an Urbit moon.

What are you controlling, anyway?  A user-level application on
Urbit is called an "appliance."  Think of an appliance as like a
Unix process that's also a persistent database.

By default, your urbit is running two appliances, the `:dojo`
shell and the `:talk` messenger.  For more advanced information
about your appliance state, see the end of this document.

## Console

The Unix console is the most basic way you talk to apps.  You've
already used it a bit, but let's do more.

The Urbit command line is a little like the Unix command line,
but different.  It's also a little like a window manager.

Your Unix terminal is separated into two parts: the prompt (the
bottom line) and the record (the rest of your screen).

The record is shared; all the output from all the apps in your
command set appears in it.  So you'll see `:talk` messages while
working in the dojo.

The prompt is multiplexed; you switch the prompting app with
`^X`.  Pressing return sends the prompt line to the app.  Urbit
does not automatically echo your input line into the record,
the way a normal Unix console does.

Also unlike a normal Unix console, the application can process
your input before you hit return.  In general, invalid input is
rejected with a beep.  Incorrect input may even be corrected!
Yes, this is highly advanced console technology.

Like many but not all Unix command lines, Urbit has built-in
history editing.  You've never seen anything like these
innovative key bindings before:

    ^A    cursor to beginning of the line (Home)
    ^B    cursor one character backward (left-arrow)
    ^E    cursor to the end of the line (End)
    ^F    cursor one character forward (right-arrow)
    ^G    beep; cancel reverse-search
    ^K    kill to end of line
    ^L    clear the screen
    ^N    next line in history (down-arrow)
    ^P    previous line in history (up-arrow)
    ^R    reverse-search
    ^T    transpose characters
    ^U    kill to beginning of line
    ^Y    yank from kill buffer

`^C` is processed at the Unix layer, not within Urbit.  If
there's an event currently running, ^C interrupts it and prints a
stack trace -- useful for finding infinite loops.

`^D` from `:talk` or `:dojo` stops your Unix process.  From any
other app, it removes the prompting app from the command set.

Pressing left-arrow or `^B` at the start of a line is an input
operation with the metaphorical meaning "get me out of here,"
ie, escape.  The exact semantics of an escape are application
dependent.

## Web

For now, we'll keep assuming you're at `http://localhost:8080`.
But for planets only, we also proxy web domains through Urbit's
own servers.  Any planet `~fintud-macrep` is also at
`fintud-macrep.urbit.org`.  Please use this proxy as little as
possible; it's not well-optimized.

There's a web interface to `:talk` at

    http://localhost:8080/~~/home/pub/talk

and a dojo interface at

    http://localhost:8080/~~/home/pub/dojo/fab

The `:talk` client is beautiful and works quite well.  Use it.
The `:dojo` client is a bit more of a work in progress.  (For
instance, you can't paste text into it.)

The login flow remains rather a work in progress.  It's not at
all secure.  But the first time you use an Urbit app (not just a
generated page -- you can tell by the `~~` in the URL), it will
prompt you for a password.

In a righteous world, the password would be (or default to) your
initial ticket.  Since the world is lawless and filled with evil,
just hit return and send a empty password.  The real password
will be printed on your console.

A URL that starts with `/~~/` authenticates your web page as the
same urbit as the server.  If instead of `/~~/` you write
`/~fintud-macrep/` (you're using an app on someone else's urbit,
and want to sign in as yourself), you'll authenticate with a
single-signon flow.  Don't worry, only your own urbit ever sees
your password.  Once you log in, a cookie is set and you don 't
need to do it again.

Internally, Urbit treats every web page as another urbit; even a
request with no `/~~/` or `/~fintud-macrep/` is an anonymous
comet.  Appliances actually can't tell whether they're talking to
an urbit over `%ames`, or a browser over HTTP.

### Enabling HTTPS

Urbit doesn't yet serve HTTPS directly.  But when you route
through `urbit.org`, you can also use HTTPS.  Our server handles
your HTTP request and proxies it over Urbit.  Of course we could
MITM you, but we won't.  If you want HTTPS on your own urbit, use
the "secure" port shown on startup:

    http: live ("secure") on 8443

Firewall off this port.  Get an nginx or other outer server.  Put
your SSL certificate in it and reverse-proxy to `8443`.

## An Urbit moon

The fanciest way to control your urbit is through Urbit itself:
a moon, or satellite urbit.  Sadly, only planets can have moons.

In this setup, you have a planet running on a box in the cloud,
and a moon on your laptop or other mobile.  Changes to the planet
automatically propagate to the moon.  Also, the moon's console is
linked to the `:talk` appliance on the planet, so you communicate
as yourself.

To build your moon, just run 

    ~fintud-macrep:dojo> +moon

This will generate a plot and a ticket.  The plot will be a
64-bit plot within your planet, like `~mignel-fotrym-fintud-macrep`.
On your laptop, install Urbit:

    urbit -w $plot -t $ticket

You'll get an urbit where ^x will switch you back and forth
between the two prompts:

    ~mignel-fotrym-fintud-macrep:dojo>
    ~fintud-macrep:talk() 

Your moon's `%home` desk is already synced to the `%kids` desk on
your planet.  If you want changes on your moon's `%home` to sync
back into your planet's `%home`, 

    ~fintud-macrep:dojo> |sync %home ~mignel-fotrym-fintud-macrep

## Urbit internals

As a user of any machine, you can't help understanding the
machine on more or less the same terms as its engineers.  You
should at least know what the major components are.

### Source code 

Urbit is a "run-from-repository" OS; all code ships as source on
your own urbit, in the revision-control vane `%clay`.  Better
yet, live code in every layer updates itself when the source
changes.  If the local source is subscribed to a remote
publisher, the update process is "evergreen" and automatic.

There are five major layers of code in Urbit.  Layer 1 is the
kernel (`arvo/hoon.hoon`), which includes both the Hoon compiler
and the Arvo event loop.  Layer 2 is the main library
(`arvo/zuse.hoon`),  Layer 3 is the kernel modules or "vanes"
(`arvo/*.hoon`).  Layer 4 is appliances, marks, and fabricators
(`ape/*.hoon`, `mar/*.hoon`, `fab/*.hoon`), run from the `%gall`
and `%ford` vanes.  Layer 5 is generators (`gen/*.hoon`), run
from the `:dojo` appliance.

The kernel (layer 1), the vanes (layer 3) and the apps (layer 4)
are stateful; their state is typed, and of course needs to be
preserved across code updates.  If the new state type differs,
the developer must provide a state adaptor.  If compilation fails,
the new code is disregarded and retried again on the next change.

Appliances are like Unix daemons, except persistent and permanent.
But they are the only process-like constructs in Urbit.  While
Unix uses many short-lived processes that are not daemons, Urbit
invokes simple tasks in simple contexts.  In Unix terms: since
the `ls` process is not meant to run forever and/or have side
effects, giving it the power to do so is asking for trouble.  In
Urbit, the layer 5 dojo generator `+ls` is a pure function which
simply produces some typed output.

You should never have to worry about any of this stuff; we're
just describing it here so it's not a mystery.

###  Appliances

Your main configuration task is choosing (a) what appliances
on your urbit should be running (the *active set*), and (b) what
appliances your console should be linked to (the *command set*).
Again, ^X switches the console prompt between appliances in (b).

Why are these different sets?  First, some apps don't need a
command prompt.  Your urbit runs three default apps: `:talk`,
`:dojo`, and `:hood`.  The hood is a system appliance and has no
direct UI (you control it through the dojo).

Second, you can also link your console to apps on other urbits,
and put those connections in the command set.  Essentially, the
console is also `ssh` or `rlogin`.  The console is just another
appliance (it's actually the `drum` library within `:hood`),
and Arvo is good at routing events over the network.

What is an appliance or "app," anyway?  An app, basically.  Think
of a Unix daemon, except that (1) it only responds to events and
can't run continuously ("nonpreemptive"); and (2) it's
"permanent" (never exits, dies, or is even reset).

Why do appliances live forever?  They have to.  An appliance name
is also a sort of port in the Internet sense.  A conversation
with appliance X on urbit Y is one conversation, not a sequence
of disconnected activations.

An appliance is always sourced from a path which is a function of
its name, from `%foo` to `/===/ape/foo/hoon`.  In this path, the
urbit is self; the version is now.  The desk defaults to `%home`;
changing it will adapt the appliance state.

The normal way to run code written by some external developer is
to merge (one time) or sync (for continuous upgrades), the
publisher's distribution desk, to a desk on your own urbit.
Obviously a desk per vendor is ideal, which also lets you
"sidegrade" an app to a different vendor by switching desks.

### Configuration commands

Initially, your command set is `:talk` and `:dojo`; your active
set is `:talk`, `:dojo` and `:hood`.  It's completely fine to
never even think about changing this, but...

#### `|link $?([app=term ~] [who=plot app=term ~])`

Link your console to an existing appliance, putting it in the
command set.  If no plot is specified, it's an appliance on your
own urbit.

    |link %dojo
    |link ~tasfyn-partyv %dojo

Permission is up to the appliance, of course.

#### `|unlink $?([app=term ~] [who=plot app=term ~])`

The opposite of `|link`.  Same interface.

#### `|start [app=term $|(~ [syd=desk ~])]`

Start an appliance, adding it to the active set.  If `syd` is not
specified, the desk is `%home`:

    |start %foo
    |start %foo %away

If the appliance is running, `|start` sets its desk instead.
