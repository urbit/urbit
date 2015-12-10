---
next: true
sort: 3
title: State
---

In the last section we built a few small apps that sent moves.
These apps were entirely stateless, though.  Most useful apps
require some amount of state.  Let's build a trivial stateful
app.  It'll keep a running sum of all the atoms we poke it with.
Here's `ape/sum.hoon`:

```
/?    314
!:
|_  [bowl state=@]
++  poke-atom
  |=  arg=@
  ^-  [(list) _+>.$]
  ~&  [%so-far (add state arg)]
  [~ +>.$(state (add state arg))]
--
```

We can start with `|start %sum`, then run:

```
~fintud-macrep:dojo> :sum &atom 5
[%so-far 5]
>=
~fintud-macrep:dojo> :sum &atom 2
[%so-far 7]
>=
~fintud-macrep:dojo> :sum &atom 15
[%so-far 22]
>=
```

We can see that app state is being saved, but when, where, and
how?

The state is stored as the second thing in the `|_` line.  In our
case, it's simply an atom named `state`.  We change it by
producing as our state not `+>.$` but `+>.$(state (add state
arg))`.  We've seen all these parts before, but you might not
recognize them.

Recall in the first chapter that we recursed with the expression
`$(b (add 3 b))`.  This meant "produce `$` with `b` changed to
`(add 3 b)`.  Similarly, `+>.$(state (add state arg))` means
"produce `+>.$` (i.e. our context, which contains our state) with
`state` changed to `(add state arg)`.

At a high level, then, when we handle state, we do it explicitly.
It's passed in and produced explicitly.  In Unix systems,
application state is just a block of memory, which you need to
serialize to disk if you want to keep it around for very long.

In urbit, app state is a single (usually complex) value.  In our
example, we have very simple state, so we defined `state=@`,
meaning that our state is an atom.  Of course, `state` is just a
name we're assigning to it, and you're free to use whatever name
you want.

Two points you may be wondering about.  Firstly, `bowl` is a set
of general global state that is managed by the system.  It
includes things like `now` (current time), `our` (our urbit
identity), and `eny` (256 bits of guaranteed-fresh entropy).  For
the full list of things in `++bowl`, search for `++  bowl` (note
the double space) in `/arvo/zuse.hoon`.

> This is a very common technique in learning hoon.  While some
> documentation exists, often the easiest way to learn about an
> identifier you see in code is to search in `/arvo/zuse.hoon`
> and `/arvo/hoon.hoon` for it.  These are our two "standard
> libraries", and they're usually not hard to read.  Since
> urbit's codebase is relatively small (those two files are less
> than 15000 lines of code combined, and besides the standard
> library they include the hoon parser and compiler, plus the
> /arvo microkernel), you can usually use the code and the
> comments as reference doc.

The second point is that urbit needs no "serialize to disk" step.
Everything you produce in the app state is persistent across
calls to the app, restarts of the urbit, and even power failure.
If you want to write to the filesystem, you can, but it's not
needed for persistence.  Urbit has transactional events, which
makes it an ACID operating system.  You don't have to worry about
persistence when programming in urbit.
