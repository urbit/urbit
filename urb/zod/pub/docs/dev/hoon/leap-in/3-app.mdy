---
hide: true
next: false
sort: 3
title: Advanced Applications
---

XXX PLACEHOLDER

But what is our app state, exactly?  In Unix systems, application
state is just a block of memory, which you need to serialize to
disk if you want to keep it around for very long.

In urbit, app state is a single (usually complex) value.  In our
example, we don't have any special state, so we defined
`state=~`, meaning that our state is null.  Of course, `state` is
just a name we're assigning to it, and you're free to use
whatever name you want.

Since urbit is purely functional, we can't just implicitly "have"
and "change" our state.  Rather, it's explicitly passed to us, in
the `|_  [bowl state=~]` line, and we produce the new state with
`+>.$` in the `[~ +>.$]` line.

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

Second point is that urbit needs no "serialize to disk" step.
Everything you produce in the app state is persistent across
calls to the app, restarts of the urbit, and even power failure.
If you want to write to the filesystem, you can, but it's not
needed for persistence.  Urbit has transactional events, which
makes it an ACID operating system.  Persistence is just another
one of those things you don't have to worry about when
programming in urbit.

As fascinating as state is, we don't actually need any state to
accomplish our immediate goal, which is to get apps on two urbits
talking to each other.  We'll discuss state more in a later
chapter.
