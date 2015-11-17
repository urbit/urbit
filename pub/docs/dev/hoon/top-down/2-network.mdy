---
hide: true
next: false
sort: 3
title: Network Messages
---

Enough of pure hoonery.  Let's get to the good stuff.  Let's get
our planets to talk to each other.

Of course, for talking to be of any use, we need someone
listening.  What we've written up until now are just shell
commands that produce a value and then disappear.  We need an
actual app to listen for messages from another planet.  Let's
take a look at a very basic one.

```
::  There is no love that is not an echo
::
::::  /hoon/echo/ape
  ::
/?    314
|%
  ++  move  ,*
--
!:
|_  [bowl state=~]
++  poke-noun
  |=  arg=*
  ^-  [(list move) _+>.$]
  ~&  [%argument arg]
  [~ +>.$]
--
```

This is a very simple app that does only one thing.  If you poke
it with a value it prints that out.  You have to start the app,
then you can poke it from the command line.

```
> |start %echo
>=
> :echo 5
[%argument 5]
> :echo [1 2]
[%argument [1 2]]
```

Most of the app code should be simple enough to guess its
function.  The important part of this code is the definition of
`++poke-noun`.

Once an app starts, it's always on in the background, and you
interact with it by sending it messages.  The most
straightforward way to do that is to poke it from the command
line.  When you do that, `++poke-noun` is called from your app.

In our case, `++poke-noun` takes an argument `arg` and prints it
out with `~&`.  This is an unusual rune that formally "does
nothing", but the interpreter detects it and printfs the first
child.  This is a slightly hacky way of printing to the console,
and we'll get to the correct way later on.

But what does `++poke-noun` produce?  The phrase to remember is
"a list of moves and our state".  Urbit is a message passing
system, so whenver we want to do something that interacts with
the rest of the system we send a message.  Thus, the first thing
that `++poke-noun` produces is a list of messages, called
"moves".  In this case, we don't actually want the system to do
anything, so we produce the empty list, `~`.

The second thing `++poke-noun` produces is our state.  `+>.$`
refers to a particular address in our subject where our formal
app state is stored.  It'll become clear why this is later on,
but for now pretend that `+>.$` is a magic invocation that means
"app state".

---

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

---

Anyway, that's a lot of theory, let's get back to coding.  Let's
say we want to only accept a number, and then print out the
square of that number.

```
/?    314
|%
  ++  move  ,*
--
!:
|_  [bowl state=~]
::
++  poke-atom
  |=  arg=@
  ^-  [(list move) _+>.$]
  ~&  [%square (mul arg arg)]
  [~ +>.$]
--
```

A few things have changed.  Firstly, we no longer accept
arbitrary nouns because we can only square atoms.  Thus, our
argument is now `arg=@`.  Secondly, it's `++poke-atom` rather
than `++poke-noun`.

Are there other `++poke`s?  Definitely.  In fact, `noun` and
`atom` are just two of arbitrarily many "marks".  A mark is
fundamentally a type definition, and each mark is defined in the
`/mar` directory.  Some marks have conversion routines to other
marks, and some have diff, path, and merge algorithms.  None of
these are required for a mark to exist, though.

`noun` and `atom` are two of dozens of predefined marks, and the
user may add more at will.  The type associated with `noun` is
`*`, and the type associated with `atom` is `@`.

Data constructed on the command line is by default marked with
`noun`.  In this case, the app is expecting an atom, so we have
to explicitly mark the data with `atom`.

```
> |start %square
> :square 6
gall: %square: no poke arm for noun
> :square &atom 6
[%square 36]
```

Marks are powerful, and they're the backbone of urbit's data
pipeline, so we'll be getting quite used to them.

Let's write our first network message!

-don't forget to explain new hoon concepts ([])
-excercises
-then walk through more in depth
-move app state section to somewhere later
  -put in interactive code snippets and explain a lil (bowl)


