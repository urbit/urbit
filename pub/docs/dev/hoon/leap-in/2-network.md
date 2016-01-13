---
next: false
sort: 2
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
!:
|_  [bowl state=~]
++  poke-noun
  |=  arg=*
  ^-  [(list) _+>.$]
  ~&  [%argument arg]
  [~ +>.$]
--
```

This is a very simple app that does only one thing.  If you poke
it with a value it prints that out.  You have to start the app,
then you can poke it from the command line with the following
commands:

```
~fintud-macrep:dojo> |start %echo
>=
~fintud-macrep:dojo> :echo 5
[%argument 5]
>=
~fintud-macrep:dojo> :echo [1 2]
[%argument [1 2]]
>=
```

> There is currently a bug where the `%argument` lines are
> printed *above* the line you entered, so your output may not
> look exactly like this.

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

But what does `++poke-noun` produce?  Recall that `^-` casts to a
type.  In this case, it's declaring that end result of the
function will be of type `[(list) _+>.$]`.  But what does this
mean?

The phrase to remember is "a list of moves and our state".  Urbit
is a message passing system, so whenver we want to do something
that interacts with the rest of the system we send a message.
Thus, a move is arvo's equivalent of a syscall.  The first
thing that `++poke-noun` produces is a list of messages, called
"moves".  In this case, we don't actually want the system to do
anything, so we produce the empty list, `~` (in the `[~ +>.$]`
line).

The second thing `++poke-noun` produces is our state.  `+>.$`
refers to a particular address in our subject where our formal
app state is stored.  It'll become clear why this is later on,
but for now pretend that `+>.$` is a magic invocation that means
"app state".

Let's look at another example.  Say we want to only accept a
number, and then print out the square of that number.

```
/?    314
!:
|_  [bowl state=~]
::
++  poke-atom
  |=  arg=@
  ^-  [(list) _+>.$]
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
fundamentally a type definition, but accessible at the arvo
level.  Each mark is defined in the `/mar` directory.  Some marks
have conversion routines to other marks, and some have diff,
patch, and merge algorithms.  None of these are required for a
mark to exist, though.

`noun` and `atom` are two of dozens of predefined marks, and the
user may add more at will.  The type associated with `noun` is
`*`, and the type associated with `atom` is `@`.

Data constructed on the command line is by default marked with
`noun`.  In this case, the app is expecting an atom, so we have
to explicitly mark the data with `atom`.  Try the following
commands:

```
~fintud-macrep:dojo> |start %square
>=
~fintud-macrep:dojo> :square 6
gall: %square: no poke arm for noun
~fintud-macrep:dojo> :square &atom 6
[%square 36]
>=
```

> Recall the bug where `%square` may get printed above the input
> line.

Marks are powerful, and they're the backbone of urbit's data
pipeline, so we'll be getting quite used to them.

**Exercises**:

- Write an app that computes fizzbuzz on its input (as in the
  previous section).

- One way of representing strings is with double quoted strings
  called "tapes".  The hoon type is `tape`, and there is a
  corresponding mark with the same name.  Write an app that
  accepts a tape and prints out `(flop argument)`, where
  `argument` is the input.  What does this do?


Let's write our first network message!  Here's `/ape/pong.hoon`:

```
/?    314
|%
  ++  move  ,[bone term path *]
--
!:
|_  [bowl state=~]
::
++  poke-urbit
  |=  to=@p
  ^-  [(list move) _+>.$]
  [[[ost %poke /sending [to %pong] %atom 'howdy'] ~] +>.$]
::
++  poke-atom
  |=  arg=@
  ^-  [(list move) _+>.$]
  ~&  [%receiving (,@t arg)]
  [~ +>.$]
::
++  coup  |=(* [~ +>.$])
--
```

Run it with these commands:

```
~fintud-macrep:dojo> |start %pong
>=
~fintud-macrep:dojo> :pong &urbit ~sampel-sipnym
>=
```

Replace `~sampel-sipnym` with another urbit.  Don't forget to
start the `%pong` app on that urbit too.  You should see, on the
foreign urbit, this output:

```
[%receiving 'howdy']
```

Most of the code should be straightforward.  In `++poke-atom`,
the only new thing is the expression `(,@t arg)`.  As we already
know, `@t` is the type of "cord" text strings.  `,` is an
operator that turns a type into a validator function -- that is,
a function whose domain is all nouns and range is the given type,
and which is identity when the domain is restricted to the given
type.  In simpler terms, it's a function that coerces any value
to the given type.  We call this `,@t` function on the argument.
This coerces the argument to text, so that we can print it out
prettily.

The more interesting part is in `++poke-urbit`.  The `urbit` mark
is an urbit identity, and the hoon type associated with it is
`@p` (the "p" stands for "phonetic base").

Recall that in a `++poke` arm we produce "a list of moves and our
state".  Until now, we've left the list of moves empty, since we
haven't wanted to tell arvo to do anything in particular.  Now we
want to send a message to another urbit.  Thus, we produce a list
with one element:

```
[ost %poke /sending [to %pong] %atom 'howdy']
```

The general form of a move is

`[bone term path *]`

If you look up `++bone` in `hoon.hoon`, you'll see that it's a
number (`@ud`), and that it's an opaque reference to a duct.
`++duct` in hoon.hoon is a list of `wire`s, where `wire` is an
alias for `path`.  `++path` is a list of `span`s, which are ASCII
text.  Thus, a duct is a list of paths, and a bone is an opaque
reference to that duct (in the same way that a Unix file
descriptor is an opaque reference to a file structure).  Thus,
the center of all this is the concept of a "duct".

A duct is stack of causes, represented as paths.  At the bottom
of every duct is a unix event, such as a keystroke, network
packet, file change, or timer event.  When arvo is given this
event, it routes the event to appropriate kernel module for
handling.

Sometimes, the module can immediately handle the event and
produce any necessary results.  Otherwise, it asks other kernel
modules or applications to do certain things, and produces the
result from that.  When it sends a message to another kernel
module or application, it sends it "along" the duct it was given,
plus with a new path.  Arvo pushes the new path onto the duct.
Now the duct has two entries, with the unix even on the bottom
and the kernel module that handled it next.  This process can
continue indefinitely, adding more and more layers onto the duct.
When an entity produces a result, a layer is popped off the duct.

In effect, a duct is an arvo-level call stack.  The duct system
creates a structured message-passing system.  It's worth noting
that while in traditional call stacks a function call happens
synchronously and returns exactly once, in arvo multiple moves
can be sent at once, they are evaluated asynchronously, and each
one may be responded to zero or more times.

Anyhow, the point is that whatever caused `++poke-urbit` to be
called is also the root cause for the network message we're
trying to send.  Thus, we say to send the network message along
the given bone `ost`.  Of course, we have to push a layer to the
duct.  This layer can have any data we want in it, but we don't
need anything specific here, so we just use `/sending`.  If we
were expecting a response (which we're not), it would come back
along the `/sending` path.  It's a good idea for debugging
purposes to make the path human-readable, but it's not necessary.

Looking back at the general form of a move, there is a `term`,
which in this case is `%poke`.  This is the name of the
particular kind of move we're sending.  If you think of a move as
a syscall (which you should), then this `term` is the name of the
syscall.  Common ones include: `%poke`, to message an app;
`%warp`, to read from the filesystem; `%wait`, to set a timer;
and `%them`, to send an http request.

The general form ends with `*` since each type of move takes
different data.  In our case, a `%poke` move takes a target
(urbit and app) and marked data and pokes that app on that urbit
with that data.  `[to %pong]` is the target urbit and app,
`%atom` is the mark`, and `'howdy'` is the data.

When arvo receives a `%poke` move, it calls the appropriate
`++poke`.  The same mechanism is used for sending messages
between apps on the same urbit as for sending messages between
apps on different urbits.

> We said earlier that we're not expecting a response.  This is
> not entirely true:  the `++coup` is called when we receive
> acknowledgment that the `++poke` was called.  We don't do
> anything with this information right now, but we could.

**Exercises**:

- Extend either of the apps in the first two exercises to accept
  input over the network in the same way as `pong`.

- Modify `pong` to print out a message when it receives an ack.

- Write two apps, `even` and `odd`.  When you pass an atom to
  `even`, check whether it's even.  If so, divide it by two and
  recurse; otherwise, poke `odd` with it.   When `odd` recieves
  an atom, check whether it's equal to one.  If so, terminate,
  printing "%success".  Otherwise, check whether it's odd.  If
  so, multiply it by three, add one, and recurse; otherwise, poke
  `even` with it.  multiply it by three and add one.  When either
  app receives a number, print it out along with the name of the
  app.  In the end, you should be able to watch Collatz's
  conjecture play out between the two apps.  Sample output:

```
~fintud-macrep:dojo> :even &atom 18
[%even 18]
[%odd 9]
[%even 28]
[%even 14]
[%odd 7]
[%even 22]
[%odd 11]
[%even 34]
[%odd 17]
[%even 52]
[%even 26]
[%odd 13]
[%even 40]
[%even 20]
[%even 10]
[%odd 5]
[%even 16]
[%even 8]
[%even 4]
[%even 2]
%success
```

- Put `even` and `odd` on two separate ships and pass the
  messages over the network.
