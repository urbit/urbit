---
hide: true
next: false
sort: 3
title: Advanced Applications
---

## State

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
~fintud-macrep:dojo> :switch &atom 5
[%so-far 5]
>=
~fintud-macrep:dojo> :switch &atom 2
[%so-far 7]
>=
~fintud-macrep:dojo> :switch &atom 15
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

## Subscriptions

We've dealt fairly extensively with "poke" messages to an app,
but these are somewhat limited.  A poke is a one-way message, but
more often we want to subscribe to updates from another app.  You
could build a subscription model out of one-way pokes, but it's
such a common pattern that it's built into arvo.

Let's take a look at two apps, `source` and `sink`.  First,
`source`:

```
/?    314
!:
|%
++  move  ,[bone %diff mark *]
--
|_  [hid=bowl state=~]
++  poke-noun
  |=  arg=*
  ^-  [(list move) _+>.$]
  :_  +>.$
  %+  turn  (prey /the-path hid)
  |=([o=bone *] [o %diff %noun arg])
++  peer
  |=  pax=path
  ^-  [(list move) _+>.$]
  ~&  [%subscribed-to pax=pax]
  [~ +>.$]
--
```

And secondly, `sink`:

```
/?    314
|%
++  move  ,[bone card]
++  card
  $%  [%peer wire [@p term] path]
      [%pull wire [@p term] ~]
  ==
--
!:
|_  [bowl available=?]
++  poke-noun
  |=  arg=*
  ^-  [(list move) _+>.$]
  ?:  &(=(%on arg) available)
    [[[ost %peer /subscribe [our %source] /the-path] ~] +>.$(available |)]
  ?:  &(=(%off arg) !available)
    [[[ost %pull /subscribe [our %source] ~] ~] +>.$(available &)]
  ~&  ?:(available %not-subscribed %subscribed)
  [~ +>.$]
++  diff-noun
  |=  [wir=wire arg=*]
  ^-  [(list move) _+>.$]
  ~&  [%recieved-data arg]
  [~ +>.$]
++  reap
  |=  [wir=wire error=(unit tang)]
  ^-  [(list move) _+>.$]
  ?~  error
    ~&  %successfully-subscribed
    [~ +>.$]
  ~&  [%subscription-failed error]
  [~ +>.$]
--
```

Here's some sample output of the two working together:

```
~fintud-macrep:dojo> |start %source
>=
~fintud-macrep:dojo> |start %sink
>=
~fintud-macrep:dojo> :sink %on
[%subscribed-to pax=/the-path]
%successfully-subscribed]
>=
~fintud-macrep:dojo> :source 5
[%received-data 5]
>=
~fintud-macrep:dojo> :sink %off
>=
~fintud-macrep:dojo> :source 6
>=
~fintud-macrep:dojo> :sink %on
[%subscribed-to pax=/the-path]
%successfully-subscribed]
>=
~fintud-macrep:dojo> :source 7
[%recived-data 7]
>=
```

Hopefully you can get a sense for what's happening here.  When we
poke `sink` with `%on`, `sink` subscribes to `source`, and so
whenever we poke `source`, `sink` gets the update and prints it
out.  Then we unsubscribe by poking `sink` with `%off`, and
`sink` stops getting updates.  We then resubscribe.

There's a fair bit going on in this code,  Let's look at `source`
first.

Our definition of `move` is fairly specific, since we're only
going to sending one kind of move.  The `%diff` move is a
subscription update, and its payload is marked data which gall
routes to our subscribers.

This is a slightly different kind of move than we've dealt with
so far.  It's producing a result rather than calling other code
(i.e. it's a return rather than a function call), so if you
recall the discussion of ducts, a layer gets popped off the duct
rather than added to it.  This is why no wire is needed for the
move -- we can get no response to it.

Anyhow, there's two functions inside the `|_`.  We already know
when `++poke-noun` is calle.  `++peer` is called when someone
tries to subscribe to our app.  Of course, you don't just
subscribe to an app; you subscribe to a path on that app.  This
path comes in as the argument to `++peer`.

In our case, we don't care what path you subscribed on, and all
we do is print out that you subscribed.  Arvo keeps track of your
subscriptions, so you don't have to.  You can access your
subscribers by looking at `sup` in the bowl that's passed in.
`sup` is of type `(map bone ,[@p path])`, which associates bones
with the urbit who subscribed, and which path they subscribed on.
If you want to talk to your subscribers, send them messages along
their bone.

`++poke-noun` spams the given arguement to all our subscribers.
There's a few things we haven't seen before.  Firstly, `:_(a b)`
is the same as `[b a]`.  It's just a convenient way of formatting
things when the first thing in a cell is much more complicated
than the second.  Thus, we're producing our state unchanged.

Our list of moves is the result of a call to `++turn`.  `++turn`
is what many languages call "map" -- it runs a function on every
item in a list and collects the results in a list.  The list is
`(prey /the-path hid)` and the function is the `|=` line right
after it.

`++prey` is a standard library function defined in `zuse`.  It
takes a path and a bowl and gives you a list of the subscribers
who are subscribed on a path that begins with the given path.
"Prey" is short for "prefix".

Now we have the list of relevant subscribers.  This a list of
triples, `[bone @p path]`, where the only thing we really need is
the bone because we don't care what urbit they are or what exact
path they subscribed on.  Thus, our transformer function takes
`[o=bone *]` and produces `[o %diff %noun arg]`, which is a move
that means "tell bone `o` this subscription update:  `[%noun
arg]`".  This is fairly dense code, but what it's doing is
straightforward.

`source` should now makes sense.  `sink` is a little longer, but
not much more complicated.

In `sink`, our definition of of `++move` is different.  All moves
start with a `bone`, and we conventionally refer to the second
half as the "card", so that we can say a move is an action that
sends a card along a bone.

We have two kinds of cards here:  we `%peer` to start a
subscription, and we `%pull` to stop it.  Both of these are
"forward" moves that may receive a response, so they need a wire,
which is what gets pushed onto the duct.  They also need a
target, which is a pair of an urbit and an app name.  `%peer`
needs a path on that app to subscribe too.  `%pull` doesn't need
this, because its semantics are to cancel any subscriptions
coming over this duct.  If your bone and wire are the same as
when you subscribed, then the cancellation will happen correctly.

The only state we need for `sink` is a loobean to tell whether
we're already subscribed to `source`.  We use `available=?`,
where `?` is a boolean (which defaults to true).

In `++poke-noun` we check our input to see if it's `%on` and
we're available, which means we should subscribe.  If so, we
produce the move to subscribe to `source`:

```
[ost %peer /subscribe [our %source] /the-path]
```

Also, we set available to false (`|`) with `+>.$(available |)`.

Otherwise, if our input is `%off` and we're already subscribed
(i.e. `available` is false), then we unsubscribe with from
`source`:

```
[ost %pull /subscribe [our %source] ~]
```

It's important to send over the same bone and wire (`/subscribe`)
as the one we originally subscribed on.

We also set `available` to true (`&`).

If neither of these cases are true, then we just print out our
current subscription state and do nothing.

`++diff-noun` is called when we get a `%diff` update along a
subscription, and it's marked with `noun`.  `++diff-noun` is
given the wire that we originally passed the `%peer`
subscription request along and the data we got back.  In our case
we just print out the data.

`++reap` is called when we receive an acknowledgment as to
whether the subscription was handled successfully.  I think
`reap` is supposed to bring to mind a backwards `peer` just like
`coup` brings to mind a backwards `poke`, but if that mnemonic
isn't helpful, just memorize it.

Anyhow, `++reap` is given the wire we attempted to subscribe over
and a possible error message.  Of cours, there's only an error
message if it fails.  `(unit type)` means "either `~` or `[~
type]`, which means it's used like Haskell's "maybe" or C's
nullability.  If `error` is `~`, then the subscription was
successful, so we tell that to the user.  Otherwise, we print out
the error message.
