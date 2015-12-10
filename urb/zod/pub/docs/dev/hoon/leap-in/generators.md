---
next: true
sort: 7
title: Using Generators with Apps
---

Up until now we've poked apps directly.  This requires the user
to specify the mark, and it requires the app to accept the
arguments in a way that's convenient for users to input.  This is
the "plumbing" way to interact with apps.  Generators are the
"porcelain" layer.  When you run a command like `+ls` or
`|merge`, there are no marks in sight.

We've used generators before, back in [Basic
Operation](basic).  At that point, we just used the generators
to produce values -- we didn't pipe their results into apps.  In
the dojo cast, the role of a generator is to take a list of
arguments and produce a value, which is often, though not always,
piped into an app.  Generators are pure, stateless functions, and
they cannot send moves.

`+ls` and `+cat` are commonly-used generators that produce a directory
listing and print out a file, respectively.  These are useful in
themselves, so we generally don't pipe the results into an app.

Another generator is `+hood/merge`.  Try running `+hood/merge`
with the normal arguments for `|merge`:

```
~fintud-macrep:dojo> +hood/merge %home our %they
[syd=%home her=~fintud-macrep sud=%they gem=%auto]
~fintud-macrep:dojo> +hood/merge %home our %they, =gem %this
[syd=%home her=~fintud-macrep sud=%they gem=%auto]
```

This didn't run any merge, it just constructed the command that,
if sent to `:hood`, would run it.  Note first that, even though
it's not printed, this value has mark `kiln-merge`.  Also, the
merge strategy `gem=%auto` was added automatically.  Optional
arguments are straightforward with generators.

In general, generators are prepended by either `+` or `|`.  The
general form is `+generator`, but often generators are created
specifically to work with a single app, they're usually placed in
a subdirectory of gen and run with `|`.  Thus, if `my-generator`
generator is made for use with `:my-app`, then `my-generator` is
put in `/gen/my-app/my-generator.hoon`, and can be run directly
with `+my-app/my-generator <args>`.  If you want to pipe the
result into `:my-app`, then run `:my-app +my-app/my-generator
<args>`.  Because this pattern is so common, this can be
abbreviated to `:my-app|my-generator <args>`.  Because most
built-in commands are generators for the `:hood` app,
`:hood|generator <args>` can be shortened to `|generator <args>`.

Let's write a generator for a modified version of `:pong` from
the chapter on [Network Messages](network).  Recall that `:pong`
takes an urbit address, which is of mark `urbit`, and sends that
urbit the message `'howdy'`.  Let's make `:ping` that does the
same, except that it lets the user optionally specify the message
as well.

We'll need a new mark for our arguments.  Let's call it
`ping-message`.

> For app-specific marks, it's good style to prefix the name of
> the mark with the name of the app.  Since many apps have
> several such marks, subdirectories in `/mar` are rendered as
> `-`, so that `ping-message` is written in
> (`/mar/ping/message.hoon`).

```
|_  [to=@p message=@t]
++  grab
  |%
  ++  noun  ,[@p @t]
  --
--
```

The app can easily be modified to use this (`/ape/ping.hoon`):

```
/?    314
|%
  ++  move  ,[bone term wire *]
--
!:
|_  [bowl state=~]
::
++  poke-ping-message
  |=  [to=@p message=@t]
  ^-  [(list move) _+>.$]
  [[[ost %poke /sending [to dap] %atom message] ~] +>.$]
::
++  poke-atom
  |=  arg=@
  ^-  [(list move) _+>.$]
  ~&  [%receiving (,@t arg)]
  [~ +>.$]
::
++  coup  |=(* `+>)
--
```

Now we can run this with:

```
~fintud-macrep:dojo> |start %ping
>=
~fintud-macrep:dojo> :ping &ping-message [~sampel-sipnym 'heyya']
>=
```

And on `~sampel-sipnym`, assuming it's running `:ping` as well,
we see `[%receiving 'heyya']`.

This is an annoying way to invoke the app, though.  The `[]` are
mandatory, and optional arguments are hard.  Let's make a
generator, `+send` to make everything nicer.  Since it's specific
to `:ping`, let's put it in `/gen/ping/send.hoon`:

```
:-  %say
|=  [^ [[to=@p message=?(~ [text=@t ~])] ~]]
[%ping-message to ?~(message 'howdy' text.message)]
```

A couple of new things here.  Firstly, `message=?(~ [text=@t ~])`
should be read as "the message is either null or a pair of text
and null".  Generator argument lists are always null-terminated,
which makes it convenient to accept lists in tail position (which
are particularly annoying without generators).  `?(a b)` is the
irregular form of `$?(a b)`, which is a union type.  Thus, `?(a
b)` means the type of anything that's in either `a` or `b`.
Thus, in our case, `?(~ [text=@t ~])` is either null or a pair of
text and null.

Secondly, `?~(a b c)` is a rune which means "if the `a` is null,
do `b`, else `c`".  It is roughly equivalent to `?:(=(~ a) b c)`.

> `?~(a b c)` is actually equivalent to `?:=(?=(~ a) b c)`,
> which, although identical at run time, is subtly different at
> compile time.  Specifically, using `?=` rather than `=` means
> that we're checking whether `a` is in the *type* of `~`, and so
> the compiler knows that in the `b` case `a` is null, and in the
> `c` case `a` is not null.  Since `=` is purely a runtime value
> check with no type implications, the compiler doesn't gain any
> information.
>
> At any rate, this is the reason why we can refer to
> `text.message` in the `c` clause.  The compiler knows that
> `message` is not null, so it must have `text` within it.  If
> you used `?:(=(~ message) 'howdy' text.message)` the compiler
> would complain that it doesn't know whether `message` has
> `text` within it.

This is run as follows:

```
~fintud-macrep:dojo> :ping|send ~sampel-sipnym
>=
~fintud-macrep:dojo> :ping|send ~sampel-sipnym 'how do you do'
>=
```

Which causes `~sampel-sipnym` to print `[%receiving 'howdy']` and
`[%receiving 'how do you do']`.

*Exercises*:

- Create a generator for `:sum` from [State](state) so that
  you can run `:sum|add 5` to add numbers to it.

- Create a generator for `:click` from [Web Apps](web-apps) so
  that you can run `:click|poke` to poke it.
