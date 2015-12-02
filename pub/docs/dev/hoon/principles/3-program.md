---
next: false
sort: 3
spam: true
title: Hoon 101.3: an algorithm
---
# Hoon 101.3: an algorithm

In [chapter 0](0-nouns), we read about nouns.  In [chapter 1](1-twigs), 
we discovered twigs and legs.  In [chapter 2](2-syntax), we learned 
Hoon syntax and created our first source file.

Now it's time for an actual, meaningful *algorithm*.

## How to use this tutorial

Ideally, you've installed an Urbit planet (if you have a ticket)
or comet (if you don't).  See the [user doc](../../../user).

We recommend opening up the dojo and just typing the examples;
you don't know a language until you know it in your fingers.
Also, make sure you've worked through the chapters in order.

## Goal: a decrement generator

Our algorithm is the classic Urbit example: decrement.

If you learned [Nock](../../nock) before Hoon, you've already
written decrement.  If not, all you need to know is that the only
built-in arithmetic operator in Nock is increment.  To decrement,
we need to count up to the result with a simple loop.

## A practical subject

As we've seen, Hoon works by running a twig against a subject.
In chapter 1, we made a simple test subject that was just a few
constants, and applied some simple twigs to it.

This time, we'll actually put useful working data in the subject,
building up more and more complex subjects as we go.

## Subject: nil

We'll start with the empty subject `~`:
```
:-  %say  |=  *  :-  %noun
=>  ~
[%hello %world]
```
The `=>` rune ("tisgar", `%tsgr`)), for `=>(p q)` executes `p`
against the subject, then uses that product as the subject of
`q`.

>  We've already used an irregular form of `=>`, or to be more
precise its mirror `=<` ("tisgal", `%tsgl`).  In chapter 1, when we wrote
`+3:test`, we meant `=>(test +3)` or `=<(+3 test)`.

What is `~`?  It's Hoon `nil`, a zero atom:
```
~tasfyn-partyv:dojo/sandbox> ?? ~
  [%cube 0 [%atom %n]]
~
```
We use `~` for list terminators and the like.  Obviously, since
`[%hello %world]` is just a constant, a nil subject works as
well as any:

```
~tasfyn-partyv:dojo/sandbox> +test
[%hello %world]
```

## Subject: `arg=@`

Above, we used an empty subject to avoid the very complex, 
interesting subject your generator twig gets by default.

But a decrement generator needs to get an argument from the 
command line -- the number we're trying to decrement.

This involves changing the `test.hoon` boilerplate a little.
Again, don't worry about the boilerplate line just yet:
```
:-  %say  |=  [* [[arg=@ud ~] ~]]  :-  %noun
=>  arg=arg
[%hello arg]

~tasfyn-partyv:dojo/sandbox> +test 42
[%hello 42]
```
>  `=>  arg=arg` looks a little odd.  We wouldn't ordinarily do
this; we're just trying to keep the subject simple.

In case there's any doubt about the subject (remember from
chapter 1 that `.` means `+1`, the whole subject):
```
:-  %say  |=  [* [[arg=@ud ~] ~]]  :-  %noun
=>  arg=arg
.

~tasfyn-partyv:dojo/sandbox> +test 42
arg=42
```

## An increment generator

We can even write a trivial increment generator using `.+`,
or even better its irregular form `+`:
```
:-  %say  |=  [* [[arg=@ud ~] ~]]  :-  %noun
=>  arg=arg
+(arg)

~tasfyn-partyv:dojo/sandbox> +test 42
43
```
We'll assume these same first two lines for the rest of the
examples in this chapter.

## Cores: one more kind of span

To decrement, we need a loop.  To write a loop, we need yet
another kind of noun: the *core*.  Briefly, a core is a `[code
data]` pair.  Our new `span` mold:
```
++  span
  $%  [%atom p=@tas]
      [%cell p=span p=span]
      [%core p=span q=(map ,@tas twig)]
      [%cube p=* q=span]
      [%face p=@tas q=span]
  ==
```

### Structure of a core

The core is a cell `[battery payload]`.  The payload is data, the
battery is code -- a tree of Nock formulas.

In the `%core` span, the payload is described by an arbitrary
span.  The battery is described by a map of symbol to twig.

> Yes, the span contains the complete AST of the whole core.
This is a very different approach from most typed languages,
which compute function signatures.  To infer the product of an
arm, we retrace the code in theory, but cache in practice.

### Arms are computed attributes

How do we use a core?  Remember that Nock is a function
```
Nock(subject formula) -> product
```
To activate a core, we pull one formula from the battery, and
execute with the whole core as the subject.  Coincidentally,
Nock has one macro instruction, `9`, that does just this.

So the formula defines an arbitrary function on the core.  The
name of this function (as defined in the twig map) is a computed
attribute, or *arm*.

> Is a core an object?  Not quite, because an arm is not a method.
Methods in an OO language have arguments.  Arms are functions
only of the payload.  (A method in Hoon is an arm that produces a
gate, which is another core -- but we're getting too far ahead.)
However, the battery does look a lot like a classic "vtable."

### Arms and wing resolution

It might be a good time to brush up on your [chapter 1](1-twigs).
The wing resolution algorithm gets a little more complicated.

Hoon overloads computed attributes (arms) and literal attributes
(legs) in the same namespace.  A label in a wing may refer to
either.  when searching a core, we look for a matching arm.  If
we find it we're done.  If we don't, or if a `^` mark makes us
skip, we search into the payload.

Only the last limb in the wing can activate an arm.  If a name
resolves to a core arm, but it's not the last limb in the wing,
the arm produces the core itself.  Similarly, when the wing is
not an access but a mutation, the arm refers to the core.  Only
the end of the wing is activated.

> Suppose `foo` is a core.  `bar` is an arm on foo.  Then `bar.foo`
computes the `bar` arm.  Perhaps the product is another core.
Even if `moo` is an arm in this new core, `bar.foo`, the wing
`moo.bar.foo` does not compute `moo` on the core `bar.foo`.
Instead, it looks for `moo` within the payload of the core `foo`.
(You're looking for `moo:bar.foo`, ie, `=>(bar.foo moo)`.)

Does this sound too tricky?  Hopefully not, but it's about the
most complicated semantic rule you'll find in Hoon.

## Increment with a core

Keeping our two-line boilerplate, let's increment with a core:
```
=<  inc
|%
++  inc
  +(arg)
--

~tasfyn-partyv:dojo/sandbox> +test 42
43
```
What's going on?  We used the `|%` rune ("barcen") to produce a
core.  (There are a lot of runes which create cores; they all
start with `|`, and are basically macros that turn into `|%`.)

The payload of a core produced with `|%` is the subject with
which `|%` is compiled.  We might say that `|%` wraps a core
around its subject.  In this case, the subject of the `|%`,
and thus payload, is our `arg=@ud` argument.

Then we used this core as the subject of the simple wing `inc`.

> Remember that `=<(a b)` is just `=>(b a)`.  The core is heavy
and `inc` is light, so we use `=<` to put the heavy part last.

The prettyprinter can even print a core, sort of.  Take out the 
`=<  inc`:
```
|%
++  inc
  +(arg)
--

~tasfyn-partyv:dojo/sandbox> +test 42
<1.bgq arg=42>

~tasfyn-partyv:dojo/sandbox> ? +test 42
  <1.bgq arg=@ud>
<1.bgq arg=42>
```
In this notation, `1` means the number of arms in the core, `bgq`
is a very short fingerprint, and `arg=42` is the payload (and
`arg=@ud` is the payload span).

> Cores can be large and complex, and we obviously can't render all
the data in them, either when printing a type or a value.  At
some point, you'll probably make the mistake of printing a big
core, maybe even the whole kernel, as an untyped noun.  Just
press ^C.

## Adding a counter

To decrement, we need to count up to the argument.  So we need a
counter in our subject, because where else would it go?

Let's change the subject to add a counter, `pre`:
```
=>  [pre=0 .]
=<  inc
|%
++  inc
  +(arg)
--

~tasfyn-partyv:dojo/sandbox> +test 42
43
```
Once again, `.` is the whole subject, so we're wrapping it in a
cell whose head is `pre=0`.  This doesn't change the way we use 
`arg`, even though it's one level deeper in the subject tree.
Let's look at the subject again:
```
=>  [pre=0 .]
.

~tasfyn-partyv:dojo/sandbox> +test 42
[pre=0 arg=42]
~tasfyn-partyv:dojo/sandbox> ? +test 42
  [pre=@ud arg=@ud]
[pre=0 arg=42]
```
There's a less cluttered way to write `=>([a .] b)`.  In wide
mode, `=+(a b)`.  A tall example:
```
=+  pre=0
.

~tasfyn-partyv:dojo/sandbox> +test 42
[pre=0 arg=42]
```
This rune `=+`, "tislus", `%tsls`, is of course the "variable 
declaration" we saw in chapter 2.

## We actually decrement

Now we can write our actual decrement program, combining the
features we've explored above:
```
=+  pre=0
=<  dec
|%
++  dec
  ?:  =(arg +(pre))
    pre
  dec(pre +(pre))
--

~tasfyn-partyv:dojo/sandbox> +test 42
41
```
`=(a b)` is an irregular form of `.=(a b)`, ie, "dottis" or the
noun `[%dtts a b]`.  Likewise, `+(a)` is `.+(a)`, ie, "dotlus"
or `[%dtls a]`.

`?:`, "wuttis", `%wtts`, does exactly what it does in C.

The real action is in `dec(pre +(pre))`.  We saw this same
mutation form in chapter 1.  It's an irregular form of the `%=`
rune, "centis", `%cnts`.

Here, of course, we're computing an arm of a mutated core.  We're
recomputing the loop arm, `dec`, against a new core with an
incremented `pre` leg.  Basically, everything interesting in Hoon
happens when you compute an arm of a mutated core.

The whole program, using only regular forms, wide and tall:
```
=+  ^=(pre 0)
=<  dec
|%
++  dec
  ?:  .=(arg .+(pre))
    pre
  %=  dec
    pre  .+(pre)
  ==
--
~tasfyn-partyv:dojo/sandbox> +test 42
41
```
A good illustration of why we need irregular forms.

## Progress

Now we've actually done something useful.  Well, if you count
O(n) decrement as something useful.

We've actually seen most of the major tools in Hoon's toolbox,
just in a super-minimalist selection.  But we'll need a few more 
runes to get to something that looks like real programming.

For instance, we have a decrement algorithm, but we don't have a
decrement *function* - in the Hoon sense of the word, anyway.  We
don't see `(dec arg)` in this code.  That'll be the next chapter.
