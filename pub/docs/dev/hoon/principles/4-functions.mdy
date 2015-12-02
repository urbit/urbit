---
next: false
sort: 4
spam: true
title: Hoon 101.4: functions
---
# Hoon 4: toward actual functions

In [chapter 0](0-nouns), we read about nouns.  In [chapter 1](1-twigs), 
we discovered twigs and legs.  In [chapter 2](2-syntax), we learned 
Hoon syntax and created our first source file.  In [chapter 3](3-algorithm) 
we wrote an actual algorithm.

Now we'll go a step farther and actually define a *function*.

## Goal: `(decrement arg)`

Of course, Hoon is a functional language and every twig defines a
function of the subject.  But we haven't seen the Hoon equivalent
of a function *declaration*.  Where is Lisp `defun`?

Also, the Hoon equivalent of a normal function call can't just
involve compiling the function into a Nock formula, whose subject
is the function's argument.  As Einstein said, everything must be
as simple as possible, but no simpler.

A function that worked this way would, by definition, have no
access to data or code other than the argument.  Where would the
standard library go?  Would we have to pass it in the argument?

## Form of the solution

Let's get the boilerplate out of the way.  We'll keep the same
external generator interface, but our solution will look like:

```
=<  :-  %say  |=  [* [[arg=@ud ~] ~]]  :-  %noun
    (decrement arg)
=>  ~
!!
```
In place of the `!!`, we'll put a core, effectively a library,
that exports a function `decrement`.  We'll then call that
function with `(decrement arg)`, an irregular form of
`%-(decrement arg)`.  Again, this core uses `=>  ~` to clear
the subject, so we can't cheat and call `(dec arg)`.

> `!!`, or "zapzap" or `[%zpzp ~]`, is a twig that
always crashes.  Because it produces the empty span (`%void`), it
doesn't cause type inference problems.

## Not quite a function call

As usual, we'll get to our goal gradually.  A first try:
```
=<  :-  %say  |=  [* [[arg=@ud ~] ~]]  :-  %noun
    =+  gat=decrement
    =<(run gat(sam arg))
=>  ~
|%
++  decrement
  =+  sam=0
  =+  pre=0
  |%
  ++  run
    ?:  =(sam +(pre))
      pre
    run(pre +(pre))
  --
--

~tasfyn-partyv:dojo/sandbox> +test 42
41
```
This works, but it's hardly concise or pretty.  We'll get there.

We replaced the `!!` with a `|%` twig that produces a library
core.  The payload of our library core is `~`, because that's the
subject we sent in.  The battery contains one arm, `decrement`.

The `decrement` arm produces an inner core.  The payload of this
inner core is `[pre=0 sam=0 +>]`, where `+>` is the library core.
The battery of the inner core has one arm, `run`, which computes
our algorithm from the last chapter.

To run our algorithm, we put the core that `decrement` produces
into a new leg, `gat`.  Mutating this core to set `sam` to our
argument `arg`, we evaluate the `run` arm.

> Why do we need this `gat` leg?  Why can't we just write `=<(run
decrement(sam arg))`?  Remember our `moo:bar.foo` problem from
the previous chapter.  `decrement(sam arg)` mutates not the
*product* of the decrement arm, but the core that computes it.
There's no `sam` in that subject, so the twig won't compile.

## Better is worse

To make this code look simpler, we need to make it more complex.
Our next try uses not two nested cores, but *three*:
```
=<  :-  %say  |=  [* [[arg=@ud ~] ~]]  :-  %noun
    =+  gat=decrement
    =<(run gat(sam arg))
=>  ~
|%
++  decrement
  =+  sam=0
  |%  
  ++  run
    =+  pre=0
    =<  loop
    |%
    ++  loop
      ?:  =(sam +(pre))
        pre
      loop(pre +(pre))
    --
  --
--
```
This is actually the final structure of our function, but uses
none of the syntactic tricks we'll use to make it pretty.

> Why do we need to make the code more complex?  Because we need
it to use the regular form that our synthetic runes, ie macros,
use to express function definitions and calls.

## Loop sugar

Look at little `loop`.  It works just like our old `run`.  But
there's something nice about it: we don't use the symbol `loop`
anywhere outside these 7 lines of code.  It's not exported.

Therefore, the `loop` name is useless and redundant.  Making up
names is one of the hard problems in computer science, so why
solve it for no reason?

So Hoon has an *empty name*: `$`.  As a constant, `$` is a
zero-length symbol (`%$` instead of `%foo`) As a limb is the
`buc` symbol (`$`).

Replacing `loop` with `$`, our loop becomes:
```
=<  $
|%
++  $
  ?:  =(sam +(pre))
    pre
  $(sam +(run))
--
```

This may not seem like a huge improvement.  It's not.  But it
lets us match and move to the synthetic rune `|-`, "barhep":
```
|-  ?:  =(sam +(pre))
      pre
    $(pre +(pre))
```
`|-` is simply the canonical Hoon loop.  All we've done to use a
core as a loop is to name our arm `$`, and evaluate it on the
core we just made.

Our program now:
```
=<  :-  %say  |=  [* [[arg=@ud ~] ~]]  :-  %noun
    =+  gat=decrement
    =<(run gat(sam arg))
=>  ~
|%
++  decrement
  =+  sam=0
  |%  
  ++  run
    =+  pre=0
    |-  ?:  =(sam +(pre))
          pre
        $(pre +(pre))
  --
--
```

## Almost to lambda

Could we use `$` for `++run`?  It certainly sounds like the same
kind of thing as `++loop`.  The word "run" just means "do it".
Why should we be saying "do it" all the time?

Our third try, with this change:
```
=<  :-  %say  |=  [* [[arg=@ud ~] ~]]  :-  %noun
    =+  gat=decrement
    =<($ gat(sam arg))
=>  ~
|%
++  decrement
  =|  sam=@ud
  |%
  ++  $
    =+  pre=0
    |-  ?:  =(sam +(pre))
          pre
        $(pre +(pre))
  --
--

~tasfyn-partyv:dojo/sandbox> +test 42
41
```
> Besides `run` to `$`, we changed `=+  sam=0` to `=|  sam=@ud`.
This is some mold magic we'll explain in the next chapter, but
it basically does the same thing: pushes a leg named `sam`, whose
value is a number we'll later overwrite.

This use of `$` in a core, very similar to the way we used `$` in
our `|-` loop, smells like it too should have a rune.  It does:
```
=<  :-  %say  |=  [* [[arg=@ud ~] ~]]  :-  %noun
    =+  gat=decrement
    =<($ gat(sam arg))
=>  ~
|%
++  decrement
  |=  sam=@ud
  =+  pre=0
  |-  ?:  =(sam +(pre))
        pre
      $(pre +(pre))
--

~tasfyn-partyv:dojo/sandbox> +test 42
41
```
Doesn't `decrement` look like a function?  Indeed, we're done with
the `decrement` arm.  This is what it should look like.

> If you squint a little, `|=` ("bartis") might even be a strange,
deformed ASCII-art lambda.

Since it's doing something simple, we might well even compress
the whole body of `decrement` into one wide-form line:
```
=+(pre=0 |-(?:(=(sam +(pre)) pre $(pre +(pre)))))
```
This is a bit tight for most.  But we can get really compact by
dropping the labels and going to full geometry:

```
=<  :-  %say  |=  [* [[arg=@ud ~] ~]]  :-  %noun
    =+  gat=decrement
    =<($ gat(sam arg))
=>  ~
|%
++  decrement  |=(@ud =+(0 |-(?:(=(+>+< +<) +< $(+< +(+<))))))
--
```
> It's not a good idea to actually program this way.  But it does
strengthen your Hoon tree-geometry muscles.  To use tree
geometry, you have to really know the shape of your subject.

## Structure of a gate

What is the noun produced by `decrement`?  In true Hoonese it's
a *gate*, but nobody will hate you for saying "function."  And
while we *slam* our gates, you can feel free to just "call" them.

Every gate is a core; not every core is a gate.  All cores are
shaped like `[battery payload]`.  A core is a gate if its payload
is a cell, and its battery has one arm, `$`.  So a gate is shaped
like `[formula [sample context]]`.

To slam (call) a gate, you replace its sample (`+6` or `+<`,
"lusgal" or "dish") with your argument.  "Multiple arguments" are
a single sample that's a tuple.  So our code should actually read 
```
    =+  gat=decrement
    =<($ gat(+< arg))
```
This `=+` sequence remains ugly.  Once again, `decrement(+< arg)`
is not a mutation to the product of `decrement`, but a mutation
to the core that produces `decrement`.  Fortunately, there's a
sugary rune that combines the semantics of both these lines:
```
    %*($ decrement +< arg))
```
This mutates the product of `decrement` as specified.  (`+< arg`
in the wide form of `$*` is just the first of n comma-separated 
pairs; we could write `%*($ decrement +< arg, +> foo, moo 42)`.

> Arguably, %* is oversweetened; $:(decrement +< arg)
would be simpler and better.  Hoon isn't perfect.

## Slamming the gate

Anyway, it shouldn't come as a surprise that a synonym for
`%*($ function +< argument)` is `%-(function argument)`.  And an
irregular form of this is just `(function argument)`.

The irregular syntax constructs multiple-argument syntax as a
tuple; `(add 2 2)` is `(add [2 2])`, `(rsh 3 1 5)` is `(rsh [3 1 5])`.
If there are no arguments, `(function)`, this means `$:function`, 
ie, `=>(function $)`.

We can also take the `=>  ~` out, since we now know what we're
doing.  The final program:
```
=<  :-  %say  |=  [* [[arg=@ud ~] ~]]  :-  %noun
    (decrement arg)
|%
++  decrement
  |=  sam=@ud
  =+  pre=0
  |-  ?:  =(sam +(pre))
        pre
      $(pre +(pre))
--
```
## Progress

In most functional languages, a function is a primitive.  In Hoon
it's a macro, and a pretty complex macro at that.  We spent four
chapters building the concept up out of much smaller pieces.

Fortunately, once you have functions you can do anything.  We
still have a fair bit to learn, but we've now seen all the
fundamental building blocks of programming in Hoon.  In the next
chapter, we'll get back into molds and spans.
