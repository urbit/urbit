barcab, `|_`, %brcb
============================

Door: core with sample

`|_` is a synthetic rune that produces a [`%gold`]() [door]() with
sample `p` and [arms]() `q`. The list must be closed with a `--`.

`|_` is similar to `|%`, but defines a sample for the set of arms it
contains. Moreover, `|_` only accepts [dry or `%elm`]() arms. Put
simply, type checking on these arms is performed on the input before
computation. For more on variance, see the [glossary entry]() and the
examples below.

See also
--------

[barcen, `|%`, `%brcn`]() [barfas, `|/`, `%brfs`]()

Produces
--------

Twig: `[%brcb p=tile q=(map term foot)]`

Sample
------

`p` is a [tile](). `q` is a [`map`]() with [`++term`]() keys and
[`++foot`]() values.

Tall form
---------

    |_  p
    ++  p.n.q
      q.n.q
    --

Wide form
---------

None

Irregular form
--------------

None

Examples
--------

    /~zod/try=> =mol
                  |_  a=@ud
                  ++  succ  +(a)
                  ++  prev  (dec a)
                  --
    /~zod/try=> ~(succ mol 1)
    2
    /~zod/try=> ~(succ mol ~(succ mol ~(prev mol 5)))
    6

In this example we create a door `mol` that operates on a [`@ud`](),
`a`. We add two arms to our door, `++succ` and `++prev`, and invoke them
with the irregular form of [`%~`](). Doors are commonly invoked with
`%~`, irregular form `~(arm door sample)`, which replaces the door's
sample and pulls the specified arm.

    /~zod/try=> =kom
                      |_  a=(list)
                      ++  hed  -.a
                      ++  tal  +.a
                      --
    new var %kom
    /~zod/try=> =kot
                      |/  a=(list)
                      +-  hed  -.a
                      +-  tal  +.a
                      --
    new var %kot
    /~zod/try=> ~(tal kom "abc")
    t=~[98 99]
    /~zod/try=> ~(tal kot "abc")
    t="bc"
    /~zod/try=> ~(tal kot [1 2 3 ~])
    [2 3 ~]
    /~zod/try=> ~(tal kom [1 2 3 ~])
    t=~[2 3]

Here we're demonstrating the difference between `|_` and `|/`. We create
a nearly identical door using both runes, each with an arm that produces
the tail of the sample, `a`. You can see that our wet gates use the
sample as a tile to produce well-typed output.

    ++  ne
      |_  tig=@
      ++  d  (add tig '0')
      ++  x  ?:((gte tig 10) (add tig 87) d)
      ++  v  ?:((gte tig 10) (add tig 87) d)
      ++  w  ?:(=(tig 63) '~' ?:(=(tig 62) '-' ?:((gte tig 36) (add tig 29) x)))
      --
    ::

`++ne` is used to print a single digit in base 10, 16, 32, or 64 and is
a part of the hoon standard library. You can find it in `hoon.hoon`.
`|_` is very commonly used throughout our standard library for groups of
arms who all take the same sample.

    ~zod/try=> ~(x ne 12)
    99
    ~zod/try=> `@t`~(x ne 12)
    'c'
    ~zod/try=> `@ux`12
    0xc

Here we put `++ne` to work a bit. Our first call renders 12 in base 16.
Since `99` is within the ASCII character range, we can cast it to a
[`@t`]() and get `'c'`. Conveniently, casting `12` to a [`@ux`]()
results in `0xc`.
