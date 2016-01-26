barhep, `|-`, %brhp
============================

Kicked trap

`|-` is a synthetic rune that produces a dry [`%gold`]() trap and
[kicks]() it. You can think of a trap like a 'trap door', since `|-` is
a [`door`]() with only one arm [`$`](), the empty name.

`|-` is different from `|.` in that it is kicked by default. `|-` is
similar to creating and calling an anonymous function and is quite
commonly used for loops or recursion.

See also
--------

bardot, `|.`, %brdt

Produces
--------

Twig: `[%brhp p=twig]`

Sample
------

`p` is a twig

Tall form
---------

    |-  p

Wide form
---------

    |-(p)

Irregular form
--------------

None

Examples
--------

    /~zod/try=> |-(42)
      42

In contrast to our `|.` example, `|-` is kicked by default, so its
internals are produced immediately.

    /~zod/try=> =+  a=`*`~[41 42]
                |-
                ?~  a
                  ~
                [-.a %m $(a +.a)]
    [41 %m [42 %m ~]]

In this case we use `|-` for one of its most common applications, a
loop. Here we walk across `a` by calling `$` with `a` replaced by `+.a`,
producing a nested tuple.

    ++  dec                                                 ::  decrement
      ~/  %dec
      |=  a=@
      ~|  %decrement-underflow
      ?<  =(0 a)
      =+  b=0
      |-  ^-  @
      ?:  =(a +(b))
        b
      $(b +(b))

In `++dec`, found in `hoon.hoon`, `|-` creates a trap that contains the
test and looping semantics. Essentially, we count up to `a-1` by
incrementing `b`. Using `|-` inside an existing core as we do here is
its most common use.
