`|-`
====

Trap: automatically called function with no arguments

A function with no arguments that's automatically called when constructed. More specifically, it produces a [dry]() [%gold]() [core]() whose single [arm]() [$]() (the empty name) is automatically called when the core is constructed. Similar to creating and calling an anonymous function and is quite commonly used for loops or recursion.

Produces
--------

[`++twig`]: `[%brhp p=twig]`

Accepts
-------

`p` is a twig

Tall form
---------

    |-  p

Wide form
---------

    |-(p)

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
