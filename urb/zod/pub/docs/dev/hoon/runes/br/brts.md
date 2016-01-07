`|=`
====

Function with argument(s)

`|=`, is a synthetic rune that produces a [dry]() [`%gold`]()
gate with sample [`$*(p)`](), arm `q`. A gate is a core with one
arm, [`$`](), the empty name. `|=` checks its input sample
against its [mold](), `p`.

`|=` is a function that takes a defined input and produces the
result of some computation. `|=` differs from `|*` in that its
typechecking occurs at compile-time to ensure that all inputs
match its sample mold.

Produces
--------

Twig: `[%brts p=mold q=twig]`

Accepts
-------

`p` is a mold. `q` is a twig.

Tall form
---------

    |=  p
    q

Wide form
---------

    |=(p q)

Examples
--------

    ~zod/try=> =inc |=(a=@ +(a))
    ~zod/try=> (inc 20)
    21

Here we create a very simple gate that increments its sample, `a`. You
can think of `|=` as similar to a straightforward function that takes
arguments.

    ++  add                                                 ::  add
          ~/  %add
          |=  [a=@ b=@]
          ^-  @
          ?:  =(0 a)
            b
          $(a (dec a), b +(b))

In [++add](), from `hoon.hoon`, `|=` creates a gate whose sample takes
two atoms labeled `a` and `b`, and whose arm evaluates an expression
that produces the sum of the atoms by decrementing `a` until it is `0`
and incrementing `b` at each step. Here, `$` is used for recursion,
calling the gate produced by `|=` with `a` replaced by `(dec a)` and `b`
replaced by `+(b)`.
