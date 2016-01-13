tisdot, `=.`, %tsdt
============================

Single change subject

`=.` is a synthetic rune that produces `r` with `p` in the subject set
to `q`.

Produces
--------

Twig: `[%tsdt p=wing q=twig r=twig]`

Sample
------

`p` is a [`wing`](). `q` and `r` are [`twig`]()s.

Tall form
---------

Kingside:

    =.  p 
      q
    r

Queenside:

    =.  p  q
    r

Wide form
---------

    =.(p q r)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> =+  a=[b=1 c=2]
               =.  b.a  3
               a
    [b=3 c=2]
    ~zod/try=> =+  a=[b=1 c=2]
               =.(b.a 3 a)
    [b=3 c=2]

Here we see the tall and wide forms of `=.` used to manipulate our
simple subject `a=[b=1 c=2]`.
