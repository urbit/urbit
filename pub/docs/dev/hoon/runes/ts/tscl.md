tiscol, `=:`, %tscl
============================

Batch change subject

`=:` is a synthetic rune that produces `q` with the subject modified by
the list of changes in `p` which must be terminated by a `==`. `=:` is
useful when you need to make a batch of changes to your subject.

See also
--------

tisdot, `=.`, %tsdt
============================

Produces
--------

Twig: `[%tscl p=tram q=twig]`

Sample
------

`p` is a [`++tram`](), a list of [`++wing`]()s and twigs. `q` is a
[twig]().

Tall form
---------

    =:  p.i.p      q.i.p
        p.i.t.p    q.i.t.p
        p.i.t.t.p  q.i.t.t.p
      ==
      q

Wide form
---------

None

Irregular form
--------------

None

Examples
--------

    ~zod/try=> =+  a=[b=1 c=2]
               =:  c.a  4
                   b.a  3
                 ==
               a
    [b=3 c=4]

Here we add a simple cell with faces to our subject, `a=[b=1 c=2]` and
make a set of changes to it using `=:`.
