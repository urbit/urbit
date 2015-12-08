`%=`
====

Evaluates `p` with the changes specified in `q`. `q` is a list of addresses ([++wing]()) followed by the values that will replace whatever value currently lies at that address.
Like `%-`, `%=` has an addtional, irregular wifeform syntax that is frequently
used: `a(p q.p, q p.a)`, where `a` is the code to be executed with the changes
specified within the `()`.


`%=` is a natural rune that evaluates `p` with the changes specified in
`q`. `%=` is used to change a batch of [wing]()s inside a [`++wing`]()
all at once, ensuring that the product is type checked.

Produces
--------

Twig: `[%cnts p=wing q=tram]`

Accepts
-------

`p` is a [++wing](). `q` is a [++tram]().

Tall form
---------

    %=  p
      p.i.q    q.i.q
      p.i.t.q  q.i.t.q
    ==

Wide form
---------

    %=(p p.i.q q.i.q, p.i.t.q q.i.t.q)

Irregular form
--------------

    p(p.i.q q.i.q, p.i.t.q q.i.t.q)

Examples
--------

    /~zod/try=> =+  a=[p=5 q=6]
                a(p 2)
    [p=2 q=6]

In this example we are using the irregular form of `%=` to replace `p`
in `a`.

    /~zod/try=> =+  a=[p=1 q=2 r=3 s=4]
        a(p 5, q 6, r 7, s 8)
    [p=5 q=6 r=7 s=8]

Here we show how you can replace multiple faces at once. We start with a
new `a` and replace all of its values with the irregular form of `%=`.

    /~zod/try=> =+  step=0
                =+  leng=10
                =+  valu=0
                |-
                    ?:  =(step leng)
                       valu
                $(valu (mul 2 valu), step +(step))
    1.024

In this case we create a simple loop, using [`|-`](). To recurse, we use
`%=` with [`$`](), the empty name — our `|-` — replacing our `valu` with
`(mul 2 valu)` and `step` with `+(step)`.
