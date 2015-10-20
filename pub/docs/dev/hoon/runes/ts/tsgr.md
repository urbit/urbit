tisgar, `=>`, %tsgr
============================

Product as subject

`=>` is a natural rune that uses the product of `p` as the subject of
`q`. `=>` is the inverse of `=<`. Use `=>` when your `p` isn't too long
and `=>` makes for more readable code.

See also
--------

tisgal, `=<`, %tsgl
============================

Produces
--------

Twig: `[%tsgr p=twig q=twig]`

Sample
------

`p` and `q` are [twig]()s.

Tall form
---------

    =>  p
        q

Wide form
---------

    =>(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> =>([a=1 b=2 c=3] b)
    2
    ~zod/try=> =>((add 2 4) [. .])
    [6 6]

In this simple example we first produce `b` from the tuple
`[a=1 b=2 c=3]` using the wide form of `=>`. Then we use `.` to produce
our context from the computation `(add 2 4)` as a cell, `[6 6]`.

    ~zod/try=> 
    =cor  |=  a=@
          =+  b=0
          =>  .(b (add 2 b))
          =>  .(a (add a b))
          [a b]
    new var %cor
    /~zod/try=> 
    (cor 4)
    [6 2]

Here we see a common pattern for using `=>` to conduct procedural
changes to values in our subject. First we replace `b` with `b+2` using
the irregular form of `%-`, then we replace `a` with the sum of `a` and
`b` the same way.
