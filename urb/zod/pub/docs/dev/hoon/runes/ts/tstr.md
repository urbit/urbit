`=*`
====

Makes `p` an alias of value `q`. Useful when you don't want to write
out a long address multiple times, for example `p.s.+.variable`, which translates to `p` within `s` within the tail of `variable`.


`=*` is a natural rune that creates a `%bull`, or alias, type.

Produces
--------

Twig: `[%tstr p=term q=wing r=twig]`

Accepts
-------

`p` is a [`term`](). `q` is a [`++wing`](). `r` is a [twig]().

Tall form
---------

    =*  p  q
        r

Wide form
---------

    =*(p q r)

Examples
--------

    ~zod/try=> 
        =+  a=1
        =*  b  a
        [a b]
    [1 1]
    ~zod/try=> 
        =+  a=1
        =*  b  a
        =.  a  2
        [a b]
    [2 2]

Here we see two simple examples of `=*`, both aliasing `b` to the value
of `a`. In our second case you can see that even when we change the
value of `a`, `b` continues to point to that value.
