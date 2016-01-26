kettis, `^=`, %ktts
============================

Wrap

`^=`, `kettis`, `[%ktts p=toga q=twig]` is a natural rune that wraps `q`
in the [`++toga`]() `p`. `^=` is most commonly used for assignment,
adding one or more names to values.

Produces
--------

Twig: `[%ktts p=toga q=twig]`

Sample
------

`p` is a [`++toga`](). `q` is a [twig]().

Tall form
---------

    ^=  p
    q

Wide form
---------

    ^=(p q)

Irregular form
--------------

    p=q

Examples
--------

    ~zod/try=> a=1
    a=1
    ~zod/try=> ^=  a
               1
    a=1

In this straightforward example we see the irregular and tall forms of
`^=`, both of which assign `a` to be `1`.

    ~zod/try=> [b ~ c]=[1 2 3 4]
    [b=1 2 c=[3 4]]

Here we see multiple names being applied at once, using the irregular
form of `^=`.
