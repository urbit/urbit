dotzaz, %dtzz
======================

Cube

`dotzaz` is a natural rune that produces a cubed noun constant of value
`q` and odor `p`, if `q` is an atom. A cube is an atom whose type
contains only itself.

See also
--------

[`cube`]()

Produces
--------

Twig: `[%dtzz p=term q=*]`

Sample
------

`p` is a [`++term`](). `q` is an [noun]().

Tall form
---------

None

Wide form
---------

None

Irregular form
--------------

None

Examples
--------

    ~zod/try=> (ream '%12')
    [%dtzz p=%ud q=12]
    ~zod/try=> (ream '%&')
    [%dtzz p=%f q=0]
    ~zod/try=> (ream '%sam')
    [%dtzz p=%tas q=7.168.371]

Here we use [`++ream`]() to unpack how values are parsed. Prefixing a
value with a `%` implicitly turns it into a `%cube` with the following
value: an odored noun.
