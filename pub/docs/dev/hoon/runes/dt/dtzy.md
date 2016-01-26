[dotzay, %dtzy](#dttr)
======================

Odored atom

`dotzay` is a natural rune that produces a non-cubed atomic constant of
odor `p` and value `q`. `dotzay` is never used explicitly, but always
implicitly to give odors to atoms.

Produces
--------

Twig: `[%dtzy p=term q=@]`

Sample
------

`p` is a [`++term`](). `q` is an [atom]().

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

    ~zod/try=> (ream '12')
    [%dtzy p=%ud q=12]
    ~zod/try=> (ream '&')
    [%dtzy p=%f q=0]
    ~zod/try=> (ream '~.sam')
    [%dtzy p=%ta q=7.168.371]

Here we use [`++ream`]() to unpack how values are parsed. Passing what
what would appear to be constants reveals that they are in fact parsed
into an odor [`++term`] and their numeric atom representation.
