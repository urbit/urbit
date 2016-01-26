wutdot, `?.`, %wtdt
============================

If, else

`?.`is a synthetic rune that produces `r` if `p` is yes (`&`, `0`), or
`q` if `p` is no (`|`, 1). `?.` is sort of like "if not", producing the
second expression if the loobean is true. `?.` is the inverse of `?:`.

See also
--------

wutcol, `?:`, %wtcl
============================

Produces
--------

Twig: `[%wtdt p=twig q=twig r=twig]`

Sample
------

`p`, `q`, and `r` are [twig]()s.

Tall form
---------

    ?.  p
      q
    r

Wide form
---------

    ?.(p q r)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> ?.((gth 1 2) 1 2)
    1
    ~zod/try=> ?.(?=(%a 'a') %not-a %yup)
    %yup

Here we see two common cases of `?.` in the wide form, one uses an
expression `gth` that produces a loobean and the other [`?=`]() to
produce one of its cases.

Equivalent to
-------------

    ?:(p r q)
