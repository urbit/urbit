`?:`
====

If-then-else.

`?:`, `wutcol`, is a natural rune that produces `q` if `p` is yes (`&`,
`0`), or `r` if `p` is no (`|`, 1). `?:` is most similar to the
traditional "if" statement, producing the first expression if the
loobean is true. `?:` is the inverse of `?.`.

Produces
--------

Twig: `[%wtcl p=twig q=twig r=twig]`

Accepts
-------

`p`, `q`, and `r` are [twig]()s.

Tall form
---------

    ?:  p
      q
    r

Wide form
---------

    ?:(p q r)

Examples
--------

    ~zod/try=> ?:((gth 1 2) 1 2)
    2
    ~zod/try=> ?:(?=(%a 'a') %yup %not-a)
    %yup

Here we see two common cases of `?:` in the wide form, one uses an
expression `gte` that produces a loobean and the other [`?=`]() to
produce one of its cases.
