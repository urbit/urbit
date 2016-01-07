`?:`
====

If-then-else

If `p` is true, produce `q`, else produce `r`.

Produces
--------

Twig: `[%wtcl p=twig q=twig r=twig]`

Accepts
-------

`p`, `q`, and `r` are [`++twig`]()s.

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
expression [`++gte`]() that produces a boolean and the other [`?=`]() to
produce one of its cases.
