`?.`
====

Inverted if-then-else

Unless `p` is true, evaluate `r`. Used to keep the heavier of `q` and `r` as the bottom expression, which makes for more readable code (see the section on [backstep]()).

Produces
--------

Twig: `[%wtdt p=twig q=twig r=twig]`

Accepts
-------

`p`, `q`, and `r` are [`++twig`]()s.

Tall form
---------

    ?.  p
      q
    r

Wide form
---------

    ?.(p q r)

Examples
--------

    ~zod/try=> ?.((gth 1 2) 1 2)
    1
    ~zod/try=> ?.(?=(%a 'a') %not-a %yup)
    %yup

Here we see two common cases of `?.` in the wide form, one uses an
expression `gth` that produces a boolean and the other [`?=`]() to
produce one of its cases.

Equivalent to
-------------

    ?:(p r q)
