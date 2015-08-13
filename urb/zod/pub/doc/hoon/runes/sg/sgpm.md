sigpam, `~&`, %sgpm
============================

printf

`~&`, `sigpam`, is a synthetic rune that prints `q` on the console
before computing `r`. `p` is the log priority, 0-3 defaulting to 0. `~&`
is similar to `printf` or `log` in other languages, and is very commonly
used for debugging.

Produces
--------

Twig: `[%sgpm p=@ud q=twig r=twig]`

Sample
------

`p` is a [`@ud`]() `q` is a [twig]() `r` is a [twig]()

Tall form
---------

Priority 0 (debug):

    ~&  q
    r

Priority 1 (notice):

    ~&  >  q
    r

Priority 2 (warning):

    ~&  >>  q
    r

Priority 3 (alarm):

    ~&  >>>  q
    r

Wide form
---------

    ~&(>> q r)

Irregular form
--------------

undefined

Examples
--------

    ~zod/try=> ~
    ~
    ~zod/try=> ~&('oops' ~)
    'oops'
    ~

The most common use of `~&`: print something to the console before
proceeding.
