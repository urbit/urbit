sigpam, `~&`, %sgpm
============================

PrintF

'PrintF' or 'log' in other languages. Often used for debugging.

More specifically in hoon terminology, prints `q` on the console
before computing `r`. `p` is the log priority, 0-3 defaulting to 0. `~&`
is similar to `printf` or `log` in other languages, and is very commonly
used for debugging.

`p` is optional and is rarely used.

Produces
--------

Twig: `[%sgpm p=@ud q=twig r=twig]`

Accepts
-------

`p` is a [`@ud`]() `q` is a [`++twig`]() `r` is a twig.

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

Examples
--------

    ~zod/try=> ~
    ~
    ~zod/try=> ~&('oops' ~)
    'oops'
    ~

The most common use of `~&`: print something to the console before
proceeding.
