`~?`
====

printf if `q` true

Same as `~&` except for that `r` is printed to the console if and
only if `q` evaluates to true.  In either case, `s` is executed
and the result is produced.

Produces
--------

Twig: `[%sgwt p=@ud q=twig r=twig s=twig]`

Accepts
-------

`p` is a [`@ud`](). `q` is a [`++twig`](). `r` is a twig. `s` is a
twig.

Tall form
---------

Priority 0 (debug):

        ~?  q
          r
        s

Priority 1 (notice):

        ~?  >  q
          r
        s

Priority 2 (warning):

        ~?  >>  q
          r
        s

Priority 3 (alarm):

        ~?  >>>  q
          r
        s

Wide form
---------

    ~&(>> q r s)

Examples
--------

    ~zod/try=> ~?((gth 1 2) 'oops' ~)
    ~
    ~zod/try=> ~?((gth 1 0) 'oops' ~)
    'oops'
    ~

A simple case of the conditional printf. When our condition evaluates to
true we print our `r`. Most useful in computation dealing with dynamic
data.
