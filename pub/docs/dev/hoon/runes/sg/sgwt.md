sigwut, `~?`, %sgwt
============================

Conditional printf

`~?` is a synthetic rune with the same hint effect as `~&`, printing `r`
if and only if `q` produces `&`, loobean yes.

Produces
--------

Twig: `[%sgwt p=@ud q=twig r=twig s=twig]`

Sample
------

`p` is a [`@ud`](). `q` is a [twig](). `r` is a [twig](). `s` is a
[twig]().

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

None

Examples
--------

    ~zod/try=> ~?((gth 1 2) 'oops' ~)
    ~
    ~zod/try=> ~?((gth 1 0) 'oops' ~)
    'oops'
    ~

A simple case of the conditional printf. When our condition evaluates to
`&` we print our `r`. Most useful in computation dealing with dynamic
data.
