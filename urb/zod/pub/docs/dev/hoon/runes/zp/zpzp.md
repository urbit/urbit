`!!`
====

Always causes a crash. Useful when testing unfinished code so that you don't
have to worry about type stuff for different conditional cases you have yet to
cover.

`!?` is a natural rune that always crashes.

Produces
--------

Twig: `[%zpzp ~]`

Accepts
-------

Always ignored

Wide form
---------

    !!

Examples
--------

Frequently used as sentinel

    ~zod/try=> !!
    ! exit
    ~zod/try=> =|(a=(unit) ?^(a !! %none))
    %none
    ~zod/try=> :type; =|(a=(unit) ?^(a !! %none))
    %none
    %none
    ~zod/try=> ?+('a' !! %a 1, %b 2)
    1
    ~zod/try=> ?+('c' !! %a 1, %b 2)
    ! exit
