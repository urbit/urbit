`!!`
====

Crash

Always causes a crash. Useful when testing unfinished code so
that you don't have to worry about type stuff for different
conditional cases you have yet to cover.

Frequently used as sentinel, especially when you don't want the
type system to give you type fails before you've written every
possible branch of the computation.  The type of `!!` is `%void`,
which nests within every other type.

Produces
--------

Twig: `[%zpzp ~]`

Accepts
-------

N/A (always ignored)

Wide form
---------

    !!

Examples
--------

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
