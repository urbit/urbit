zapwut, `!?`, %zpwt
============================

Restrict version

`!?` is a synthetic rune that enforces a Hoon version restriction.

Produces
--------

Twig: `[%zpwt p=twig]`

Sample
------

`p` is a [twig]().

Tall form
---------

To declare code that runs only in Hoon 164K or newer:

    !?  164
    q

To declare code that runs only in 164K through 161K:

    !?  [164 161] 
    q

Wide form
---------

None

Irregular form
--------------

None

Examples
--------

    ~zod/try=> !?(264 (add 2 2))
    4
    ~zod/try=> !?(164 (add 2 2))
    4
    ~zod/try=> !?(163 (add 2 2))
    ! exit
