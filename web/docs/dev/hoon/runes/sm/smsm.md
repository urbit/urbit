semsem, `;;`, %smsm
============================

Fixpoint

`;;` is a synthetic rune that types `q` as a fixpoint of `p`. More
specifically, `;;` slams `q` through gate `p`, asserting that the
resulting noun [`.=`]() the original, and produces it.

See also
--------

[`++hard`]()

Produces
--------

Twig: `[%smsm p=twig q=twig]`

Sample
------

`p` is a [twig](). `q` is a [twig]().

Tall form
---------

    ;;  p
        q

Wide form
---------

    ;;(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> ^-(tape ~[97 98 99])
    ! type-fail
    ! exit
    ~zod/try=> ;;(tape ~[97 98 99])
    "abc"
    ~zod/try=> (tape [50 51 52])
    "23"
    ~zod/try=> ;;(tape [50 51 52])
    ! exit
