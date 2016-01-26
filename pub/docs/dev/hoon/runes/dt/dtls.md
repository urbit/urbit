dotlus, `.+`, %dtls
============================

Increment

`.+` is a natural rune that generates nock operator `4`, which
increments an atomic operand.

Produces
--------

Twig: `[%dtls p=twig]`

Sample
------

`p` is a [twig]()

Tall form
---------

    .+  p

Wide form
---------

    .+(p)

Irregular form
--------------

    +(p)

Examples
--------

    ~zod/try=> +(6)
    7
    ~zod/try=> +(2)
    3

In the most straightforward case `.+` increments its operand.

    ~zod/try=> +(~zod)
    1
    /~zod/try=> `@`~zod
    0
    /~zod/try=> `@`'a'
    97    
    ~zod/try=> +('a')
    98
    /~zod/try=> `@`0xff
    255    
    ~zod/try=> +(0xff)
    256
    ~zod/try=> +(41)
    42

When passed a non-atomic odored atoms `.+` down-casts to an atom.

    ~zod/try=> +([1 2])
    ! type-fail
    ! exit

Passing an operand that cannot be directly down-cast to an atom produces
a type-fail.
