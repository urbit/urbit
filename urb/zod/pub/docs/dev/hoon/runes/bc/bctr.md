`$*` 
====

Bunt (default value)

Produces the default value ([bunt]()) of [mold]() `p`. Does so as a compile-time constant if possible. If not possible, then it is produced dynamically.

Produces
--------

The default value (bunt) of the mold `p`.

Accepts
-------

A mold `p`.

Tall form
---------

    $*  p

Wide form
---------

    $*(p)

Irregular form
--------------

    *p

Examples
--------

    ~zod/try=> *@t
    ''
    ~zod/try=> *[@p @ux]
    [~zod 0x0]
    ~zod/try=> *(list ,@)
    ~
