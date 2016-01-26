buctar `$*` %bctr
==========================

Default value

`$*`, is a synthetic rune that produces the [bunt]() (default value) of
a tile as a compile-time constant if possible. If it is not possible,
then it is produced dynamically.

Produces
--------

[Twig](): `[%bctr p=tile]`

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
