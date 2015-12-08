ketwut, `^?`, %ktwt
============================

Used to convert a core whose context is unreadable (aka
a %gold core) to a core both of whose context and sample are
unreadable. XX type reasons


`^?` is a natural rune that converts a `%gold` core into a `%lead` core.

Produces
--------

Twig: `[%ktwt p=twig]`

Sample
------

`p` is a [twig]().

Tall form
---------

    ^?  p

Wide form
---------

    ^?(p)

Examples
--------

    ~zod/try=> |=(@ 1)
    <1.gcq [@  @n <250.yur 41.wda 374.hzt 100.kzl 1.ypj %164>]>
    ~zod/try=> ^?(|=(@ 1))
    <1?gcq [@  @n <250.yur 41.wda 374.hzt 100.kzl 1.ypj %164>]>
