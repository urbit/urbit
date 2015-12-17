ketwut, `^?`, %ktwt
============================

Used to convert a [core]() to a core whose both context and
sample are unreadable (aka a `%lead` core).  This is primarily
used on cores which have no sample, in which case it's equivalent
to `^|` and `^&`.  It's useful in allowing cores with different
contexts to be considered of the same type.

Produces
--------

Twig: `[%ktwt p=twig]`

Sample
------

`p` is a [`++twig`]().

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
