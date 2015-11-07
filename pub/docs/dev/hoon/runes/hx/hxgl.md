haxgal, `#<`, %hxgl
============================

Prettyprint `++tape`

`#<` is a synthetic rune that slams the gate [`++noah`]() with `p`. `#<`
is used to produce a [`++tape`]() of the computation passed to it, and
is only used in the irregular form.

Produces
--------

Twig: `[%hxgl p=tusk]`

Sample
------

`p` is a [++tusk]().

Tall form
---------

None

Wide form
---------

None

Irregular form
--------------

    <i.p i.t.p i.t.t.p>

Examples
--------

    ~zod/try=> <1>
    "1"

This is the simplest example of `#<`, pass it a constant and it produces
a `++tape` of the constant.

    ~zod/try=> <`path`"abc">
    "/a/b/c"
    ~zod/try=> <0x5 ~zod 'a'>
    "[0x5 ~zod 'a']"

In these cases we use `#<` to produce the result of a simple computation
as a `++tape`.

    ~zod/try=> <|.(1)>
    "<1.gcq [@n <250.yur 41.wda 374.hzt 100.kzl 1.ypj %164>]>"

Here you can see what would normally be the pretty-printed result
produced as a `++tape`.

    ~zod/try=> (ream '<~>')
    [%hxgl p=~[[%bczp p=%null]]]

Using `++ream` to unpack how `#<` is parsed we can see that its sample
is wrapped in a [`$!`]().
