cendot, `%.`, %cndt
============================

Slam, reverse order

`%.` is a synthetic rune that reverses the order of [`%-`](). `%.`
exists primarily for code readability and organization, see the [style
guide]().

Produces
--------

Twig: `[%cndt p=twig q=twig]`

Sample
------

`p` and `q` are [twig]()s.

Tall form
---------

    %.  p
        q

Wide form
---------

    %.(p q)

Irregular form
--------------

None

Examples
--------

    /~zod/try=> (dec 42)
    41
    ~zod/try=> %.(42 dec)
    41

In the most straightforward case `%.` allows us to reverse the order of
arm and arguments.

    /~zod/try=> %.
                  %+  add
                    %+  mul  2  20
                  2
                dec
    41

Here we add `2` to the product of `2` and `20`, and use `%.` to
decrement our result. As you can see, `%.` is most useful for code
organization, when you need to compute intermediate products for your
final result.

Equivalent to
-------------

    %-(q p)
