`%.`
====

Inverts the order of `%-`, so the argument is the first
element and the second element is the function. Exists primarily
for code readability and organization. See the [backstep
section]() for more information.


`%.` is a synthetic rune that reverses the order of [`%-`](). `%.`
exists primarily for code readability and organization, see the [style
guide]().

Produces
--------

Twig: `[%cndt p=twig q=twig]`

Accepts
-------

`p` and `q` are [twig]()s. `p` is the argument and `q` is the function.

Tall form
---------

    %.  p
        q

Wide form
---------

    %.(p q)

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
