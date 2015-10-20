cencol, `%:`, %cncl
============================

Slam, one argument

`%:` is a synthetic rune that that [pull]()s [`$`]() from the [door]()
`p` with its sample set to `q`. `%:` in the most common case simply
[slam]()s `p` with `q`, similar to a function call with one argument.
Unlike its close relative `%-`, `%:` is designed for [gate]()s who take
a single value as their sample.

Produces
--------

Twig: `[%cncl p=twig q=twig]`

Sample
------

`p` is a [twig](), most commonly a [gate]() `q` is a [twig]()

Tall form
---------

    %:  p
        q

Wide form
---------

    %:(p q)

Irregular form
--------------

None

Examples
--------

    /~zod/try=> %:(dec 42)
    41

Here we use `%:` in its most straightforward form, to call an arm that
takes one argument.

    /~zod/try=> %:  dec
                  %+  add  2
                %+  mul  2  20
    41

Here we call [`++dec`]() with the sum of `2` and the product of `2` and
`20` using `%:`. As you can see, `%:` is most useful for code
organization, when you need to compute intermediate products for your
final result
