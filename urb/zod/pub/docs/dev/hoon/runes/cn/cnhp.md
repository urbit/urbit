`%-`
====

Call function w/one argument

Call function with one argument. Has an irregular wideform
syntax: `(function [arg0 arg1...])`, which takes `n` arguments
enclosed within the `()`. Note that one can also pass a function
`n` arguments with regular tall or wide form `%-` with a tuple
with `n` elements.

`%-` is a synthetic rune that that pulls `$` from the door `p`
with its `%sample` set to `[%cltr q]`.

Produces
--------

Twig: `[%cnhp p=twig q=tusk]`

Accepts
-------

`p` is a [++twig](), most commonly a function, and in tall form
`q` is a twig. In irregular wide form, `q` is a [`++tusk`](), a
list of twigs.

Tall form
---------

    %-  p
        q
    ==

Wide form
---------

    %-(p q)

Irregular form
--------------

    (p q)

Examples
--------

    > %-(dec 4)
    3

    > (add 2 2)
    4

