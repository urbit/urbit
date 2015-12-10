`%-`
====

Call function w/one argument

Call function with one argument. Has an irregular wideform syntax: `(function[arg0 arg1...])`, which can takes n arguments enclosed within the `()`. Note
that one can also pass a tall form (or wide form) function called with `%-` a tuple with `n` elements for functions that require more than one argument.

%- is a synthetic rune that that pulls $ from the door p with its sample set to [%cltr q]. %- in both its tall and wide forms is like a function call with one argument.

Produces
--------

Twig: `[%cnhp p=twig q=tusk]`

Accepts
-------

`p` is a [++twig](), most commonly a function, and in tall form `q` is a twig. In irregular wide form, `q` is a [`++tusk`](), a list of twigs.

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

