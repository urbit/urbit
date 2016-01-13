coltar, `:*`, %cltr
============================

Tuple

`:*`, `coltar`, `[%cltr p=tusk]` is a synthetic rune that produces a
tuple.

Produces
--------

Twig: `[%cltr p=tusk]`

Sample
------

`p` is a [`++tusk`](), a list of twigs.

Tall form
---------

    :~  i.p
        i.t.p
        i.t.t.p
    ==

Wide form
---------

    :*(i.p i.t.p i.t.t.p)

Irregular form
--------------

    [i.p i.t.p i.t.t.p]

Examples
--------

    /~zod/try=> :*(5 3 4 1 4 9 0 ~ 'a')
    [5 3 4 1 4 9 0 ~ 'a']
    /~zod/try=> [5 3 4 1 4 9 0 ~ 'a']
    [5 3 4 1 4 9 0 ~ 'a']
    /~zod/try=> :*  5
                    3
                    4 
                    1
                    4
                    9
                    0
                    ~
                    'a'
                ==
    [5 3 4 1 4 9 0 ~ 'a']

This is the most straightforward case of `:*`, producing a tuple of n
values in wide, irregular and tall form.
