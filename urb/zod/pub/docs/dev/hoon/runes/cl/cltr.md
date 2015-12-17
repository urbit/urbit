`:*`
====

Tuple of n elements

Closed with `==`.

Produces
--------

Twig: `[%cltr p=tusk]`

Accepts
-------

`p` is a [`++tusk`](), a list of [++twig]()s.

Tall form
---------

    :*  i.p
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

This is the most straightforward case of `:*`, producing tuples of n
values in wide, irregular and tall forms.
