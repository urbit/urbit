tissig, `=~`, %tssg
============================

Compose list

`=~` is a synthetic rune that composes a list of twigs.

Produces
--------

Twig: `[%tssg p=tusk]`

Sample
------

`p` is a [`++tusk`](), a list of [twig]()s.

Tall form
---------

    =~    i.p
            i.t.p
            i.t.t.p
        ==

Queenside:

    =~  i.p
        i.t.p
        i.t.t.p
    ==

Wide form
---------

    =~(i.p i.t.p i.t.t.p)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> =~(1 +(.) +(.))
    3
    ~zod/try=> =~([1 2] [. [3 4]] [. [5 6]] [. ~])
    [[[[1 2] 3 4] 5 6] ~]
