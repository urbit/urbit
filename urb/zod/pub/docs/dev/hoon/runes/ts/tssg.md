`=~`
====

Composes a list of twigs. Applies `=>` to a list of expressions, using each result as the
subject to the following expression.

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

Examples
--------

    ~zod/try=> =~(1 +(.) +(.))
    3
    ~zod/try=> =~([1 2] [. [3 4]] [. [5 6]] [. ~])
    [[[[1 2] 3 4] 5 6] ~]
