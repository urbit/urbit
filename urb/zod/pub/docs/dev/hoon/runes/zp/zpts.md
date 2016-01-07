`!=`
====

Show nock

`!=` is a natural rune that produces the nock formula of `p` as a
noun.

Note that `!=(a)` is this rune while `!=(a b)` is a combination
of the irregular forms of `?!` and `.=`.

Accepts
-------

Twig: `[%zpts p=twig]`

Accepts
-------

`p` is a [`++twig`]().

Tall form
---------

    !=  p

Wide form
---------

    !=(p)

Examples
--------

    ~zod/try=> !=(20)
    [1 20]
    ~zod/try=> !=(~zod)
    [1 0]
    ~zod/try=> !=((add 2 2))
    [8 [9 3.110.356 0 31] 9 2 [0 4] [7 [0 3] 1 2 2] 0 11]
    ~zod/try=> !=(|=(a=@ [a a]))
    [8 [1 0] [1 [0 6] 0 6] 0 1]
    ~zod/try=> !=(+(2))
    [4 1 2]
    ~zod/try=> !=(.?(2))
    [3 1 2]
    ~zod/try=> !=(.*(~ 2))
    [2 [1 0] 1 2]
    ~zod/try=> !=(2)
    [1 2]
    ~zod/try=> !=(.)
    [0 1]
    ~zod/try=> !=(!!)
    [0 0]
