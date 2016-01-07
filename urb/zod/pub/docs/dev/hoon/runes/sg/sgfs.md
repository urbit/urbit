`~/`
====

Hint that jet exists

`~/` is a synthetic rune that implements one common case - a
function arm in a [book](), i.e. a library function - of the
[`~%`]() [jet]() hint. Assumes the parent axis is `7` and that
there are no children.

Produces
--------

Twig: `[%sgfs p=term q=twig]`

Accepts
-------

`p` is a [`++term`](). `q` is a [`++twig`]().

Tall form
---------

    ~/  p
    q

Wide form
---------

    ~/(p q)

Examples
--------

    ~zod/try=> (make '~/  %bam  |.(40)')
    [%8 p=[%1 p=[1 40]] q=[%10 p=[p=1.953.718.630 q=[%1 p=[7.168.354 [0 7] 0]]] q=[%0 p=1]]]
    ~zod/try=> `@`%bam
    7.168.354
    ~zod/try=> `@tas`1.953.718.630
    %fast
