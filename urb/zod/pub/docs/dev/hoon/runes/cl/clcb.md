`:_`
====

Inverted tuple of `p` `q`

Exists for code readability and organization. See the
[backstep]() convention.

Produces
--------

Twig: `[%clcb p=twig q=twig]`

Accepts
-------

`p` is a [++twig](). `q` is a twig.

Tall form
---------

    :_  p
    q

Wide form
---------

    :_(p q)

Examples
--------

    ~zod/try=> :_(1 2)
    [2 1]

A simple example. `:_` produces the cell of any two twigs, but in
reverse order.

    ~zod/try=> `tape`:_(~ 'a')
    "a"

Since a [`++tape`]() is a null-terminated list of characters, casting
the result of `:_(~ 'a')` to a `tape` produces `"a"`.

    /~zod/try=> 
        :_  (add 2 2)
        |-  (div 4 2)
    [2 4]

Most commonly `:_` helps to organize code, allowing you to produce a
cell from nested computation.
