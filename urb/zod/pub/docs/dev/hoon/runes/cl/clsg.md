`:~`
====

Null-terminated tuple of n elements

Closed with `==`.

Produces
--------

Twig: `[%clsg p=tusk]`

Accepts
-------

`p` is a [`++tusk`](), a list of [`++twig`]()s.

Tall form
---------

    :~  i.p
        i.t.p
        i.t.t.p
    ==

Wide form
---------

    :~(i.p i.t.p i.t.t.p)

Irregular form
--------------

    ~[i.p i.t.p i.t.t.p]

Examples
--------

    /~zod/try=> :~(5 3 4 2 1)
    [5 3 4 2 1 ~]
    /~zod/try=> ~[5 3 4 2 1]
    [5 3 4 2 1 ~]
    /~zod/try=> :~  5
                    3
                    4
                    2
                    1
                ==
    [5 3 4 2 1 ~]

This is the most straightforward case of `:~`, producing a tuple in wide, irregular and tall form.

    /~zod/try=> %-  flop
                %-  limo
                :~  5
                    3
                    4
                    2
                    1
                    ==
    ~[1 2 4 3 5]

In this example we use `%-` to pass the results of our previous example
to [`++limo`](), which creates a [`++list`](), and [`++flop`](), which
reverses its order. This example shows how `:~` is commonly useful.
Null-terminated tuples are easily converted to lists, which are
frequently encountered in hoon.
