`:-`
====

Cell. Pair. Tuple of two.

Produces
--------

Twig: `[%clhp p=twig q=twig]`

Accepts
-------

`p` is a [++twig](). `q` is a twig.

Tall form
---------

    :-  p
    q

Wide form
---------

    :-(p q)

Irregular form
--------------

    [p q]

Examples
--------

    ~zod/try=> :-(1 2)
    [1 2]
    ~zod/try=> :-  'a'
               %b
    ['a' %b]

This is the most straightforward case of `:-`, producing a cell of
static data in either tall or wide form.

    /~zod/try=> 
        :-  (add 2 2)
        |-  (div 4 2)
    [4 2]

Most commonly `:-` helps to organize code, allowing you to produce a
cell from nested computation.

Some obscure `:-` irregular forms
==================================

### Infix `^`

    ~zod/main=/app> 1^2^3
    [1 2 3]

`a^b` is equivalent to `:-  a  b`

### Infix `/`

    ~zod/main=/app> a/1
    [%a 1]
    ~zod/main=/app> a/'twig'
    [%a 'twig']

Like `^`, but first item must be a [`++term`](), and is
[cube]()d. Used to construct [`path`]()s and [frond]().

### Prefix `` ` ``, pre/postfix `~`

    ~zod/main=/app> ````20
    [~ ~ ~ ~ 20]
    ~zod/main=/app> ~[42 30]
    [42 30 ~]
    ~zod/main=/app> [42 30]~
    [[42 30] ~]

Complementing each other, these construct pairs with `~` in the
[head]() and [`tail`]().  Technically, prefix `~` is an irregular
form of `:~`.
