colhep, `:-`, %clhp
============================

Cell

`:-`, `colhep`, `[%clhp p=twig q=twig]` is a synthetic rune that
produces the cell `[p q]`.

Produces
--------

Twig: `[%clhp p=twig q=twig]`

Sample
------

`p` is a [twig](). `q` is a [twig]().

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

Some obscure `:-` irregular forms.
==================================

Moveme: irregular form doc

### Infix `^`

    ~zod/main=/app> 1^2^3
    [1 2 3]

`a^b` is equivalent to `:-  a  b`

### Infix `/`

    ~zod/main=/app> a/1
    [%a 1]
    ~zod/main=/app> a/'twig'
    [%a 'twig']

Like `^`, but first item must be a term, and is cubed. Used to construct
paths and fronds.

### Prefix `` ` ``, postfix `~`

    ~zod/main=/app> ````20
    [~ ~ ~ ~ 20]
    ~zod/main=/app> [42 30]~
    [[42 30] ~]

Complimenting each other, these construct pairs with nil in the head and
tail respectively. Multiple postfix `~` do not work for unknown reasons.
