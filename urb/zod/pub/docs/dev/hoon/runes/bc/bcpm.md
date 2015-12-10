`$&` 
====

Union of two cells: one w/cellular head & other w/atomic head

Produces a [mold]() that's a union between two different kinds of noun: a cell whose head is a cell (`p`), and cell whose head is an atom (`q`). Its default value ([bunt]()) is the value of `q`. One important use of `$&` is to implement autocons in [`++twig`]().

Produces
--------

A validator function that uses mold `p` to validate cells whose head is a cell, and `q` to validate cells whose head is an atom. Default value is the bunt of `q`.

Accepts
-------

A mold of a cell whose head is a cell `p` and a mold of a cell whose head is an atom `q`.

Tall form
---------

    $&  p
        q

Wide form
---------

    $&(p q)

Examples
--------
