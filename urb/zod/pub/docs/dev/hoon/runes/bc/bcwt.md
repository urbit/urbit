`$?` 
====

Union of types

Produce a union between a non-empty list of [mold]()s `p`. The default value ([bunt]()) is the value of the first element.

Produces
--------

A validator function that keeps trying to validate a value using every mold in `p` from left to right. On a success, the value is produced. If no mold in `p` succeeds, the default value of the last element in `p` is produced.

Accepts
-------

`p` is a [++list]() of mold.

Tall form
---------

    $?  p
        q
    ==

Irregular form
--------------

    ?(p q)

Examples
--------

    ++  base  ?([%atom p=odor] %noun %cell %bean %null)     ::  axils, @ * ^ ? ~

`++base`, `?` (the irregular form of `$?`) specifies a list of
orthoganal cases for the `%axil` mold.

    ~zod/try=> *?(%a %b %c)
    %a
    ~zod/try=> :type; *?(%a %b %c)
    %a
    {%a %b %c}
