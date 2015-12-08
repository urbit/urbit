`$?` 
====

Produce a union between a list of types `p`. The default value (bunt) is the value of the first element.

`$?` produces a fork, called a `%fern`. A `%fern` is a non-empty list of
cases.

Produces
--------

[Tile](): `[%fern p=[i=tile t=(list tile)]]`.

Accepts
-------

`p` is a [list]() of [tiles]().

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
orthoganal cases for the `%axil` tile.

    ~zod/try=> *?(%a %b %c)
    %a
    ~zod/try=> :type; *?(%a %b %c)
    %a
    {%a %b %c}
