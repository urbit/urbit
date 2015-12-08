`$=` 
====

Adds a name `p` to a type.

`$=` is a tile rune that produces a `%bark`. A `%bark` is a tile that
wraps a [face]() around another tile. Used primarily to add faces to
[nouns]().

Produces
--------

[Tile](): `[%bark p=term q=tile]`

Accepts
------

`p` is a [term]().

`q` is a [tile]().

Tall form
---------

    $=  p
        q

Examples
--------

`a=*` parses as `[%bark %a %noun]`.

    ~zod/try=> *$=(a @)
    a=0
    ~zod/try=> :type; *$=(a @)
    a=0
    a=@
    ~zod/try=> :type; *a=@
    a=0
    a=@
    ~zod/try=> :type; *a=[1 2]
    a=[%1 %2]
    a=[%1 %2]
