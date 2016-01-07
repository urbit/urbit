`$=` 
====

Add variable name

Wraps a [++face]() around another [mold](). Used primarily to add
variable names to values.

Produces
--------

Produces a [mold]() that validates the input value with `q`,
before adding face `p`.

Accepts
------

`p` is a [++term]().

`q` is a mold.

Tall form
---------

    $=  p
    q

Wide form
---------

        $=(p q)

Irregular form
--------------

        `p+q` 

Examples
--------

`a+*` parses as `[%bark %a %noun]`.

    ~zod/try=> *$=(a @)
    a=0
    ~zod/try=> :type; *$=(a @)
    a=0
    a=@
    ~zod/try=> :type; *a+@
    a=0
    a=@
    ~zod/try=> :type; *a+[1 2]
    a=[%1 %2]
    a=[%1 %2]

    |_  [hid=bowl vat=axle]
    ...
    --

See here how we wrap the faces 'hid' and 'vat' around the two
[sample]() (input) types of the core above. We can now refer to
our 'bowl' and 'vat' by these names.


