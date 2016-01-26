semdoq, %smdq
======================

String

`semdoq` is a synthetic rune used to make strings, interpreted or not.

Produces
--------

Twig: `[%smdq p=(list beer)]`

Sample
------

`p` is a [list]() of [`++beer`]().

Tall form
---------

None

Wide form
---------

None

Irregular form
--------------

    "foo"

Examples
--------

    ~zod/try=> "foo"
    "foo"
    ~zod/try=> "bar"
    "bar"

Here we see our most common case of `semdoq`, producing [`++tape`]s.

    ~zod/try=> "ba{<+(2)>}r"
    "ba3r"

In this case we include some simple string interpolation using `{` and
`}`.

    ~zod/try=> (ream '"foo"')
    [%smdq p=~[102 111 111]]
    ~zod/try=> (ream '"ba{<+(2)>}r"')
    [ %smdq
      p=~[98 97 [~ p=[%cltr p=~[[%hxgl p=~[[%dtls p=[%dtzy p=%ud q=2]]]]]]] 114]
    ]

Using [`++ream`]() we can see how our previous expressions are parsed.
