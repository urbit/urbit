`^~`
====

Try executing `p` at compile time

Tries to execute `p` statically at compile time. If this fails,
`p` remains dynamic. Primarily useful for optimization: when a
value is static, `^~` can simplify the compilation.

Produces
--------

Twig: `[%ktsg p=twig]`

Accepts
-------

`p` is a [`++twig`]().

Tall form
---------

    ^~  a

Wide form
---------

    ^~(a)

Examples
--------

    /~zod/try=> (make '|-(42)')
    [%8 p=[%1 p=[1 42]] q=[%9 p=2 q=[%0 p=1]]]
    /~zod/try=> (make '^~(|-(42))')
    [%1 p=42]

Here we use [`++make`]() to examine the nock generated for a particular
computation. We start with a simple kicked trap, `|-`, that just
generates the static value `42`. When we wrap our `|-` in a `^~` you can
see that our compilation is much simpler.
