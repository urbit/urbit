`|*`
====

Wet function

Similar to `|=` except that it does type checking at
call time, and that the product type is checked to be the same as
the input type (in other words it's [wet]()), rather than the input ([sample]()) [mold]().

Produces
--------

Twig: `[%brtr p=mold q=twig]`

Accepts
-------

`p` is a mold. `q` is a [++twig]().

Tall form
---------

    |*  p
        q

Wide form
---------

    |*(p q)

Examples
--------

    ~zod/try=> %.('c' |*(a=@ a))
    'c'
    ~zod/try=> %.('c' |=(a=@ a))
    99

This is a concise way of understanding the difference between `|*` and
`|=`. We use `%.` in both cases to slam each gate with the sample `'c'`.
`|=` uses its mold `a=@` to cast `'c'` to an atom (`99` is the ASCII
code for `'c'`). `|*` simply ensures that the product type matches the
input sample type.

    ++  flop                                                ::  reverse
          ~/  %flop
          |*  a=(list)
          =>  .(a (homo a))
          ^+  a
          =+  b=`_a`~
          |-
          ?@  a
            b
          $(a t.a, b [i.a b])

In [`++flop`](), `|*` is used so the type information within the passed
in list is maintained. Without a `|*`, any cords would be cast to nouns
as in our previous example.
