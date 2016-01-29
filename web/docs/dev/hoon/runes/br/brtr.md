bartar, `|*`, %brtr
============================

Wet gate

`|*` is a synthetic rune that produces a [wet]() [gate]() with sample
[`$*(p)`](), arm `q`. A gate is a core with one arm, [`$`](), the empty
name.

`|*` is similar to a function in the same way that `|=` is, but differs
in that it does type checking at runtime. With `|*`, the product type is
checked to be the same as the input type, rather than the sample tile.

See also
--------

bartis, `|=`, %brts
============================

Produces
--------

Twig: `[%brtr p=tile q=twig]`

Sample
------

`p` is a [tile](). `q` is a [twig]().

Tall form
---------

    |*  p
        q

Wide form
---------

    |*(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> %.('c' |*(a=@ a))
    'c'
    ~zod/try=> %.('c' |=(a=@ a))
    99

This is a concise way of understanding the difference between `|*` and
`|=`. We use `%.` in both cases to slam each gate with the sample `'c'`.
`|=` uses its tile `a=@` to cast `'c'` to an atom (`99` is the ASCII
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
