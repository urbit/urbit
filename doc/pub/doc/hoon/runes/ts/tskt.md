[tisket, `=^`, %tskt](#tskt)
============================

[Short description]

`=^` is a synthetic rune that handles a product which is a cell of a new
result, and a mutation to the subject.

Produces
--------

Twig: `[%tskt p=twig q=twig r=twig s=twig]`

Sample
------

`p`, `q`, `r` and `s` are [twig]()s.

Tall form
---------

Kingside:

    =^    p 
        q
      r
    s

Queenside:

    =^  p  q
      r
    s

Wide form
---------

    =^(p q r s)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> =+  a=3
               =^  b  a  [a +(a)]
               [a ~ b]
    [4 ~ 3]
    ~zod/try=> =+  a="ham"
               =^  b  a  a
               [<`@tas`b> a]
    ["%h" "am"]
