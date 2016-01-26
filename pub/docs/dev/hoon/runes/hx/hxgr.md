haxgar, `#>`, %hxgr
============================

Prettyprint `++tank`

`#>`, `haxgar`, `[%hxgr p=tusk]`is a synthetic rune that slams the
assumed gate `cain` on `[%zpgr %cntr p]`.

`#>` is a synthetic rune that slams the gate [`++cain`]() with `p`. `#>`
is used to produce a [`++tank`]() of the computation passed to it, and
is only used in the irregular form.

Produces
--------

Twig: `[%hxgr p=tusk]`

Sample
------

`p` is a [++tusk]().

Tall form
---------

None

Wide form
---------

None

Irregular form
--------------

    >i.p i.t.p i.t.t.p<

Examples
--------

    ~zod/try=/zop> >1<
    [%leaf p="1"]

This is the simplest example of `#>`, pass it a constant and it produces
a `++tank` of the constant.

    ~zod/try=/zop> >`path`"abc"<
    [%rose p=[p="/" q="/" r=""] q=~[[%leaf p="a"] [%leaf p="b"] [%leaf p="c"]]]
    ~zod/try=/zop> >0x5 ~zod 'a'<
    [ %rose
      p=[p=" " q="[" r="]"]
      q=~[[%leaf p="0x5"] [%leaf p="~zod"] [%leaf p="'a'"]]
    ]
    ~zod/try=/zop> >|.(1)<
    [ %rose
      p=[p=" " q="<" r=">"]
        q
      ~[
        [%leaf p="1.gcq"]
        [ %rose
          p=[p=" " q="[" r="]"]
            q
          ~[
            [%leaf p="@n"]
            [ %rose
              p=[p=" " q="<" r=">"]
                q
              ~[
                [%leaf p="250.xnq"]
                [%leaf p="41.vrk"]
                [%leaf p="418.ssa"]
                [%leaf p="101.jzo"]
                [%leaf p="1.ypj"]
                [%leaf p="%164"]
              ]
            ]
          ]
        ]
      ]
    ]

In these cases we use `#>` to produce the result of a simple computation
as a `++tank`.
