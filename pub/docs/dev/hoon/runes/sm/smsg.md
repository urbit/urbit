semsig, `;~`, %smsg
============================

Monad composer

`;!` is a synthetic rune used to compose functions under a monad.

Produces
--------

Twig: `[%smsg p=(list beer)]`

Sample
------

`p` is a [`++list`]() of [`++beer`]().

Tall form
---------

    ;~  p
      i.q
      i.t.q
      i.t.t.q
    ==

Wide form
---------

    ;~(p i.q i.t.q i.t.t.q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> =cmp |=([a=tape b=$+(char tape)] `tape`?~(a ~ (weld (b i.a) t.a)))
    ~zod/try=> ;~(cmp trip)
    <1.xef [a=@ <374.hzt 100.kzl 1.ypj %164>]>
    ~zod/try=> (;~(cmp trip) 'a')
    "a"

`;~` accepts a composer and a nonempty list of gates; at one gate, the
composer is ignored, and the gate applied directly.

    ~zod/try=> (;~(cmp trip |=(a=@ ~[a a])) 'a')
    "aa"
    ~zod/try=> (;~(cmp trip |=(a=@ ~[a a])) '')
    ""

For multiple gates, `;~` uses the composer to connect the gates in
order.

    ~zod/try=> (;~(cmp trip ;~(cmp |=(a=@ ~[a a]) |=(a=@ <(dec a)>))) 'b')
    "97b"
    ~zod/try=> (;~(cmp trip |=(a=@ ~[a a]) |=(a=@ <(dec a)>)) 'b')
    "97b"
    ~zod/try=> (;~(cmp trip |=(a=@ ~[a a]) |=(a=@ <(dec a)>)) '')
    ""
    ~zod/try=> (;~(cmp trip |=(a=@ ~[a a]) |=(a=@ <(dec a)>)) 'a')
    "96a"
    ~zod/try=> (;~(cmp trip |=(a=@ ~[a a]) |=(a=@ <(dec a)>)) 'acd')
    "96acd"

Multiple gates are equivalent to stacked semsigs.

------------------------------------------------------------------------

    [~ 0]
    ~zod/try=> ((slat %p) '~nec')
    [~ 1]
    ~zod/try=> ((slat %p) 'nec')
    ~
    ~zod/try=> ;~(biff (slat %p))
    <1.ags [txt=@ta <1.wqj [mod=@tas <374.hzt 100.kzl 1.ypj %164>]>]>
    ~zod/try=> (;~(biff (slat %p)) '~zod')
    [~ 0]
    ~zod/try=> (;~(biff (slat %p)) '~')
    ~
    ~zod/try=> (;~(biff (slat %p)) '~nec')
    [~ 1]

More commonly, this can be used for unit composition. `(slat %p)` parses
cords in phonetic base, failure case: invalid text

    ~zod/try=> (mo [~nec 12] [~tug 16] ~)
    {[p=~nec q=12] [p=~tug q=16]}
    ~zod/try=> ~(get by (mo [~nec 12] [~tug 16] ~)) 
    <1.yvn [* <18.wzi [a=nlr([p=@p q=@ud]) <374.hzt 100.kzl 1.ypj %164>]>]>
    ~zod/try=> =yaz ~(get by (mo [~nec 12] [~tug 16] ~)) 
    ~zod/try=> (yaz ~nec)
    [~ 12]
    ~zod/try=> (yaz ~zod)
    ~
    ~zod/try=> (yaz ~tug)
    [~ 16]

`mo` makes a map, and `+-get`:by is used to retrieve values from one,
failure case: no such key

    ~zod/try=> (;~(biff (slat %p) yaz) '~zod')
    ~
    ~zod/try=> (;~(biff (slat %p) yaz) '~nec')
    [~ 12]
    ~zod/try=> (;~(biff (slat %p) yaz) 'tug')
    ~
    ~zod/try=> (;~(biff (slat %p) yaz) '~tug')
    [~ 16]

    ~zod/try=> (;~(biff (slat %p) yaz |=(a=@ud [~ (add 100 a)])) '~nec')
    [~ 112]
    ~zod/try=> (;~(biff (slat %p) yaz |=(a=@ud [~ (add 100 a)])) '~zod')
    ~
    ~zod/try=> (;~(biff (slat %p) yaz |=(a=@ud [~ (add 100 a)])) 'mal')
    ~

These can be combined with `biff`, which connects a previous succeeded
unit result by a gate that itself produces a unit from it.

------------------------------------------------------------------------

    ~zod/try=> (hep [1 1] "")
    [p=[p=1 q=1] q=~]
    ~zod/try=> (hep [1 1] "-")
    [p=[p=1 q=2] q=[~ [p=~~- q=[p=[p=1 q=2] q=""]]]]
    ~zod/try=> `(like cord)`(hep [1 1] "-")
    [p=[p=1 q=2] q=[~ [p='-' q=[p=[p=1 q=2] q=""]]]]
    ~zod/try=> `(like cord)`(hep [1 1] "?")
    [p=[p=1 q=1] q=~]
    ~zod/try=> (lus [1 1] "+")
    [p=[p=1 q=2] q=[~ [p=~~~2b. q=[p=[p=1 q=2] q=""]]]]
    ~zod/try=> `(like cord)`(lus [1 1] "+")
    [p=[p=1 q=2] q=[~ [p='+' q=[p=[p=1 q=2] q=""]]]]
    ~zod/try=> `(like cord)`(lus [1 1] "++")
    [p=[p=1 q=2] q=[~ [p='+' q=[p=[p=1 q=2] q="+"]]]]
    ~zod/try=> `(like cord)`(lus [1 1] " +")
    [p=[p=1 q=1] q=~]
    ~zod/try=> `(like cord)`(;~(pose hep lus) [1 1] "?")
    [p=[p=1 q=1] q=~]
    ~zod/try=> `(like cord)`(;~(pose hep lus) [1 1] "-")
    [p=[p=1 q=2] q=[~ [p='-' q=[p=[p=1 q=2] q=""]]]]
    ~zod/try=> `(like cord)`(;~(pose hep lus) [1 1] "+")
    [p=[p=1 q=2] q=[~ [p='+' q=[p=[p=1 q=2] q=""]]]]
    ~zod/try=> `(like cord)`(;~(pose hep lus) [1 1] "a")
    [p=[p=1 q=1] q=~]
    ~zod/try=> `(like cord)`(;~(pose hep lus) [1 1] "-+")
    [p=[p=1 q=2] q=[~ [p='-' q=[p=[p=1 q=2] q="+"]]]]
    ~zod/try=> `(like ,[cord cord])`(;~(plug hep lus) [1 1] "+")
    [p=[p=1 q=1] q=~]
    ~zod/try=> `(like ,[cord cord])`(;~(plug hep lus) [1 1] "-+")
    [p=[p=1 q=3] q=[~ [p=['-' '+'] q=[p=[p=1 q=3] q=""]]]]
    ~zod/try=> `(like ,[cord cord])`(;~(plug hep lus) [1 1] "-+ ")
    [p=[p=1 q=3] q=[~ [p=['-' '+'] q=[p=[p=1 q=3] q=" "]]]]

Most prominently, however, `;~` is used for combinator parsers,
composing [rule]s in various ways

See parsing sections 2eA - 2eH, and specifically section 2eD, for more
information.
