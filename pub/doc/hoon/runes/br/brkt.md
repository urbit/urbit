barket, `|^`, %brkt
============================

Kicked book

`|^` is a synthetic rune that produces and then kicks a [`%gold`]()
[book]() with sample `p` as a [`%$(p)`](), and a list of [arm]()s `q`.
The list must be closed with a `--`.

`|^` is similar to `|-`, but differs in that it can contain internal
arms.

See also
--------

barhep, `|-`, %brhp](#brhp) [barcab, `|_`, %brcb

Produces
--------

Twig: `[%brkt p=twig q=(map term foot)]`

Sample
------

`p` is a [twig](). `q` is a [`map`]() with [`++term`]() keys and
[`++foot`]() values, which are called arms.

Tall form
---------

    |^  p
    ++  p.n.q
      q.n.q
    --

Wide form
---------

None

Irregular form
--------------

None

Examples
--------

    /~zod/try=> =+  a=21
                |^  [square double]
                ++  square  (mul a a)
                ++  double  (mul 2 a)
                --
    [441 42]

Here `|^` computes a result by calling its helper arms `++square` and
`++double`. Notice that `a` is also in the context. `|^` is most
commonly used inside another gate or core.

    ++  mum                                                 ::  mug with murmur3
      ~/  %mum
      |=  a=*
      |^  (trim ?@(a a (mix $(a -.a) (mix 0x7fff.ffff $(a +.a)))))
      ++  spec                                              ::  standard murmur3
        |=  [syd=@ key=@]
        ?>  (lte (met 5 syd) 1)
        =+  ^=  row
            |=  [a=@ b=@] 
            (con (end 5 1 (lsh 0 a b)) (rsh 0 (sub 32 a) b))
        =+  mow=|=([a=@ b=@] (end 5 1 (mul a b)))
        =+  len=(met 5 key)
        =-  =.  goc  (mix goc len)
            =.  goc  (mix goc (rsh 4 1 goc))
            =.  goc  (mow goc 0x85eb.ca6b)
            =.  goc  (mix goc (rsh 0 13 goc))
            =.  goc  (mow goc 0xc2b2.ae35)
            (mix goc (rsh 4 1 goc))
        ^=  goc
        =+  [inx=0 goc=syd]
        |-  ^-  @
        ?:  =(inx len)  goc
        =+  kop=(cut 5 [inx 1] key)
        =.  kop  (mow kop 0xcc9e.2d51)
        =.  kop  (row 15 kop) 
        =.  kop  (mow kop 0x1b87.3593)
        =.  goc  (mix kop goc)
        =.  goc  (row 13 goc)
        =.  goc  (end 5 1 (add 0xe654.6b64 (mul 5 goc)))
        $(inx +(inx))
      ::
      ++  trim                                              ::  31-bit nonzero
        |=  key=@
        =+  syd=0xcafe.babe
        |-  ^-  @
        =+  haz=(spec syd key)
        =+  ham=(mix (rsh 0 31 haz) (end 0 31 haz))
        ?.(=(0 ham) ham $(syd +(syd)))
      --
    ::

[`++mum`]() is a hashing gate, which uses two helper arms to computes
its results. The `|^` at the top performs the computation.
