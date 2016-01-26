buclus `$+` %bcls
==========================

Signature

`$+` a tile for a gate which accepts `p` and produces `q`. `$+` is
similar to a function signature. `$+(p q)` is a `%bark` of a `%weed`, or
`$_(|+(p _q))`.

Produces
--------

Tile: `$_(|+(p _q))`

Sample
------

`p` is a [tile]() `q` is a [tile]()

Tall form
---------

      $+  p  q

Wide form
---------

      $+(p q)

Irregular form
--------------

None

Examples
--------

      ++  sort                                                ::  quicksort
            ~/  %sort
            !:
            |*  [a=(list) b=$+([* *] ?)]
            =>  .(a ^.(homo a))
            |-  ^+  a
            ?~  a  ~
            %+  weld
              $(a (skim t.a |=(c=_i.a (b c i.a))))
            ^+  t.a
            [i.a $(a (skim t.a |=(c=_i.a !(b c i.a))))]

In ++sort, `$+` is a tile for a comparator gate, which takes two nouns
and produces a loobean.

    ~zod/try=> |=(a=@ (add 2 a))
    <1.sgg [a=@ [[@da @ta] [@p @ta] *''] @n <246.qra 41.uuw 374.glo 100.rip 1.ypj %164>]>
    ~zod/try=> `$+(@ @)`|=(a=@ (add 2 a))
    <1|crm [@ [[@da @ta] [@p @ta] *''] @n <246.qra 41.uuw 374.glo 100.rip 1.ypj %164>]>
    ~zod/try=> +<:|=(a=@ (add 2 a))
    a=0
    ~zod/try=> +<:`$+(@ @)`|=(a=@ (add 2 a))
    ! -axis.6
    ! peek-park
    ! exit
