`$+`
====

Type of function (type signature)

Produces a [mold]() for a function that accepts `p` and produces
`q`. Used when we want to use the type of a function that we do
not want to immediately execute.

Reduces to `$_(|+(p *q))`.

Produces
--------

A mold whose [span]() is used to validate the types of functions.

Accepts
-------

`p` is the mold of the [argument]() ([sample]()) and `q` is a mold of the product.

Tall form
---------

      $+  p  q

Wide form
---------

      $+(p q)

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

In ++sort, `$+` is a mold for a comparator gate, which takes two nouns
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
