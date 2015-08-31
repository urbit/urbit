zapfas, `!/`, %zpfs
============================

Report as error

`!/` is a virtual natural rune that should never be compiled. When
compiled with error checking turned on, it reports its subject as an
error.

Produces
--------

Twig: `[%zpfs p=twig q=twig]`

Sample
------

`p` and `q` are [twig]()s.

Tall form
---------

    !/  p
        q

Wide form
---------

    !/(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> (~(mint ut %noun) %noun [%zpfs %dtzz %tas %foo])
    ! -lost.%foo
    ! mint-lost
    ! exit
    ~zod/try=> (~(mint ut %noun) %noun [%zpfs (ream '=+(a=5 a)')])
    ! -lost.@ud
    ! mint-lost
    ! exit
    ~zod/try=> (ream '?-(. @ 1)')
    [ %wthz
      p=[%.y p=~ q=~[[%.y p=1]]] q=~[[p=[%axil p=[%atom p=~.]] 
      q=[%dtzy p=%ud q=1]]]
    ]
    ~zod/try=> ~(open ap ~(open ap (ream '?-(. @ 1)')))
    [ %wtcl
      p=[%wtts p=[%axil p=[%atom p=~.]] q=~[[%.y p=1]]] 
      q=[%dtzy p=%ud q=1] r=[%zpfs p=[%cnzz p=~[[%.y p=1]]]]
    ]
    ~zod/try=> (~(mint ut %noun) %noun (ream '?-(. @ 1)'))
    ! -lost.[* *]
    ! mint-lost
    ! exit
    ~zod/try=> (~(mint ut %noun) %noun [%zpfs bczp/%cell])
    ! -lost.[* *]
    ! mint-lost
    ! exit
