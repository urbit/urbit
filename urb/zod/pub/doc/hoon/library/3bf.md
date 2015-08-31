section 3bF, filesystem interface
=================================

### `++feel`

Generate file diff

    ++  feel                                                ::  simple file write
      |=  [pax=path val=*]
      ^-  miso
      =+  dir=((hard arch) .^(%cy pax))
      ?~  q.dir  [%ins val]
      :-  %mut
      ^-  udon
      [%a %a .^(%cx pax) val]
    ::

Generates a diff between a file located at `pax` and an input value
`val`.

`pax` is a [`++path`]().

`val` is a value as a [noun]().

    ~zod/try=> + %/mel 'test'
    + /~zod/try/2/mel
    ~zod/try=> (feel %/mel 'tesh?')
    [%mut p=[p=%a q=[%a p=44.903.392.628 q=272.335.332.724]]]
    ~zod/try=> `@t`44.903.392.628
    '''
    test
    '''
    ~zod/try=> `@t`272.335.332.724
    'tesh?'

### `++file`

Simple file load

    ++  file                                                ::  simple file load
      |=  pax=path
      ^-  (unit)
      =+  dir=((hard arch) .^(%cy pax))
      ?~(q.dir ~ [~ .^(%cx pax)])
    ::

Reads the value of a file located at `pax` and renders it as a
[`++unit`]().

`pax` is a [`++path`]().

    ~zod/try=> %/zak
    ~zod/try=/zak> :ls %
    ~zod/try=/zak> + %/mop 20
    + /~zod/try/3/zak/mop
    ~zod/try=/zak> :ls %
    mop
    ~zod/try=/zak> (file %/mop)
    [~ 20]
    ~zod/try=/zak> (file %/lak)
    ~
    ~zod/try=/zak> (file /==2%/mop)
    ~

### `++foal`

Write high-level change

    ++  foal                                                ::  high-level write
      |=  [pax=path val=*]
      ^-  toro
      ?>  ?=([* * * *] pax)
      [i.t.pax [%& [*cart [[t.t.t.pax (feel pax val)] ~]]]]
    ::

Produces a [`++toro`](), a change intended for whatever file is located
at `pax`. Handled by `%clay`.

`pax` is a [`++path`]().

`val` is a value as a [noun]().

    ~zod/try=> + %/mek 'a'
    + /~zod/try/4/mek
    ~zod/try=> (foal %/mek 'b')
    [ p=~.try
        q
      [%.y q=[p=[p=0v0 q=0v0] q=~[[p=/mek q=[%mut p=[p=%a q=[%a p=97 q=98]]]]]]]
    ]
    ~zod/try=> (feel %/mek 'b')
    [%mut p=[p=%a q=[%a p=97 q=98]]]

### `++fray`

High-level delete

    ++  fray                                                ::  high-level delete
      |=  pax=path
      ^-  toro
      ?>  ?=([* * * *] pax)
      [i.t.pax [%& [*cart [[t.t.t.pax [%del .^(%cx pax)]] ~]]]]
    ::

Produces a deletion [`++toro`]() for a file located at path `pax`.
Handled by `%clay`.

`pax` is a [`++path`]().

    ~zod/try=> + %/mek 'a'
    + /~zod/try/4/mek
    ~zod/try=> (fray %/mek)
    [p=~.try q=[%.y q=[p=[p=0v0 q=0v0] q=~[[p=/mek q=[%del p=97]]]]]]
    ~zod/try=> `@t`97
    'a'

### `++furl`

Unify changes

    ++  furl                                                ::  unify changes
      |=  [one=toro two=toro]
      ^-  toro
      ~|  %furl
      ?>  ?&  =(p.one p.two)                                ::  same path
              &(?=(& -.q.one) ?=(& -.q.two))                ::  both deltas
          ==
      [p.one [%& [*cart (weld q.q.q.one q.q.q.two)]]]
    ::

Merge two [`++toro`]()s `one` and `two` that are in the same [`desk`]()
and pointed at the same [`++path`]().

`one` is a [`++toro`]().

`two` is a [`++toro`]().

    ~zod/try=> %/zak
    ~zod/try=/zak> :ls %
    mop
    ~zod/try=/zak> (furl (fray %/mop) (foal %/mok 'hi'))
    [ p=~.try
        q
      [ %.y
          q
        [ p=[p=0v0 q=0v0]
          q=~[[p=/zak/mop q=[%del p=20]] [p=/zak/mok q=[%ins p=26.984]]]
        ]
      ]
    ]

### `++meat`

Kite to .\^ path

    ++  meat                                                ::  kite to .^ path
      |=  kit=kite
      ^-  path
      [(cat 3 'c' p.kit) (scot %p r.kit) s.kit (scot `dime`q.kit) t.kit]
    ::

Converts a type request name to a [`++path`]().

`kit` is a [`++kite`]().

    zod/try=/zop> `kite`[%x ud/1 ~zod %main /sur/down/gate/hook]
    [p=%x q=[%ud p=1] r=~zod s=%main t=/sur/down/gate/hook]
    ~zod/try=/zop> (meat [%x ud/1 ~zod %main /sur/down/gate/hook])
    /cx/~zod/main/1/sur/down/gate/hook
    ~zod/try=/zop> .^((meat [%x ud/1 ~zod %main /sur/down/gate/hook]))
    8.024.240.839.827.090.233.853.057.929.619.452.695.436.878.709.611.140.677.
    745.908.646.440.925.885.935.296.374.867.974.972.908.054.571.544.099.882.490.
    677.391.983.737.511.220.072.391.888.081.664.570
    ~zod/try=/zop> (,@t .^((meat [%x ud/1 ~zod %main /sur/down/gate/hook])))
    '''
    ::
    ::::  /hoon/gate/down/sur
    ::
    /?  314
    /-  *markdown
    down

    '''

### `++tame`

Parse kite path

    ++  tame                                                ::  parse kite path
      |=  hap=path
      ^-  (unit kite)
      ?.  ?=([@ @ @ @ *] hap)  ~
      =+  :*  hyr=(slay i.hap)
              fal=(slay i.t.hap)
              dyc=(slay i.t.t.hap)
              ved=(slay i.t.t.t.hap)
              ::  ved=(slay i.t.hap)
              ::  fal=(slay i.t.t.hap)
              ::  dyc=(slay i.t.t.t.hap)
              tyl=t.t.t.t.hap
          ==
      ?.  ?=([~ %$ %tas @] hyr)  ~
      ?.  ?=([~ %$ %p @] fal)  ~
      ?.  ?=([~ %$ %tas @] dyc)  ~
      ?.  ?=([~ %$ case] ved)  ~
      =+  his=`@p`q.p.u.fal
      =+  [dis=(end 3 1 q.p.u.hyr) rem=(rsh 3 1 q.p.u.hyr)]
      ?.  ?&(?=(%c dis) ?=(?(%v %w %x %y %z) rem))  ~
      [~ rem p.u.ved q.p.u.fal q.p.u.dyc tyl]
    ::

Parses a clay [.\^]()
[`++path` ]()to request details. Produces the [`++unit`]() of a [`++kite`]().

`hap` is a [`++path`]().

    ~zod/try=/zop> (tame /cx/~zod/main/1/sur/down/gate/hook)
    [~ [p=%x q=[%ud p=1] r=~zod s=%main t=/sur/down/gate/hook]]
    ~zod/try=/zop> (tame /cx/0/main/1/sur/down/gate/hook)
    ~
    ~zod/try=/zop> (tame /~zod/main/0x12/sur/down/gate/hook)
    ~

### `++tome`

Parse path to beam

    ++  tome                                                ::  parse path to beam
      |=  pax=path
      ^-  (unit beam)
      ?.  ?=([* * * *] pax)  ~
      %+  biff  (slaw %p i.pax)
      |=  who=ship
      %+  biff  (slaw %tas i.t.pax)
      |=  dex=desk
      %+  biff  (slay i.t.t.pax)
      |=  cis=coin
      ?.  ?=([%$ case] cis)  ~
      `(unit beam)`[~ [who dex `case`p.cis] (flop t.t.t.pax)]
    ::

Parses a [`++path`]() `pax` to a [\`++beam](), a well-typed location.

    ~zod/try=/zop> (tome /~fyr/try/2/for/me)
    [~ [[p=~fyr q=%try r=[%ud p=2]] s=/me/for]]
    ~zod/try=/zop> (tome /~zod/main/1)
    [~ [[p=~zod q=%main r=[%ud p=1]] s=/]]
    ~zod/try=/zop> (tome /0/main/1)
    ~
    ~zod/try=/zop> (tome /~zod/main/0x12)
    ~

### `++tope :: beam to path`

Parse beam to path

    |=  bem=beam
      ^-  path
      [(scot %p p.bem) q.bem (scot r.bem) (flop s.bem)]

Parses a [`++beam`]() to a [`++path`](/doc/hoon/library/1#++path).

    ~zod/try=/zop> (tope [~zod %main ud/1] /hook/down/sur)
    /~zod/main/1/sur/down/hook
    ~zod/try=/zop> (tope [~fyr %try da/~2015.1.1] /txt/test)
    /~fyr/try/~2015.1.1/test/txt
    ~zod/try=/zop> (tope [~doznec %try da/-<-] /txt/test)
    /~doznec/try/~2014.10.30..00.32.48..3ae4/test/txt
