/+  *test
=/  gimme=$-(cord (pair type hoon))
  |=  =cord
  =-  [p=(~(play ut %noun) -) q=-]
  (ream cord)
::
=/  foo  %-  gimme
  '=>  |%  ++  foo  ~  ++  bar  0  --  [a=~ b=[p=~ q=.]]'
=/  bar  %-  gimme
  '=+(p=[a=1 b=2] =*(zop a.p =*(bop b.p |=([a=@ b=@] [[b zop] [a bop]]))))'
=/  baz  %-  gimme
  '=>  |%  ++  foo  ~  --  =*  zop  .  [a=~ b=[p=~ q=zop]]'
=/  qux  %-  gimme
  '=*  zop  |=(a=@ +(a))  zop'
=/  fiz  %-  gimme
  '=/  foo  |=(a=@ +(a))  =,  foo  =>  |%  ++  hi  |=(a=@ (^$ a))  --  hi'
=/  biz  %-  gimme
  '=*(zop ~ |=(a=@ zop))'
=>
|%
+$  pony                                                ::  raw match
  $@  ~                                                 ::  void
  %+  each                                              ::  natural/abnormal
    palo                                                ::  arm or leg
  %+  each                                              ::  abnormal
    @ud                                                 ::  unmatched
  (pair type nock)                                      ::  synthetic
--
|%
++  test-fond-leg-match  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  `pony`[%.y p=[p=~[~ [~ 12]] q=[%.y p=[%atom p=%$ q=~]]]]
      !>  (~(fond ut p.bar) %both ~[%a])
    %+  expect-eq
      !>  `pony`[%.y p=[p=~[~ [~ 2] ~ [~ 14] ~ ~] q=[%.y p=[%atom p=%ud q=~]]]]
      !>  (~(fond ut p.bar) %both ~[%a %p])
  ==
::
++  test-fond-arm-match  ^-  tang
  =/  pony  (~(fond ut p.foo) %both ~[%foo %q %b])
  =/  puny  (~(fond ut p.foo) %both ~[%bar %q %b])
  ?>  ?=([%& * %| *] pony)
  ?>  ?=([%& * %| *] puny)
  =/  =vein  ~[[~ 1] ~ [~ 3] ~ [~ 3]]
  ;:  weld
    %+  expect-eq
      !>  vein
      !>  p.p.pony
    %+  expect-eq
      !>  `axis`5
      !>  p.q.p.pony
    %+  expect-eq
      !>  vein
      !>  p.p.puny
    %+  expect-eq
      !>  `axis`4
      !>  p.q.p.puny
  ==
::
++  test-fond-leg-match-under-macro  ^-  tang
  ?>  ?=([%core [%cell *] *] p.bar)
  ;:  weld
    %+  expect-eq
      !>  ^-  pony
      [%.y p=[p=~[~ [~ 2] ~ [~ 2] ~ ~ [~ 1] ~] q=[%.y p=[%atom p=%ud q=~]]]]
      !>  (~(fond ut q.p.p.bar) %both ~[%zop])
    %+  expect-eq
      !>  ^-  pony
      [%.y p=[p=~[~ [~ 3] ~ [~ 2] ~ ~ ~ [~ 1]] q=[%.y p=[%atom p=%ud q=~]]]]
      !>  (~(fond ut q.p.p.bar) %both ~[%bop])
  ==
::
++  test-fond-arm-match-under-macro  ^-  tang
  =/  =pony  (~(fond ut p.baz) %both ~[%foo %q %b])
  ?>  ?=([%& * %| *] pony)
  ;:  weld
    %+  expect-eq
      !>  `vein`~[[~ 1] ~ ~ [~ 3] ~ [~ 3]]
      !>  p.p.pony
    %+  expect-eq
      !>  2
      !>  `axis`p.q.p.pony
  ==
::
++  test-fond-synthetic-match-under-macro  ^-  tang
  =/  =pony  (~(fond ut p.qux) %both ~[%zop])
  ?>  ?=([%| %| *] pony)
  %+  expect-eq
    !>  `nock`[%7 p=[%0 p=7] q=[%8 p=[%1 p=0] q=[p=[%1 p=[4 0 6]] q=[%0 p=1]]]]
    !>  q.p.p.pony
::
++  test-fond-leg-match-under-bridge  ^-  tang
  =/  sut  p.fiz
  =/  pony  (~(fond ut sut) %both ~[%foo])
  ?>  ?=([%& *] pony)
  =/  sony  (~(fond ut sut) %both ~[%a %foo])
  ;:  weld
    %+  expect-eq
      !>  `vein`~[~ [~ 30] ~]
      !>  p.p.pony
    %+  expect-eq
      !>
      [%.y p=[p=~[~ [~ 6] ~ [~ 30] ~] q=[%.y p=[%atom p=%$ q=~]]]]
      !>  sony
  ==
:: ::
++  test-fond-arm-match-under-bridge  ^-  tang
  =/  =pony  (~(fond ut p.fiz) %both ~[%hi])
  ?>  ?=([%& * %| *] pony)
  ;:  weld
    %+  expect-eq
      !>  `vein`~[[~ 7]]
      !>  p.p.pony
    %+  expect-eq
      !>  2
      !>  `axis`p.q.p.pony
  ==
::
++  test-fond-synthetic-match-under-bridge  ^-  tang
  =/  =pony  (~(fond ut p.fiz) %both ~[%a %hi])
  ?>  ?=([%| %| *] pony)
  =/  sony  (~(fond ut p.fiz) %both ~[[%| p=1 q=(some %$)]])
  ?>  ?=([%| %| *] sony)
  ;:  weld
    %+  expect-eq
      !>  `nock`[%0 p=26]
      !>  q.p.p.pony
    %+  expect-eq
      !>  [%7 p=[%0 p=30] q=[%9 p=2 q=[%0 p=1]]]
      !>  q.p.p.sony
  ==
--
