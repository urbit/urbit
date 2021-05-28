/+  *test
=/  example-macro=(pair type hoon)
  =/  gen=hoon  (ream '=/  a  1  =*  zop  |=(a=@ [a +(^a)])  zop')
  =/  sut=type  (~(play ut %noun) gen)
  [p=sut q=gen]
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
::
:: ++  test-fond-leg-match  ^-  tang  !!
::   ::;:  weld
::   ::  %+  expect-eq
::   ::    !>  0
::   ::    !>  (fynd:ob (fein:ob 0))
::   ::
::   ::  %+  expect-eq
::   ::    !>  15.663.360
::   ::    !>  (fynd:ob (fein:ob 15.663.360))
::   ::
::   ::==
:: ::
:: ++  test-fond-arm-match  ^-  tang  !!
:: ::
:: ++  test-fond-unmatch  ^-  tang  !!
:: ::
:: ++  test-fond-synthetic-alias  ^-  tang  !!
::
++  test-fond-leg-match-under-macro  ^-  tang
  =/  sut  p.example-macro
  ;:  weld
    %+  expect-eq
      !>  (~(fond ut sut) %both ~[%a %$])
      !>  `pony`[%& p=[p=~[~ [~ 6] [~ 1]] q=[%.y p=[%atom p=%$ q=~]]]]
    %+  expect-eq
      !>  (~(fond ut sut) %both ~[[%| p=1 q=(some %a)] %$])
      !>  `pony`[%& p=[p=~[~ [~ 14] ~ [~ 1]] q=[%.y p=[%atom p=%ud q=~]]]]
  ==
::
++  test-fond-arm-match-under-macro  ^-  tang
  =/  sut=type  p.example-macro
  =/  =pony  (~(fond ut sut) %both ~[%$])
  ?>  &(!=(pony ~) =(%& -.pony) =(%| +>-.pony))
  ;:  weld
    %+  expect-eq
      !>  +<.pony
      !>  ~[[~ 1]]
    %+  expect-eq
      !>  +>+<.pony
      !>  2
  ==
::
:: ++  test-fond-synthetic-match-under-macro  ^-  tang
::   =/  sut  p.example-macro
::   =/  =pony  (~(fond ut sut) %both ~[%zop])
::   ?>  &(!=(pony ~) =(%| -.pony) =(%| +<.pony))
::   =/  =nock
::     [%7 p=[%0 p=7] q=[%8 p=[%1 p=0] q=[p=[%1 p=[[0 6] 4 0 14]] q=[%0 p=1]]]]
::   %+  expect-eq
::     !>  +>+.pony
::     !>  nock
--
