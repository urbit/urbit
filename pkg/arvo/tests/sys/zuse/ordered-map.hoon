/+  *test
=/  items-from-keys
  |=  keys=(list @ud)
  %+  turn  keys
  |=  k=@ud
  [k `@tas`(add k %a)]
::
=/  test-items=(list [@ud @tas])
  (items-from-keys (gulf 0 6))
::
=/  atom-map  ((ordered-map @ud @tas) lte)
::
|%
++  test-ordered-map-gas  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  %+  expect-eq
    !>  %.y
    !>  (check-balance:atom-map a)
::
++  test-ordered-map-tap  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  %+  expect-eq
    !>  test-items
    !>  (tap:atom-map a)
::
++  test-ordered-map-pop  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  %+  expect-eq
    !>  [[0 %a] (gas:atom-map ~ (items-from-keys (gulf 1 6)))]
    !>  (pop:atom-map a)
::
++  test-ordered-map-peek  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  %+  expect-eq
    !>  `[0 %a]
    !>  (peek:atom-map a)
::
++  test-ordered-map-nip  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  b  (nip:atom-map a)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ ~[[0^%a] [1^%b] [2^%c] [3^%d] [4^%e] [5^%f]])
    !>  b
::
++  test-ordered-map-subset  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  b  (subset:atom-map a `0 `4)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ ~[[1^%b] [2^%c] [3^%d]])
    !>  b
::
++  test-ordered-map-null-start-subset  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  b  (subset:atom-map a ~ `5)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ ~[[0^%a] [1^%b] [2^%c] [3^%d] [4^%e]])
    !>  b
::
++  test-ordered-map-null-end-subset  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  b  (subset:atom-map a `1 ~)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ ~[[2^%c] [3^%d] [4^%e] [5^%f] [6^%g]])
    !>  b
::
++  test-ordered-map-double-null-subset  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  b  (subset:atom-map a ~ ~)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ ~[[0^%a] [1^%b] [2^%c] [3^%d] [4^%e] [5^%f] [6^%g]])
    !>  b
::
++  test-ordered-map-not-found-start-subset  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ ~[[1^%b]])
  ::
  =/  b  (subset:atom-map a `0 ~)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ ~[[1^%b]])
    !>  b
::
++  test-ordered-map-traverse  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  b  %-  (traverse:atom-map ,(list [@ud @tas]))
         :*  a
             state=~
             ::
             |=  [s=(list [@ud @tas]) k=@ud v=@tas]
             :+  ?:  =(3 k)
                   ~
                 [~ `@tas`+(v)]
               =(5 k)
             [[k v] s]
         ==
  ::
  ;:  weld
    %+  expect-eq
      !>  (gas:atom-map ~ ~[[0^%b] [1^%c] [2^%d] [4^%f] [5^%g] [6^%g]])
      !>  +.b
  ::
    %+  expect-eq
      !>  (flop (items-from-keys (gulf 0 5)))
      !>  -.b
  ==
::
++  test-ordered-map-traverse-delete-all  ^-  tang
  ;:  weld
    =/  q  ((ordered-map ,@ ,~) lte)
    =/  o  (gas:q ~ ~[1/~ 2/~ 3/~])
    =/  b  ((traverse:q ,~) o ~ |=([~ key=@ ~] [~ %| ~]))
    %+  expect-eq
      !>  [~ ~]
      !>  b
  ::
    =/  c
      :~  [[2.127 1] ~]  [[2.127 2] ~]  [[2.127 3] ~]
          [[2.127 7] ~]  [[2.127 8] ~]  [[2.127 9] ~]
      ==
    =/  compare
      |=  [[aa=@ ab=@] [ba=@ bb=@]]
      ?:((lth aa ba) %.y ?:((gth aa ba) %.n (lte ab bb)))
    =/  q  ((ordered-map ,[@ @] ,~) compare)
    =/  o  (gas:q ~ c)
    =/  b  ((traverse:q ,~) o ~ |=([~ key=[@ @] ~] [~ %| ~]))
    %+  expect-eq
      !>  [~ ~]
      !>  b
  ==
::
++  test-ordered-map-uni  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ (scag 4 test-items))
  =/  b=(tree [@ud @tas])  (gas:atom-map ~ (slag 4 test-items))
  ::
  =/  c  (uni:atom-map a b)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ test-items)
    !>  c
--
