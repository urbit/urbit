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
=/  gte-atom-map  ((ordered-map @ud @tas) gte)
::
|%
++  test-ordered-map-gas  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  %+  expect-eq
    !>  %.y
    !>  (apt:atom-map a)
::
++  test-ordered-map-tap  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  %+  expect-eq
    !>  test-items
    !>  (tap:atom-map a)
::
++  test-ordered-map-tab-gte  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:gte-atom-map ~ test-items)
  ::
  %+  expect-eq
    !>  (flop test-items)
    !>  (tab:gte-atom-map a ~ 7)
::
++  test-ordered-map-tab-gte-starting-from  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:gte-atom-map ~ test-items)
  =/  small-test-items=(list [@ud @tas])
    (items-from-keys (gulf 2 5))
  ::
  %+  expect-eq
    !>  (flop small-test-items)
    !>  (tab:gte-atom-map a [~ 6] 4)
::
++  test-ordered-map-tab-gte-count  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:gte-atom-map ~ test-items)
  =/  small-test-items=(list [@ud @tas])
    (items-from-keys (gulf 4 6))
  ::
  %+  expect-eq
    !>  (flop small-test-items)
    !>  (tab:gte-atom-map a ~ 3)
::
++  test-ordered-map-tab  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  %+  expect-eq
    !>  test-items
    !>  (tab:atom-map a ~ 7)
::
++  test-ordered-map-tab-starting-from  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  =/  small-test-items=(list [@ud @tas])
    (items-from-keys (gulf 1 4))
  ::
  %+  expect-eq
    !>  small-test-items
    !>  (tab:atom-map a [~ 0] 4)
::
++  test-ordered-map-tab-count  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  =/  small-test-items=(list [@ud @tas])
    (items-from-keys (gulf 0 2))
  ::
  %+  expect-eq
    !>  small-test-items
    !>  (tab:atom-map a ~ 3)
::
++  test-ordered-map-tab-more-than-exist  ^-  tang
  ::
  =/  specific-test-items=(list [@ud @tas])
    (items-from-keys (gulf 1 6))
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ specific-test-items)
  ::
  %+  expect-eq
    !>  specific-test-items
    !>  (tab:atom-map a [~ 0] 8)
::
++  test-ordered-map-pop  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  %+  expect-eq
    !>  [[0 %a] (gas:atom-map ~ (items-from-keys (gulf 1 6)))]
    !>  (pop:atom-map a)
::
++  test-ordered-map-pry  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  %+  expect-eq
    !>  `[0 %a]
    !>  (pry:atom-map a)
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
++  test-ordered-map-lot  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  b  (lot:atom-map a `0 `4)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ ~[[1^%b] [2^%c] [3^%d]])
    !>  b
::
++  test-ordered-map-null-start-lot  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  b  (lot:atom-map a ~ `5)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ ~[[0^%a] [1^%b] [2^%c] [3^%d] [4^%e]])
    !>  b
::
++  test-ordered-map-null-end-lot  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  b  (lot:atom-map a `1 ~)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ ~[[2^%c] [3^%d] [4^%e] [5^%f] [6^%g]])
    !>  b
::
++  test-ordered-map-double-null-lot  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  b  (lot:atom-map a ~ ~)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ ~[[0^%a] [1^%b] [2^%c] [3^%d] [4^%e] [5^%f] [6^%g]])
    !>  b
::
++  test-ordered-map-not-found-start-lot  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ ~[[1^%b]])
  ::
  =/  b  (lot:atom-map a `0 ~)
  ::
  %+  expect-eq
    !>  (gas:atom-map ~ ~[[1^%b]])
    !>  b
::
++  test-ordered-map-dip  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  b  %-  (dip:atom-map ,(list [@ud @tas]))
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
++  test-ordered-map-dip-delete-all  ^-  tang
  ;:  weld
    =/  q  ((ordered-map ,@ ,~) lte)
    =/  o  (gas:q ~ ~[1/~ 2/~ 3/~])
    =/  b  ((dip:q ,~) o ~ |=([~ key=@ ~] [~ %| ~]))
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
    =/  b  ((dip:q ,~) o ~ |=([~ key=[@ @] ~] [~ %| ~]))
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
