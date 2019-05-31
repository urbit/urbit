/+  *test
/=  alef  /:  /===/sys/vane/alef
          /!noun/
::
=/  vane  (alef !>(..zuse))
=/  lane-foo=lane:alef  `@uxlane``@`'lane-foo'
::
=/  items-from-keys
  |=  keys=(list @ud)
  %+  turn  keys
  |=  k=@ud
  [k `@tas`(add k %a)]
::
=/  test-items=(list [@ud @tas])
  (items-from-keys (gulf 0 6))
::
=/  atom-map  ((ordered-map:alef @ud @tas) lte)
::
|%
::
+|  %ordered-map
::
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
++  test-ordered-map-traverse  ^-  tang
  ::
  =/  a=(tree [@ud @tas])  (gas:atom-map ~ test-items)
  ::
  =/  trav-func  (traverse:atom-map ,(list [@ud @tas]))
  ::
  =/  b  %-  trav-func
         :*  a
             start=3
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
      !>  (gas:atom-map ~ ~[[0^%a] [1^%b] [2^%c] [4^%f] [5^%g] [6^%g]])
      !>  +.b
  ::
    %+  expect-eq
      !>  ~[[5^%f] [4^%e] [3^%d]]
      !>  -.b
  ==
::
+|  %ordered-set
::
++  test-ordered-set-gas  ^-  tang
  ::
  =/  atom-set  ((ordered-set:alef @) lte)
  =/  a=(tree @)  (gas:atom-set ~ (gulf 1 7))
  ::
  %+  expect-eq
    !>  %.y
    !>  (check-balance:atom-set a)
::
++  test-ordered-set-tap  ^-  tang
  ::
  =/  atom-set  ((ordered-set:alef @) lte)
  =/  a=(tree @)  (gas:atom-set ~ (gulf 1 7))
  ::
  %+  expect-eq
    !>  (gulf 1 7)
    !>  (tap:atom-set a)
::
++  test-ordered-set-pop  ^-  tang
  ::
  =/  atom-set  ((ordered-set:alef @) lte)
  =/  a=(tree @)  (gas:atom-set ~ (gulf 1 7))
  ::
  %+  expect-eq
    !>  [1 (gas:atom-set ~ (gulf 2 7))]
    !>  (pop:atom-set a)
::
++  test-ordered-set-peek  ^-  tang
  ::
  =/  atom-set  ((ordered-set:alef @) lte)
  =/  a=(tree @)  (gas:atom-set ~ (gulf 1 7))
  ::
  %+  expect-eq
    !>  `1
    !>  (peek:atom-set a)
::
++  test-ordered-set-sift  ^-  tang
  ::
  =/  atom-set  ((ordered-set:alef @) lte)
  =/  items=(list @)  (gulf 1 7)
  =/  a=(tree @)  (gas:atom-set ~ items)
  ::  reject items less than 3
  ::
  =/  res  (sift:atom-set a |=(@ (lth +< 4)))
  ::
  ;:  weld
    %+  expect-eq
      !>  %.y
      !>  (check-balance:atom-set kept.res)
  ::
    %+  expect-eq
      !>  (gas:atom-set ~ (gulf 4 7))
      !>  kept.res
  ::
    %+  expect-eq
      !>  (gulf 1 3)
      !>  lost.res
  ==
::
++  test-packet-encoding  ^-  tang
  ::
  =/  =packet:alef
    :*  [sndr=~nec rcvr=~doznec-doznec]
        encrypted=%.n
        origin=~
        content=[12 13]
    ==
  ::
  =/  encoded  (encode-packet:alef packet)
  =/  decoded  (decode-packet:alef encoded)
  ::
  %+  expect-eq
    !>  packet
    !>  decoded
::
++  test-alien-encounter  ^-  tang
  ::
  =/  =packet:alef
    :*  [sndr=~nec rcvr=~doznec-doznec]
        encrypted=%.y
        origin=~
        content=%double-secret
    ==
  =/  =blob:alef  (encode-packet:alef packet)
  ::
  =/  event-args
    [our=~doznec-doznec eny=0xdead.beef now=~2222.2.2 *sley]
  ::
  =/  res
    (call:(vane event-args) ~[//unix] *type %hear lane-foo blob)
  ::
  ;:  weld
    %+  expect-eq
      !>  [~[//unix] %pass /alien %j %pubs ~nec]~
      !>  -.res
  ::
    %+  expect-eq
      !>  [%alien [lane-foo packet]~ ~]
      !>  (~(got by peers.ames-state.+.res) ~nec)
  ==
--
