/+  *test
/=  alef  /:  /===/sys/vane/alef
          /!noun/
::
|%
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
--
