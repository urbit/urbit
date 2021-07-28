/+  *test
::
=/  pr  (~(gas up *(pry)) ~[[1 0 'A'] [2 0 'B'] [3 0 'C'] [4 0 'D']])
=/  qr  (~(gas up *(pry)) ~[[1 3 'A'] [2 2 'B'] [3 1 'C'] [4 0 'D']])
=+  ~(deb up *(pry))
|%
++  test-balance-errors  ^-  tang
  =/  node  [[0 0 'a'] ~ 0 ~]
  ;:  weld
    %-  expect-fail
      |.  (llsin node)
    %-  expect-fail
      |.  (rlsin node)
    %-  expect-fail
      |.  (lrsin node)
    %-  expect-fail
      |.  (rrsin node)
    %-  expect-fail
      |.  (lldub node)
    %-  expect-fail
      |.  (rldub node)
    %-  expect-fail
      |.  (lrdub node)
    %-  expect-fail
      |.  (rrdub node)
  ==
::
++  test-wyt  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  0
      !>  ~(wyt up *(pry))
    %+  expect-eq
      !>  1
      !>  ~(wyt up (one [1 1 1]))
    %+  expect-eq
      !>  4
      !>  ~(wyt up (~(gas up *(pry)) ~[[1 1 1] [2 2 2] [3 3 3] [4 4 4]]))
  ==
::
++  test-min  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  ~(min up *(pry))
    %+  expect-eq
      !>  `[3 100 'b']
      !>  ~(min up (~(gas up *(pry)) ~[[5 101 'a'] [3 100 'b']]))
  ==
::
++  test-gas  ^-  tang
  =/  ls  ~[[1 0 'A'] [2 0 'B'] [3 0 'C'] [4 0 'D']]
  %+  expect-eq
    !>  (~(gas up *(pry)) ls)
    !>  (~(gas up *(pry)) (flop ls))
::
++  test-has-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (~(has up pr) 1)
      !>  !=(~ (~(get up pr) 1))
    %+  expect-eq
      !>  (~(has up pr) 5)
      !>  !=(~ (~(get up pr) 5))
  ==
::
++  test-put-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (some [0 'A'])
      !>  (~(get up (~(put up *(pry)) 1 0 'A')) 1)
    %+  expect-eq
      !>  (some [5 'E'])
      !>  (~(get up (~(put up pr) 5 5 'E')) 5)
  ==
::
++  test-put-del  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (~(get up (~(del up (~(put up *(pry)) 1 0 'A')) 1)) 1)
    %+  expect-eq
      !>  ~
      !>  (~(get up (~(del up (~(put up pr) 5 5 'E')) 5)) 5)
  ==
::
::++  test-pet  ^-  tang
::  =/  pat  (~(pet up (~(put up *(pry)) 1 0 'A')) 1)
::  ;:  weld
::    (expect !>(?=(^ pat)))
::    (expect !>(&(=(0 p.p.u.pat) =('A' v.p.u.pat) =(q.u.pat *(pry)))))
::  ==
::
++  test-del-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (~(get up (~(del up *(pry)) 1)) 1)
    %+  expect-eq
      !>  ~
      !>  (~(get up (~(del up pr) 1)) 1)
  ==
::
::++  test-cut  ^-  tang
::  ;:  weld
::    %+  expect-eq
::      !>  ~
::      !>  ~(cut up *(pry))
::    %+  expect-eq
::      !>  [3 1 'C']
::      !>  ~(min up ~(cut up pr))
::  ==
::
::++  test-jab  ^-  tang
::  =/  eff
::    |=  x=(unit (pair @ cord))
::    ^-  (pair tape (unit (pair @ cord)))
::    ?~  x
::      ["hello" `[100 'a']]
::    ?:  =(`[100 'a'] x)
::      ["world" ~]
::    ["cats" `[101 'b']]
::  ::
::  ;:  weld
::    %+  expect-eq
::      !>  ["hello" (one [3 100 'a'])]
::      !>  (~(jab up *(pry)) 3 eff)
::    %+  expect-eq
::      !>  ["world" ~]
::      !>  (~(jab up (one [3 100 'a'])) 3 eff)
::    %+  expect-eq
::      !>  ["cats" (one [3 101 'b'])]
::      !>  (~(jab up (one [3 100 'b'])) 3 eff)
::  ==
::
::++  test-jib  ^-  tang
::  =/  eff
::    |=  *
::    [~ ~]
::  ;:  weld
::    %+  expect-eq
::      !>  [~ ~]
::      !>  (~(jib up *(pry)) |=(* [~ ~]))
::    %+  expect-eq
::      !>  (~(jib up (one [3 100 'a'])) |=(* [~ ~]))
::      !>  [~ ~]
::  ==
++  test-tap
  ;:  weld
    %+  expect-eq
      !>  qr
      !>  (~(gas up *(pry)) ~(tap up qr))
    %+  expect-eq
      !>  pr
      !>  (~(gas up *(pry)) ~(tap up pr))
  ==
::
::++  test-pan
::  =/  pun  (~(pan up qr) 5 10 'Q')
::  ;:  weld
::    %+  expect-eq
::      !>  p.pun
::      !>  (~(get up qr) 5)
::    %+  expect-eq
::      !>  (some [10 'Q'])
::      !>  (~(get up q.pun) 5)
::  ==
--
