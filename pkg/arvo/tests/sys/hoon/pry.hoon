/+  *test
::
=/  ord  ord:(up @ @t)
=/  pr  (gas:ord ~ ~[[1 0 'A'] [2 0 'B'] [3 0 'C'] [4 0 'D']])
=/  qr
  %+  gas:ord  ~
  ~[[1 3 'A'] [2 2 'B'] [3 1 'C'] [4 1 'D'] [5 10 'E'] [6 0 'F']]
=+  deb:ord
|%
++  test-balance-errors  ^-  tang
  =/  node  [[0 0 'a'] ~ 0 ~]
  ;:  weld
    %-  expect-fail
      |.  (llsin:ord node)
    %-  expect-fail
      |.  (rlsin:ord node)
    %-  expect-fail
      |.  (lrsin:ord node)
    %-  expect-fail
      |.  (rrsin:ord node)
    %-  expect-fail
      |.  (lldub:ord node)
    %-  expect-fail
      |.  (rldub:ord node)
    %-  expect-fail
      |.  (lrdub:ord node)
    %-  expect-fail
      |.  (rrdub:ord node)
  ==
::
++  test-wyt  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  0
      !>  (wyt:ord ~)
    %+  expect-eq
      !>  1
      !>  (wyt:ord (one:ord [1 1 'a']))
    %+  expect-eq
      !>  4
      !>  (wyt:ord (gas:ord ~ ~[[1 1 'a'] [2 2 'b'] [3 3 'c'] [4 4 'd']]))
  ==
::
++  test-min  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (min:ord ~)
    %+  expect-eq
      !>  `[3 100 'b']
      !>  (min:ord (gas:ord ~ ~[[5 101 'a'] [3 100 'b']]))
  ==
::
++  test-gas  ^-  tang
  =/  ls  ~[[1 0 'A'] [2 0 'B'] [3 0 'C'] [4 0 'D']]
  %+  expect-eq
    !>  (gas:ord ~ ls)
    !>  (gas:ord ~ (flop ls))
::
++  test-has-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (has:ord pr 1)
      !>  !=(~ (get:ord pr 1))
    %+  expect-eq
      !>  (has:ord pr 5)
      !>  !=(~ (get:ord pr 5))
  ==
::
++  test-put-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (some [0 'A'])
      !>  (get:ord (put:ord ~ 1 0 'A') 1)
    %+  expect-eq
      !>  (some [5 'E'])
      !>  (get:ord (put:ord ~ 5 5 'E') 5)
  ==
::
++  test-put-del  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (get:ord (del:ord (put:ord ~ 1 0 'A') 1) 1)
    %+  expect-eq
      !>  ~
      !>  (get:ord (del:ord (put:ord ~ 5 5 'E') 5) 5)
  ==
::
++  test-pet  ^-  tang
  =/  pat  (pet:ord (put:ord ~ 1 0 'A') 1)
  (expect !>(?>(?=(^ pat) &(=(0 p.u.pat) =('A' q.u.pat) =(~ r.u.pat)))))
::
++  test-del-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (get:ord (del:ord ~ 1) 1)
    %+  expect-eq
      !>  ~
      !>  (get:ord (del:ord pr 1) 1)
  ==
::
++  test-cut  ^-  tang
  =/  zr  (gas:ord ~ ~[[5 101 'a'] [3 100 'b']])
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (cut:ord ~)
    %+  expect-eq
      !>  (some [3 1 'C'])
      !>  (min:ord (cut:ord qr))
  ==
::
++  test-jab  ^-  tang
  =/  eff
    |=  x=(unit (pair @ cord))
    ^-  (pair (unit cord) (unit (pair @ cord)))
    ?~  x
      [(some 'hello') `[100 'a']]
    ?:  =(`[100 'a'] x)
      [(some 'world') ~]
    [(some 'cats') `[101 'b']]
  ::
  ;:  weld
    %+  expect-eq
      !>  [[~ 'hello'] (one:ord [3 100 'a'])]
      !>  (jab:ord ~ 3 eff)
    %+  expect-eq
      !>  [[~ 'world'] ~]
      !>  (jab:ord (one:ord [3 100 'a']) 3 eff)
    %+  expect-eq
      !>  [[~ 'cats'] (one:ord [3 101 'b'])]
      !>  (jab:ord (one:ord [3 100 'b']) 3 eff)
  ==
::
++  test-jib  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  [~ ~]
      !>  (jib:ord ~ |=(* [~ ~]))
    %+  expect-eq
      !>  [~ ~]
      !>  (jib:ord (one:ord [3 100 'a']) |=(* [~ ~]))
  ==
::
++  test-tap
  ;:  weld
    %-  expect
      !>  (sam:ord qr (gas:ord ~ (tap:ord qr)))
    %-  expect
      !>  (sam:ord pr (gas:ord ~ (tap:ord pr)))
  ==
::
++  test-pan
  =/  pun  (pan:ord qr 5 10 'Q')
  ;:  weld
    %+  expect-eq
      !>  p.pun
      !>  (get:ord qr 5)
    %+  expect-eq
      !>  (some [10 'Q'])
      !>  (get:ord q.pun 5)
  ==
--
