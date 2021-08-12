/+  *test
::
=/  cor  (up @ @t)
=/  pr  (gas:cor ~ ~[[1 0 'A'] [2 0 'B'] [3 0 'C'] [4 0 'D']])
=/  qr
  %+  gas:cor  ~
  ~[[1 3 'A'] [2 2 'B'] [3 1 'C'] [4 1 'D'] [5 10 'E'] [6 0 'F']]
=+  deb:cor
|%
++  test-balance-errors  ^-  tang
  =/  node  [[0 0 'a'] ~ 0 ~]
  ;:  weld
    %-  expect-fail
      |.  (llsin:cor node)
    %-  expect-fail
      |.  (rlsin:cor node)
    %-  expect-fail
      |.  (lrsin:cor node)
    %-  expect-fail
      |.  (rrsin:cor node)
    %-  expect-fail
      |.  (lldub:cor node)
    %-  expect-fail
      |.  (rldub:cor node)
    %-  expect-fail
      |.  (lrdub:cor node)
    %-  expect-fail
      |.  (rrdub:cor node)
  ==
::
++  test-wyt  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  0
      !>  (wyt:cor ~)
    %+  expect-eq
      !>  1
      !>  (wyt:cor (one:cor [1 1 'a']))
    %+  expect-eq
      !>  4
      !>  (wyt:cor (gas:cor ~ ~[[1 1 'a'] [2 2 'b'] [3 3 'c'] [4 4 'd']]))
  ==
::
++  test-min  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (min:cor ~)
    %+  expect-eq
      !>  `[3 100 'b']
      !>  (min:cor (gas:cor ~ ~[[5 101 'a'] [3 100 'b']]))
  ==
::
++  test-gas  ^-  tang
  =/  ls  ~[[1 0 'A'] [2 0 'B'] [3 0 'C'] [4 0 'D']]
  %+  expect-eq
    !>  (gas:cor ~ ls)
    !>  (gas:cor ~ (flop ls))
::
++  test-has-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (has:cor pr 1)
      !>  !=(~ (get:cor pr 1))
    %+  expect-eq
      !>  (has:cor pr 5)
      !>  !=(~ (get:cor pr 5))
  ==
::
++  test-put-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (some [0 'A'])
      !>  (get:cor (put:cor ~ 1 0 'A') 1)
    %+  expect-eq
      !>  (some [5 'E'])
      !>  (get:cor (put:cor ~ 5 5 'E') 5)
  ==
::
++  test-put-del  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (get:cor (del:cor (put:cor ~ 1 0 'A') 1) 1)
    %+  expect-eq
      !>  ~
      !>  (get:cor (del:cor (put:cor ~ 5 5 'E') 5) 5)
  ==
::
++  test-pet  ^-  tang
  =/  pat  (pet:cor (put:cor ~ 1 0 'A') 1)
  (expect !>(?>(?=(^ pat) &(=(0 p.u.pat) =('A' q.u.pat) =(~ r.u.pat)))))
::
++  test-del-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (get:cor (del:cor ~ 1) 1)
    %+  expect-eq
      !>  ~
      !>  (get:cor (del:cor pr 1) 1)
  ==
::
++  test-cut  ^-  tang
  =/  zr  (gas:cor ~ ~[[5 101 'a'] [3 100 'b']])
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (cut:cor ~)
    %+  expect-eq
      !>  (some [3 1 'C'])
      !>  (min:cor (cut:cor qr))
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
      !>  [[~ 'hello'] (one:cor [3 100 'a'])]
      !>  (jab:cor ~ 3 eff)
    %+  expect-eq
      !>  [[~ 'world'] ~]
      !>  (jab:cor (one:cor [3 100 'a']) 3 eff)
    %+  expect-eq
      !>  [[~ 'cats'] (one:cor [3 101 'b'])]
      !>  (jab:cor (one:cor [3 100 'b']) 3 eff)
  ==
::
++  test-jib  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  [~ ~]
      !>  (jib:cor ~ |=(* [~ ~]))
    %+  expect-eq
      !>  [~ ~]
      !>  (jib:cor (one:cor [3 100 'a']) |=(* [~ ~]))
  ==
::
++  test-tap
  ;:  weld
    %-  expect
      !>  (sam:cor qr (gas:cor ~ (tap:cor qr)))
    %-  expect
      !>  (sam:cor pr (gas:cor ~ (tap:cor pr)))
  ==
::
++  test-pan
  =/  pun  (pan:cor qr 5 10 'Q')
  ;:  weld
    %+  expect-eq
      !>  p.pun
      !>  (get:cor qr 5)
    %+  expect-eq
      !>  (some [10 'Q'])
      !>  (get:cor q.pun 5)
  ==
--
