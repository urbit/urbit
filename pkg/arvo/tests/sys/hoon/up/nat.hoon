/+  *test
::
=/  nat  (nat:up @t)
=/  pr  (gas:nat ~ ~[[1 0 'A'] [2 0 'B'] [3 0 'C'] [4 0 'D']])
=/  qr
  %+  gas:nat  ~
  ~[[1 3 'A'] [2 2 'B'] [3 1 'C'] [4 1 'D'] [5 10 'E'] [6 0 'F']]
|%
++  test-wyt  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  0
      !>  (wyt:nat ~)
    %+  expect-eq
      !>  1
      !>  (wyt:nat (one:nat [1 1 'a']))
    %+  expect-eq
      !>  4
      !>  (wyt:nat (gas:nat ~ ~[[1 1 'a'] [2 2 'b'] [3 3 'c'] [4 4 'd']]))
  ==
::
++  test-min  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (min:nat ~)
    %+  expect-eq
      !>  `[3 100 'b']
      !>  (min:nat (gas:nat ~ ~[[5 101 'a'] [3 100 'b']]))
  ==
::
++  test-gas  ^-  tang
  =/  ls  ~[[1 0 'A'] [2 0 'B'] [3 0 'C'] [4 0 'D']]
  %-  expect
    !>  (sam:nat (gas:nat ~ ls) (gas:nat ~ (flop ls)))
::
++  test-has-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (has:nat pr 1)
      !>  !=(~ (get:nat pr 1))
    %+  expect-eq
      !>  (has:nat pr 5)
      !>  !=(~ (get:nat pr 5))
  ==
::
++  test-put-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  (some [0 'A'])
      !>  (get:nat (put:nat ~ 1 0 'A') 1)
    %+  expect-eq
      !>  (some [5 'E'])
      !>  (get:nat (put:nat ~ 5 5 'E') 5)
  ==
::
++  test-put-del  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (get:nat (del:nat (put:nat ~ 1 0 'A') 1) 1)
    %+  expect-eq
      !>  ~
      !>  (get:nat (del:nat (put:nat ~ 5 5 'E') 5) 5)
  ==
::
++  test-pet  ^-  tang
  =/  pat  (pet:nat (put:nat ~ 1 0 'A') 1)
  (expect !>(?>(?=(^ pat) &(=(0 p.u.pat) =('A' q.u.pat) =(~ r.u.pat)))))
::
++  test-del-get  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (get:nat (del:nat ~ 1) 1)
    %+  expect-eq
      !>  ~
      !>  (get:nat (del:nat pr 1) 1)
  ==
::
++  test-cut  ^-  tang
  =/  zr  (gas:nat ~ ~[[5 101 'a'] [3 100 'b']])
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (cut:nat ~)
    %+  expect-eq
      !>  (some [3 1 'C'])
      !>  (min:nat (cut:nat qr))
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
      !>  [[~ 'hello'] (one:nat [3 100 'a'])]
      !>  (jab:nat ~ 3 eff)
    %+  expect-eq
      !>  [[~ 'world'] ~]
      !>  (jab:nat (one:nat [3 100 'a']) 3 eff)
    %+  expect-eq
      !>  [[~ 'cats'] (one:nat [3 101 'b'])]
      !>  (jab:nat (one:nat [3 100 'b']) 3 eff)
  ==
::
++  test-jib  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  [~ ~]
      !>  (jib:nat ~ |=(* [~ ~]))
    %+  expect-eq
      !>  [~ ~]
      !>  (jib:nat (one:nat [3 100 'a']) |=(* [~ ~]))
  ==
::
++  test-tap
  ;:  weld
    %-  expect
      !>  (sam:nat qr (gas:nat ~ (tap:nat qr)))
    %-  expect
      !>  (sam:nat pr (gas:nat ~ (tap:nat pr)))
  ==
::
++  test-pan
  =/  pun  (pan:nat qr 5 10 'Q')
  ;:  weld
    %+  expect-eq
      !>  p.pun
      !>  (get:nat qr 5)
    %+  expect-eq
      !>  (some [10 'Q'])
      !>  (get:nat q.pun 5)
  ==
--
