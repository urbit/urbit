/+  *test
::
=/  cor  (up @ @)
=/  pr  (gas:cor ~ ~[[1 1 1] [2 2 2] [3 3 3] [4 4 4]])
=/  qr  (gas:cor ~ ~[[4 4 4] [3 3 3] [2 2 2] [1 1 1]])
=/  rr  (gas:cor ~ ~[[1 5 1] [2 2 2] [3 3 3] [4 4 4]])
=/  sr  (gas:cor ~ ~[[2 2 2] [1 1 1] [4 4 4] [3 3 3]])
::
=>
|%
++  grab
  |=  a=(unit (qual @ @ @ pri:cor))
  ^-  (trel @ @ @)
  ?>  ?=(^ a)
  [p.u.a q.u.a r.u.a]
--
::
|%
++  test-bot  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  [1 1 1]
      !>  (grab (bot:cor pr))
    %+  expect-eq
      !>  [1 1 1]
      !>  (grab (bot:cor qr))
    %+  expect-eq
      !>  [1 1 1]
      !>  (grab (bot:cor sr))
  ==
::
++  test-cut  ^-  tang
  =/  tr  (gas:cor ~ ~[[2 2 2] [3 3 3] [4 4 4]])
  ;:  weld
    %+  expect-eq
      !>  tr
      !>  (cut:cor pr)
    %+  expect-eq
      !>  tr
      !>  (cut:cor qr)
    %+  expect-eq
      !>  tr
      !>  (cut:cor sr)
  ==
::
++  test-gun-pri  ^-  tang
  =/  tr  (gas:cor qr ~[[5 5 5]])
  ;:  weld
    %+  expect-eq
      !>  tr
      !>  q:(gun:cor pr 5 5 5)
    %+  expect-eq
      !>  tr
      !>  q:(gun:cor qr 5 5 5)
    %+  expect-eq
      !>  tr
      !>  q:(gun:cor sr 5 5 5)
  ==
::
++  test-gun-vic  ^-  tang
  =/  tr  (gun:cor qr 1 5 1)
  =/  ur  (gun:cor qr 2 5 2)
  =/  vr  (gun:cor qr 3 5 3)
  =/  wr  (gun:cor qr 4 5 4)
  ;:  weld
    %+  expect-eq
      !>  (some [1 1])
      !>  p.tr
    %+  expect-eq
      !>  (some [2 2])
      !>  p.ur
    %+  expect-eq
      !>  (some [3 3])
      !>  p.vr
    %+  expect-eq
      !>  (some [4 4])
      !>  p.wr
  ==
::
++  test-see-pri  ^-  tang
  %+  expect-eq
    !>  rr
    !>  q:(see:cor qr 1 5)
::
++  test-see-vic  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  p:(see:cor qr 5 5)
    %+  expect-eq
      !>  (some [1 1])
      !>  p:(see:cor qr 1 5)
  ==
--
