/+  *test
=/  cor  (up @ @)
=/  colk=@  281.474.976.718.163
=/  coll=@  281.474.976.723.900
=/  tf=@  0xffff.ffff.ffff
=/  ff=@  0xffff.ffff.ffff.ffff
=/  sp=pri:cor  [%bin 1 1 [1 ~] 8.388.608 ~ [%tip 2 2 [2 ~]]]
=/  bp=pri:cor  [%bin tf 1 [1 ~] 268.435.456 [%tip ff 2 [2 ~]] ~]
::
=/  so=pro:qor:cor  [[1 1 1] ~ 1]
=/  ro=pro:qor:cor  [[1 1 1] [%rlos 1 [[2 2 2] ~ 1 ~]] 2]
=/  bo=pro:qor:cor  [[tf 1 1] ~ tf]
=/  co=pro:qor:cor  [[tf 1 1] [%llos 1 [[ff 2 2] ~ ff ~]] tf]
::
|%
++  test-lex
  ;:  weld
    %-  expect  !>((lex:cor 0 0 1 1))
    %-  expect  !>((lex:cor 0 1 0 2))
    %-  expect  !>((lex:cor ff 1 ff 2))
    %-  expect  !>((lex:cor 0 ff 0 ff))
  ==
::
++  test-zero
  ;:  weld
    %-  expect  !>((zero:cor 3 2))
    %-  expect  !>((zero:cor 3 ff))
    %-  expect  !>(!(zero:cor colk coll))
  ==
::
++  test-peak
  ;:  weld
    %+  expect-eq
      !>  0b1000.0000.0000.0000.0000.0000.0000
      !>  (peak:cor 0 1)
    %+  expect-eq
      !>  0b1.0000.0000.0000.0000.0000.0000.0000
      !>  (peak:cor tf ff)
    %+  expect-eq
      !>  0
      !>  (peak:cor colk coll)
  ==
::
++  test-feud
  ;:  weld
    %-  expect  !>((feud:cor 1 2 3))
    %-  expect  !>((feud:cor 1 ff tf))
    %-  expect  !>(!(feud:cor 1 colk coll))
  ==
::
++  test-rule
  ;:  weld
    %+  expect-eq
      !>  sp
      !>  (rule:cor 1 1 [1 ~] [%tip 2 2 [2 ~]] ~)
    %+  expect-eq
      !>  bp
      !>  (rule:cor tf 1 [1 ~] [%tip ff 2 [2 ~]] ~)
    %+  expect-eq
      !>  `pri:cor`[%bin colk 1 [1 ~] 0 [%tip coll 2 [2 ~]] ~]
      !>  (rule:cor colk 1 [1 ~] [%tip coll 2 [2 ~]] ~)
  ==
::
++  test-fuse
  ;:  weld
    %+  expect-eq
      !>  sp
      !>  (fuse:cor 8.388.608 [%tip 1 1 [1 ~]] [%tip 2 2 [2 ~]])
    %+  expect-eq
      !>  bp
      !>  (fuse:cor 268.435.456 [%tip ff 2 [2 ~]] [%tip tf 1 [1 ~]])
  ==
::
++  test-qor-put
  ;:  weld
    %+  expect-eq
      !>  so
      !>  (put:qor:cor ~ 1 1 1)
    %+  expect-eq
      !>  ro
      !>  (put:qor:cor so 2 2 2)
    %+  expect-eq
      !>  bo
      !>  (put:qor:cor ~ tf 1 1)
    %+  expect-eq
      !>  co
      !>  (put:qor:cor bo ff 2 2)
    %+  expect-eq
      !>  [[1 2 2] ~ 1]
      !>  (put:qor:cor so 1 2 2)
    %+  expect-eq
      !>  [[tf 2 2] ~ tf]
      !>  (put:qor:cor bo tf 2 2)
  ==
::
++  test-qor-del
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (del:qor:cor so 1)
    %+  expect-eq
      !>  so
      !>  (del:qor:cor ro 2)
    %+  expect-eq
      !>  ~
      !>  (del:qor:cor bo tf)
    %+  expect-eq
      !>  bo
      !>  (del:qor:cor co ff)
  ==
::
++  test-qor-get
  ;:  weld
    %+  expect-eq
      !>  [~ 1 1]
      !>  (get:qor:cor so 1)
    %+  expect-eq
      !>  [~ 1 1]
      !>  (get:qor:cor bo tf)
  ==
::
++  test-qor-bot
  ;:  weld
    %+  expect-eq
      !>  `[1 1 1 [[2 2 2] ~ 2]]
      !>  (bot:qor:cor ro)
    %+  expect-eq
      !>  `[tf 1 1 [[ff 2 2] ~ ff]]
      !>  (bot:qor:cor co)
  ==
::  FIXME add tests for collision handling and such
--


