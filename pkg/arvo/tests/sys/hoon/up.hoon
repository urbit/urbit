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
=/  ac=pri:cor  [%tip colk 4 [4 [[coll 5 5] ~ coll]]]
=/  po=pri:cor
  :*  %bin
      1
      1
      [1 ~]
      268.435.456
      [%tip colk 4 [4 ~]]
      [%bin 2 2 [2 ~] 33.554.432 ~ [%tip 3 3 [3 ~]]]
  ==
=/  re=pri:cor  [%tip colk 1 [1 [[coll 2 2] ~ coll]]]
::
|%
::  utilities
::
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
::  collision resolution
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
::
++  test-qor-dew
  ;:  weld
    %+  expect-eq
      !>  `[1 1 [[2 2 2] ~ 2]]
      !>  (dew:qor:cor ro 1)
    %+  expect-eq
      !>  `[1 1 [[ff 2 2] ~ ff]]
      !>  (dew:qor:cor co tf)
  ==
::  pri logic
::
++  test-get
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (get:cor ~ tf)
    %+  expect-eq
      !>  `[1 1]
      !>  (get:cor sp 1)
    %+  expect-eq
      !>  `[2 2]
      !>  (get:cor sp 2)
    %+  expect-eq
      !>  `[1 1]
      !>  (get:cor bp tf)
    %+  expect-eq
      !>  `[2 2]
      !>  (get:cor bp ff)
    %+  expect-eq
      !>  `[4 4]
      !>  (get:cor ac colk)
    %+  expect-eq
      !>  `[5 5]
      !>  (get:cor ac coll)
    %+  expect-eq
      !>  `[1 1]
      !>  (get:cor po 1)
    %+  expect-eq
      !>  `[2 2]
      !>  (get:cor po 2)
    %+  expect-eq
      !>  `[3 3]
      !>  (get:cor po 3)
    %+  expect-eq
      !>  `[4 4]
      !>  (get:cor po colk)
    %+  expect-eq
      !>  ~
      !>  (get:cor po coll)
  ==
::
++  test-put
  ;:  weld
    %+  expect-eq
      !>  [%tip 1 1 1 ~]
      !>  (put:cor ~ 1 1 1)
    %+  expect-eq
      !>  [%tip tf 1 1 ~]
      !>  (put:cor ~ tf 1 1)
    %+  expect-eq
      !>  sp
      !>  (put:cor [%tip 1 1 1 ~] 2 2 2)
    %+  expect-eq
      !>  bp
      !>  (put:cor [%tip tf 1 1 ~] ff 2 2)
    %+  expect-eq
      !>  re
      !>  (put:cor (put:cor ~ colk 1 1) coll 2 2)
    %+  expect-eq
      !>  re
      !>  (put:cor (put:cor ~ coll 2 2) colk 1 1)
  ==
::
++  test-del
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (del:cor ~ 1)
    %+  expect-eq
      !>  [%tip 2 2 2 ~]
      !>  (del:cor sp 1)
    %+  expect-eq
      !>  [%tip ff 2 2 ~]
      !>  (del:cor bp tf)
    %+  expect-eq
      !>  [%tip tf 1 1 ~]
      !>  (del:cor bp ff)
    %+  expect-eq
      !>  [%tip coll 5 5 ~]
      !>  (del:cor ac colk)
    %+  expect-eq
      !>  [%tip colk 4 4 ~]
      !>  (del:cor ac coll)
  ==
::
++  test-dew
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (dew:cor ~ 1)
    %+  expect-eq
      !>  `[1 1 [%tip 2 2 2 ~]]
      !>  (dew:cor sp 1)
    %+  expect-eq
      !>  `[1 1 %tip ff 2 2 ~]
      !>  (dew:cor bp tf)
    %+  expect-eq
      !>  `[4 4 [%tip coll 5 5 ~]]
      !>  (dew:cor ac colk)
    %+  expect-eq
      !>  `[5 5 [%tip colk 4 4 ~]]
      !>  (dew:cor ac coll)
  ==
::
++  test-bot
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (bot:cor ~)
    %+  expect-eq
      !>  `[1 1 1 [%tip 2 2 2 ~]]
      !>  (bot:cor sp)
    %+  expect-eq
      !>  `[tf 1 1 [%tip ff 2 2 ~]]
      !>  (bot:cor bp)
    %+  expect-eq
      !>  `[colk 4 4 %tip coll 5 5 ~]
      !>  (bot:cor ac)
    %+  expect-eq
      !>  `[1 1 1 %bin 2 2 [2 ~] 268.435.456 [%tip colk 4 4 ~] [%tip 3 3 3 ~]]
      !>  (bot:cor po)
    %+  expect-eq
      !>  `[colk 1 1 %tip coll 2 2 ~]
      !>  (bot:cor re)
  ==
::
++  test-cut
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (cut:cor ~)
    %+  expect-eq
      !>  [%tip 2 2 2 ~]
      !>  (cut:cor sp)
    %+  expect-eq
      !>  [%tip ff 2 2 ~]
      !>  (cut:cor bp)
    %+  expect-eq
      !>  [%tip coll 5 5 ~]
      !>  (cut:cor ac)
    %+  expect-eq
      !>  [%bin 2 2 [2 ~] 268.435.456 [%tip colk 4 4 ~] [%tip 3 3 3 ~]]
      !>  (cut:cor po)
    %+  expect-eq
      !>  [%tip coll 2 2 ~]
      !>  (cut:cor re)
  ==
::
++  test-vip
  ;:  weld
    %+  expect-eq
      !>  `[%tip 1 1 1 ~]
      !>  (vip:cor ~ 1 1 1)
    %+  expect-eq
      !>  `(put:cor sp 3 3 3)
      !>  (vip:cor sp 3 3 3)
    %+  expect-eq
      !>  [`[2 2] [%bin 1 1 [1 ~] 8.388.608 ~ [%tip 2 3 3 ~]]]
      !>  (vip:cor sp 2 3 3)
    %+  expect-eq
      !>  [`[4 4] [%tip coll 5 5 [[colk 6 6] ~ colk]]]
      !>  (vip:cor ac colk 6 6)
    %+  expect-eq
      !>  [`[5 5] [%tip colk 4 4 [[coll 6 6] ~ coll]]]
      !>  (vip:cor ac coll 6 6)
  ==
::
++  test-see
  ;:  weld
    %+  expect-eq
      !>  [~ ~]
      !>  (see:cor ~ 1 1)
    %+  expect-eq
      !>  [`[1 1] [%bin 2 2 [2 ~] 8.388.608 [%tip 1 10 1 ~] ~]]
      !>  (see:cor sp 1 10)
    %+  expect-eq
      !>  [`[2 2] [%bin 1 1 [1 ~] 8.388.608 ~ [%tip 2 10 2 ~]]]
      !>  (see:cor sp 2 10)
    %+  expect-eq
      !>  [`[4 4] [%tip coll 5 5 [[colk 10 4] ~ colk]]]
      !>  (see:cor ac colk 10)
    %+  expect-eq
      !>  [`[5 5] [%tip colk 4 4 [[coll 10 5] ~ coll]]]
      !>  (see:cor ac coll 10)
  ==
--


