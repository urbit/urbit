/+  *test
|%
++  test-negated-wuttis
  %-  expect-fail  |.
  %+  ride  %noun
  '''
  =/  u=(unit)  ~
  ?.  !?=(~ u)  %blah
  ?-(u ~ %empty, ^ %value)
  '''
::
++  test-negated-wuttis-2
  %-  expect-success  |.
  %+  ride  %noun
  '''
  =/  u=$@(~ [~ u=*])  [~ 123]
  ?>(!?=(~ u) u.u)
  '''
::
++  test-wutpam-specialization
  %-  expect-success  |.
  %+  ride  %noun
  '''
  =/  n=[p=* q=* r=*]  [[1 2] [3 4] [5 6]]
  ?>  ?&(?=([p=* *] p.n) ?=([p=* *] q.n) ?=([p=* *] r.n))
  [p.p.n p.q.n p.r.n]
  '''
::
++  test-wutbar-no-specialization
  %-  expect-success  |.
  %+  ride  %noun
  '''
  =/  n=[p=* q=* r=*]  [[1 2] [3 4] [5 6]]
  ?>  ?|(?=([p=* *] p.n) ?=([p=* *] q.n) ?=([p=* *] r.n))
  [p.n q.n r.n]
  '''
::
++  test-wutbar-no-specialization-demorgan
  %-  expect-success  |.
  %+  ride  %noun
  '''
  =/  n=[p=* q=* r=*]  [[1 2] [3 4] [5 6]]
  ?>  !?&(!?=([p=* *] p.n) !?=([p=* *] q.n) !?=([p=* *] r.n))
  [p.n q.n r.n]
  '''
::
++  test-sequential-narrowing
  %-  expect-success  |.
  %+  ride  -:!>(unit=unit)
  '''
  =/  c=[o=(unit (unit @)) d=(unit (unit @))]  [``1 ~]
  ?>  ?&(?=(^ o.c) ?=(^ u.o.c) ?=(~ d.c))
  [u.u.o.c `~`d.c]
  '''
::
++  test-sequential-narrowing-demorgan
  %-  expect-success  |.
  %+  ride  -:!>(unit=unit)
  '''
  =/  c=[o=(unit (unit @)) d=(unit (unit @))]  [``1 ~]
  ?>  !?|(!?=(^ o.c) !?=(^ u.o.c) !?=(~ d.c))
  [u.u.o.c `~`d.c]
  '''
--
