/+  *test
=/  cor  (lu @ @)
=/  colk=@  281.474.976.718.163
=/  coll=@  281.474.976.723.900
=/  tf=@  0xffff.ffff.ffff
=/  ff=@  0xffff.ffff.ffff.ffff
=/  em=pes:cor  (new:cor 3)
=/  sp=pes:cor  [3 2 2 [%bin 1 0 [1 ~] 8.388.608 ~ [%tip 2 1 2 ~]]]
=/  bp=pes:cor  [3 2 2 [%bin tf 0 [1 ~] 268.435.456 [%tip ff 1 2 ~] ~]]
=/  ac=pes:cor  [3 2 2 [%tip colk 0 1 [[coll 1 2] ~ coll]]]
=/  ful=pes:cor  :^  3  3  3
                 [%bin 1 0 [1 ~] 33.554.432 [%tip 2 1 2 ~] [%tip 3 2 3 ~]]
=/  woa=pes:cor  (put:cor ac 3 3)
=/  max=pes:cor  ac(tic (dec 0x7fff.ffff))
::
|%
++  test-get
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (get:cor em 1)
    %+  expect-eq
      !>  `[1 [3 2 3 [%bin 2 1 [2 ~] 8.388.608 [%tip 1 2 1 ~] ~]]]
      !>  (get:cor sp 1)
    %+  expect-eq
      !>  `[1 [3 2 3 [%tip coll 1 2 [[colk 2 1] ~ colk]]]]
      !>  (get:cor ac colk)
    %+  expect-eq
      !>  `[2 [3 2 3 [%tip colk 0 1 [[coll 2 2] ~ coll]]]]
      !>  (get:cor ac coll)
    %+  expect-eq
      !>  `[1 [3 0 0 ~]]
      !>  (get:cor max colk)
  ==
::
++  test-put
  ;:  weld
    %+  expect-eq
      !>  +(siz.em)
      !>  siz:(put:cor em 1 1)
    %+  expect-eq
      !>  +(tic.em)
      !>  tic:(put:cor em 1 1)
    %+  expect-eq
      !>  siz.sp
      !>  siz:(put:cor sp 1 10)
    %+  expect-eq
      !>  +(siz.sp)
      !>  siz:(put:cor sp 10 10)
    %+  expect-eq
      !>  siz.bp
      !>  siz:(put:cor bp tf 10)
    %+  expect-eq
      !>  +(siz.bp)
      !>  siz:(put:cor bp 10 10)
    %+  expect-eq
      !>  siz.ac
      !>  siz:(put:cor ac colk 10)
    %+  expect-eq
      !>  siz.ac
      !>  siz:(put:cor ac coll 10)
    %+  expect-eq
      !>  +(siz.ac)
      !>  siz:(put:cor ac 10 10)
    %+  expect-eq
      !>  [3 3 4 [%bin 2 1 [2 ~] 134.217.728 [%tip 3 2 3 ~] [%tip 4 3 4 ~]]]
      !>  (put:cor ful 4 4)
    %+  expect-eq
      !>  :^  3  3  4
          :*  %bin
              coll
              1
              [2 ~]
              268.435.456
              ~
              [%bin 3 2 [3 ~] 134.217.728 ~ [%tip 4 3 4 ~]]
          ==
      !>  (put:cor woa 4 4)
    %+  expect-eq
      !>  em
      !>  (put:cor max 3 3)
  ==
::
++  test-del
  ;:  weld
    %+  expect-eq
      !>  em
      !>  (del:cor em 1)
    %+  expect-eq
      !>  [3 1 2 [%tip 2 1 2 ~]]
      !>  (del:cor sp 1)
    %+  expect-eq
      !>  [3 1 2 [%tip ff 1 2 ~]]
      !>  (del:cor bp tf)
    %+  expect-eq
      !>  [3 1 2 [%tip coll 1 2 ~]]
      !>  (del:cor ac colk)
    %+  expect-eq
      !>  [3 1 2 [%tip colk 0 1 ~]]
      !>  (del:cor ac coll)
    %+  expect-eq
      !>  [3 2 3 [%bin 2 1 [2 ~] 33.554.432 ~ [%tip 3 2 3 ~]]]
      !>  (del:cor ful 1)
  ==
--

