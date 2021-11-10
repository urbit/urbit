/+  *test
::
=/  cor  (up @ @)
=/  pr  (gas:cor ~ ~[[1 1 1] [2 2 2] [3 3 3] [4 4 4]])
=/  qr  (gas:cor ~ ~[[4 4 4] [3 3 3] [2 2 2] [1 1 1]])
=/  rr  (gas:cor ~ ~[[1 5 1] [2 2 2] [3 3 3] [4 4 4]])
=/  sr  (gas:cor ~ ~[[2 2 2] [1 1 1] [4 4 4] [3 3 3]])
=/  lnode-a=lnode
  [n=[k=2 p=2 v=2] l=~ m=1 r=[%rlos s=1 p=[n=[k=3 p=3 v=3] l=~ m=2 r=~]]]
=/  lnode-b=lnode
  [n=[k=1 p=1 v=1] l=~ m=1 r=[%llos s=1 p=[n=[k=2 p=2 v=2] l=~ m=2 r=~]]]
=/  lnode-c=lnode
  [n=[k=1 p=1 v=1] l=~ m=1 r=~]
=/  lnode-d=lnode
  :^    [3 3 3]
      :+  %llos
        3
      :^    [5 5 5]
          [%rlos 1 [[6 6 6] ~ 5 ~]]
        6
      [%rlos 1 [[2 2 2] ~ 1 ~]]
    2
  [%rlos 1 [[4 4 4] ~ 3 ~]]
=/  lnode-e=lnode
  :^    [5 5 5]
      [%rlos 1 [[6 6 6] ~ 5 ~]]
    6
  :+  %rlos
    3
  :^    [3 3 3]
      [%rlos 1 [[2 2 2] ~ 1 ~]]
    2
  [%rlos 1 [[4 4 4] ~ 3 ~]]
=/  psq=pro
  :+  [1 1 1]
    [%rlos 5 lnode-d]
  4
=/  qsp=pro
  :+  [1 1 1]
    [%llos 5 lnode-e]
  4
=/  tsq=pro
  :+  [1 1 1]
    :+  %rlos
      11
    :^    [1 1 1]
        :+  %rlos
          5
        :^    [3 3 3]
            :+  %llos
              3
            :^    [5 5 5]
                [%rlos 1 [[6 6 6] ~ 5 ~]]
              6
            [%rlos 1 [[2 2 2] ~ 1 ~]]
          2
        [%rlos 1 [[4 4 4] ~ 3 ~]]
      4
    :+  %llos
      5
    :^    [5 5 5]
        [%rlos 1 [[6 6 6] ~ 5 ~]]
      6
    :+  %rlos
      3
    :^    [3 3 3]
        [%rlos 1 [[2 2 2] ~ 1 ~]]
      2
    [%rlos 1 [[4 4 4] ~ 3 ~]]
  4
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
++  test-size
  ;:  weld
    %+  expect-eq
      !>  0
      !>  (size ~)
    %+  expect-eq
      !>  1
      !>  (size:cor [%llos s=1 p=[n=[k=1 p=1 v=1] l=~ m=1 r=~]])
  ==
::
++  test-llos
  ;:  weld
    %+  expect-eq
      !>  [%llos s=2 p=lnode-a]
      !>  (llos:cor lnode-a)
    %+  expect-eq
      !>  [%llos s=2 p=lnode-b]
      !>  (llos:cor lnode-b)
    %+  expect-eq
      !>  [%llos s=1 lnode-c]
      !>  (llos:cor lnode-c)
  ==
::
++  test-rlos
  ;:  weld
    %+  expect-eq
      !>  [%rlos s=2 p=lnode-a]
      !>  (rlos:cor lnode-a)
    %+  expect-eq
      !>  [%rlos:cor s=2 p=lnode-b]
      !>  (rlos:cor lnode-b)
    %+  expect-eq
      !>  [%rlos:cor s=1 lnode-c]
      !>  (rlos:cor lnode-c)
  ==
::
++  test-lbal
  ;:  weld
    %+  expect-eq
      !>  [%llos s=2 p=lnode-a]
      !>  (lbal:cor lnode-a)
    %+  expect-eq
      !>  [%llos s=2 p=lnode-b]
      !>  (lbal:cor lnode-b)
    %+  expect-eq
      !>  [%llos s=1 lnode-c]
      !>  (lbal:cor lnode-c)
    %+  expect-eq
      !>  :+  %llos
            5
          :^    [3 3 3]
              :+  %llos
                3
              :^    [5 5 5]
                  [%rlos 1 [[6 6 6] ~ 5 ~]]
                6
              [%rlos 1 [[2 2 2] ~ 1 ~]]
            2
          [%rlos 1 [[4 4 4] ~ 3 ~]]
      !>  (lbal:cor lnode-d)
    %+  expect-eq
      !>  :+  %llos
            5
          :^    [5 5 5]
              [%rlos 1 [[6 6 6] ~ 5 ~]]
            6
          :+  %rlos
             3
          :^    [3 3 3]
              [%rlos 1 [[2 2 2] ~ 1 ~]]
            2
          [%rlos 1 [[4 4 4] ~ 3 ~]]
      !>  (lbal:cor lnode-e)
  ==
::
++  test-rbal
  ;:  weld
    %+  expect-eq
      !>  [%rlos s=2 p=lnode-a]
      !>  (rbal:cor lnode-a)
    %+  expect-eq
      !>  [%rlos s=2 p=lnode-b]
      !>  (rbal:cor lnode-b)
    %+  expect-eq
      !>  [%rlos s=1 lnode-c]
      !>  (rbal:cor lnode-c)
    %+  expect-eq
      !>  :+  %rlos
            5
          :^    [3 3 3]
              :+  %llos
                3
              :^    [5 5 5]
                  [%rlos 1 [[6 6 6] ~ 5 ~]]
                6
              [%rlos 1 [[2 2 2] ~ 1 ~]]
            2
          [%rlos 1 [[4 4 4] ~ 3 ~]]
      !>  (rbal:cor lnode-d)
    %+  expect-eq
      !>  :+  %rlos
            5
          :^    [5 5 5]
              [%rlos 1 [[6 6 6] ~ 5 ~]]
            6
          :+  %rlos
             3
          :^    [3 3 3]
              [%rlos 1 [[2 2 2] ~ 1 ~]]
            2
          [%rlos 1 [[4 4 4] ~ 3 ~]]
      !>  (rbal:cor lnode-e)
  ==
::++  test-llbal  !!
::++  test-lrbal  !!
::++  test-rlbal  !!
::++  test-rrbal  !!
::::
++  test-llsin
  ;:  weld
    %+  expect-eq
      !>  :+  %rlos
            s=2
          ^=  p
          :^    n=[k=3 p=3 v=3]
              l=[%llos s=1 p=[n=[k=2 p=2 v=2] l=~ m=1 r=~]]
            m=2
          r=~
      !>  (llsin:cor lnode-a)
    %+  expect-eq
      !>  :+  %llos
            s=2
          ^=  p
          :^    n=[k=1 p=1 v=1]
              l=[%rlos s=1 p=[n=[k=2 p=2 v=2] l=~ m=1 r=~]]
            m=2
          r=~
      !>  (llsin:cor lnode-b)
  ==
::
++  test-rrsin
  ;:  weld
    %+  expect-eq
      !>  :+  %llos
            s=5
          ^=  p
          :^    n=[k=5 p=5 v=5]
              l=[%rlos s=1 p=[n=[k=6 p=6 v=6] l=~ m=5 r=~]]
            m=6
          ^=  r
          :+  %rlos
            3
          :^    [3 3 3]
              [%rlos 1 [[2 2 2] ~ 1 ~]]
            2
          [%rlos 1 [[4 4 4] ~ 3 ~]]
      !>  (rrsin:cor lnode-d)
    %+  expect-eq
      !>  :+  %rlos
            s=5
          ^=  p
          :^    n=[k=5 p=5 v=5]
              ~
            m=5
          :+  %llos
            4
          :^    [6 6 6]
              ~
            6
          :+  %rlos
            3
          :^    [3 3 3]
              [%rlos 1 [[2 2 2] ~ 1 ~]]
            2
          [%rlos 1 [[4 4 4] ~ 3 ~]]
      !>  (rrsin:cor lnode-e)
  ==
::::
::++  test-rlsin  !!
::++  test-lrsin  !!
::::
::++  test-lldub  !!
::++  test-rrdub  !!
::++  test-lrdub  !!
::++  test-rldub  !!
::::
++  test-toy
  ;:  weld
    %+  expect-eq
      !>  psq
      !>  (toy:cor ~ psq)
    %+  expect-eq
      !>  psq
      !>  (toy:cor psq ~)
    %+  expect-eq
      !>  tsq
      !>  (toy:cor psq qsp)
  ==
::
++  test-sec
  ;:  weld
    %+  expect-eq
      !>  :+  [2 2 2]
            :+  %rlos
              4
            :^    [3 3 3]
                :+  %llos
                  2
                [[5 5 5] [%rlos 1 [[6 6 6] ~ 5 ~]] 6 ~]
              2
            [%rlos 1 [[4 4 4] ~ 3 ~]]
          4
      !>  (sec:cor ?>(?=(^ psq) [t.psq m.psq]))
  ==
::
++  test-win
  ;:  weld
    %-  expect  !>  (win:cor [1 %one] [2 %two])
    %-  expect  !>  !(win:cor [2 %one] [1 %two])
    %-  expect  !>  !(win:cor [1 %one] [1 %two])
    %-  expect  !>  (win:cor [1 [2 2]] [1 [2 1]])
  ==
::
++  test-zero
  ;:  weld
    %-  expect
      !>  (zero:cor 0 2)
    %-  expect
      !>  !(zero:cor 4 5)
  ==
::
++  test-gone
  ;:  weld
    %-  expect  !>  (gone:cor 0 2 3)
    %-  expect  !>  !(gone:cor 0 1 3)
  ==
::
++  test-mask
  ;:  weld
    %+  expect-eq
      !>  (dec 0xffff.ffff)
      !>  (mask:cor 1)
    %+  expect-eq
      !>  (dec 0xffff.ffff)
      !>  (mask:cor 0xffff.ffff)
    %+  expect-eq
      !>  3.758.096.384
      !>  (mask:cor 0xf000.0000)
  ==
::
++  test-pert:cor
  ;:  weld
    %+  expect-eq
      !>  0
      !>  (pert:cor 1 1)
    %+  expect-eq
      !>  2
      !>  (pert:cor 2 1)
    %+  expect-eq
      !>  2.147.483.648
      !>  (pert:cor 2 0xffff.ffff)
  ==
::
++  test-high
  ;:  weld
    %+  expect-eq
      !>  1
      !>  (high:cor 0b1)
    %+  expect-eq
      !>  2
      !>  (high:cor 0b10)
    %+  expect-eq
      !>  4
      !>  (high:cor 0b100)
    %+  expect-eq
      !>  128
      !>  (high:cor 0b1000.0000)
  ==
::
++  test-lex
  ;:  weld
    %-  expect
      !>  (lex:cor [1 2] [3 4])
    %-  expect
      !>  (lex:cor [1 3] [2 4])
    %-  expect
      !>  !(lex:cor [2 3] [1 4])
    %-  expect
      !>  !(lex:cor [1 4] [1 2])
  ==
::
++  test-fuse
  =/  foo=pri
    :*  %bin
        k=1.901.865.568
        p=1
        v=[k=1 v=1 t=~]
        m=33.554.432
        ^=  l  :*  %tip
                   k=1.904.972.904
                   p=2
                   v=[k=2 v=2 t=~]
               ==
        ^=  r  :*  %tip
                   k=1.923.673.882
                   p=3
                   v=[k=3 v=3 t=~]
               ==
    ==
  =/  bar=pri
    :*  %bin
        k=1.904.972.904
        p=2
        v=[k=2 v=2 t=~]
        m=33.554.432
        l=~
        ^=  r  :*  %tip
                   k=1.923.673.882
                   p=3
                   v=[k=3 v=3 t=~]
               ==
    ==
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  (fuse:cor 0 ~ ~)
    %+  expect-eq
      !>  bar
      !>  ?>(?=(%bin -.foo) (fuse:cor m.foo l.foo r.foo))
  ==
::
++  test-funk
  ;:  weld
    %+  expect-eq
      !>  [%tip *@ *@ *buc]
      !>  (funk *@ *@ *buc *@ ~ ~)
    %+  expect-eq
      !>  [%bin *@ *@ *buc *@ ~ [%tip *@ *@ *buc]]
      !>  (funk *@ *@ *buc *@ ~ [%tip *@ *@ *buc])
    %+  expect-eq
      !>  [%bin *@ *@ *buc *@ [%tip *@ *@ *buc] ~]
      !>  (funk *@ *@ *buc *@ [%tip *@ *@ *buc] ~)
  ==
::
++  test-wane
  ;:  weld
    %+  expect-eq
      !>  [%tip *@ *@ *buc]
      !>  (wane *@ *@ *buc *@ ~ ~)
    %+  expect-eq
      !>  [%bin *@ *@ *buc *@ [%tip *@ *@ *buc] ~]
      !>  (wane *@ *@ *buc *@ [%tip *@ *@ *buc] ~)
    %+  expect-eq
      !>  [%bin *@ *@ *buc *@ ~ [%tip *@ *@ *buc]]
      !>  (wane *@ *@ *buc *@ ~ [%tip *@ *@ *buc])
  ==
::
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
