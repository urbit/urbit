::  test gall request queue fix, which implicates ames and gall
::
/+  *test, v=test-ames-gall
|%
++  test-setup-nec-bud
  %-  run-chain
  |.  :-  %|
    =+  nec-bud:v
    =/  =task:gall  [%deal [~nec ~nec] %sub %poke watch+!>(~bud)]
    =^  t1  gall.nec
      %:  gall-check-call:v  gall.nec  ~1111.1.1  *roof  [~[/foo] task]
        :~  ^-  move:gall-bunt:v
            :*  ~[/foo]  %give  [%unto %poke-ack ~]
            ==
            ^-  move:gall-bunt:v
            :*  ~[/init]  %pass  /use/sub/0w1.d6Isf/out/~bud/pub/1/sub
                [%g %deal [~nec ~bud] %pub %watch /foo]
        ==  ==
      ==
  :-  t1  |.  :-  %&
  %+  expect-eq
    !>  bud
  !>  bud
--
