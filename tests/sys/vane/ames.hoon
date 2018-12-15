/+  *test
::
/=  ames-raw  /:  /===/sys/vane/ames
              /!noun/
=/  type-spear  -:!>(ames-raw)
::
=/  test-pit=vase  !>(.)
=/  ames-gate  (ames-raw test-pit)
::
|%
::  tests that %ames asks for private keys on %init
::
++  test-init
  =^  results1  ames-gate
    =/  hen=duct
      [/ /term/1 / ~]
    =/  wir=wire
      /our/~nul
    %-  ames-call
    :*  ames-gate
        now=~1234.5.6
        call-args=[hen type=*type %soft %init ~nul]
        [[hen %pass wir %j %vein ~] [hen %pass / %j %turf ~] ~]
    ==
  ::
  results1
::
++  ames-call
  |=  $:  ames-gate=_ames-gate
          now=@da
          call-args=[=duct wrapped-task=(hypo (hobo task:able:ames-gate))]
          expected-moves=(list move:ames-gate)
      ==
  ^-  [tang _ames-gate]
  ::
  =/  ames  (ames-gate our=~nul now=now eny=`@`0xdead.beef scry=*sley)
  ::
  =^  moves  ames-gate
    %-  call:ames  call-args
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output ames-gate]
--
