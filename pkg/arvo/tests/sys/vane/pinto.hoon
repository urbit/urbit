/+  *test, pinto
::/=  pinto  /:  /===/sys/vane/pinto  /!noun/
::
=/  scry=sley
  |=  [* (unit (set monk)) tem=term bem=beam]
  ^-  (unit (unit cage))
  =-  (~(get by -) tem bem)
  %-  ~(gas by *(map [term beam] (unit cage)))
  :~  :-  cx+[[~nul %home da+~1234.5.6] /hoon/foo/lib]
      `hoon+!>('%bar')
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/baz/lib]
      `hoon+!>('!:  [12 13]  !:  -')
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/hoon/sys]
      `hoon+!>('=<  |=(* ~)  |%  ++  one  1  --')
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/arvo/sys]
      `hoon+!>('|%  ++  is  one  --')
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/zuse/sys]
      `hoon+!>('|%  ++  aint  is  --')
  ==
=/  ford-gate  (pinto !>(..zuse))
=/  ford-core  (ford-gate ~nul ~1234.5.6 0xdead.beef scry)
|%
++  test-make-one-file  ^-  tang
  ::
  =^  fex  ford-gate
    %:  call:ford-core
      *duct  *type
      %make
      desk=%home
      case=`da+~1234.5.6
      all=(sy /lib/foo/hoon ~)
    ==
  %+  expect-eq
    !>  %bar
    =/  cad  card:(head fex)
    ?>  ?=(%give -.cad)
    =/  gif  p.cad
    ?>  ?=(%make -.gif)
    =/  all  all.gif
    ?>  ?=([* ~ ~] all)
    =/  [key=path val=(each vase tang)]  n.all
    ?>  =(/lib/foo/hoon key)
    ?>  ?=(%& -.val)
    p.val
--
