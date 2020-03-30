/+  *test, pinto
/=  hun  /:  /===/sys/hoon  /hoon/
/=  rav  /:  /===/sys/arvo  /hoon/
/=  zus  /:  /===/sys/zuse  /hoon/
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
      :-  cs+[[~nul %home da+~1234.5.6] /int/mar]
      `path+!>(/mar/int/hoon)
  ::
      :-  ax+[[~nul %home da+~1234.5.6] /hoon/hoon/sys]
      `hoon+!>(hun)
  ::
      :-  ax+[[~nul %home da+~1234.5.6] /hoon/arvo/sys]
      `hoon+!>(rav)
  ::
      :-  ax+[[~nul %home da+~1234.5.6] /hoon/zuse/sys]
      `hoon+!>(zus)
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/hoon/sys]
      `hoon+!>(hun)
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/arvo/sys]
      `hoon+!>(rav)
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/zuse/sys]
      `hoon+!>(zus)
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/int/mar]
      :+  ~  %hoon
      !>
      '''
      !:
      =>  |%
          +$  int  @ud
          +$  dif  [pos=? val=int]
          --
      |_  sam=int
      ++  grad
        |%
        ++  diff
          |=  new=int
          ^-  dif
          ?:  (gte new sam)
            &+(sub new sam)
          |+(sub sam new)
        ++  form  %int-diff
        ++  join  |=([a=dif b=dif] (some b))
        ++  mash  |=([a=[=ship =desk =dif] b=[=ship =desk =dif]] (some dif.b))
        ++  pact
          |=  =dif
          ^-  int
          ?-  pos.dif
            %&  (add sam val.dif)
            %|  (sub sam val.dif)
          ==
        --
      --
      '''
  ==
=/  ford-gate  (pinto !>(..zuse))
=/  ford-core  (ford-gate ~nul ~1234.5.6 0xdead.beef scry)
=*  dais  dais:ford-core
|%
++  test-make-one-file  ^-  tang
  =^  fex  ford-gate
    %:  call:ford-core
      *duct  *type
      %make
      desk=%home
      case=`da+~1234.5.6
      fiz=(sy /lib/foo/hoon ~)
      maz=~
      caz=~
    ==
  %+  expect-eq
    !>  %bar
    =/  cad  card:(head fex)
    ?>  ?=(%give -.cad)
    =/  gif  p.cad
    ?>  ?=(%made -.gif)
    ?>  =(~ maz.gif)
    ?>  =(~ caz.gif)
    =/  fiz  fiz.gif
    ?>  ?=([* ~ ~] fiz)
    =/  [key=path val=(each vase tang)]  n.fiz
    ?>  =(/lib/foo/hoon key)
    ?>  ?=(%& -.val)
    p.val
::
++  test-make-one-mark  ^-  tang
  =^  fex  ford-gate
    %:  call:ford-core
      *duct  *type
      %make
      desk=%home
      case=`da+~1234.5.6
      fiz=~
      maz=(sy %int ~)
      caz=~
    ==
  =/  =dais
    =/  cad  card:(head fex)
    ~|  fex
    ?>  ?=(%give -.cad)
    =/  gif  p.cad
    ?>  ?=(%made -.gif)
    ?>  =(~ fiz.gif)
    ?>  =(~ caz.gif)
    =/  maz  maz.gif
    ?>  ?=([* ~ ~] maz)
    =/  [key=mark val=(each dais tang)]  n.maz
    ?>  =(%int key)
    ?>  ?=(%& -.val)
    p.val
  ;:  weld
    %+  expect-eq
      !>  13
      (~(pact dais !>(17)) !>(|+4))
    %+  expect-eq
      !>  [`?`& 4]
      (~(diff dais !>(13)) !>(17))
  ==
--
