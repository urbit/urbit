/-  seer
=,  seer
|%
::  monadic bind for seer
++  rapt
  |*  [r=mold a=mold b=mold]
  |=  [a=(seer r a) f=$-(a (seer r b))]
  ^-  (seer r b)
  ?-  -.a
    %&  (f p.a)
    %|  :-  %|
        :-  p.p.a             ::  path
        |=  r=*
        $(a (q.p.a r))
  ==
++  consult
  |*  [r=mold a=mold]
  |=  [rof=(sky r) eye=(seer r a)]
  ^-  (tone a)
  ?-  -.eye
    %&  done+p.eye
    %|  =*  p  p.p.eye
        =/  b=(boon a)  (rof p)
        ?-  b
          %blok  blok+p
          %mute  mute+p
          ^      $(eye (q.p.eye p.b))
        ==
  ==
--
