/-  seer
=,  seer
|%
::  monadic bind for seer
++  rapt
  |*  [r=mold b=mold]
  |*  a=mold
  |=  [m=(seer r a) f=$-(a (seer r b))]
  ^-  (seer r b)
  ?:  ?=(%done -.m)  (f p.m)
  =;  g=$-(r (seer r b))
    m(k g)
  |=  =r
  ^$(m (k.m r))
++  read
  |*  [r=mold a=mold]
  |=  [s=(view r) m=(seer r a)]
  ^-  (tale a)
  ?:  ?=(%done -.m)  m
  =*  p  p.m
  =/  b=(seen r)  (s p)
  ?^  b  $(m (k.m p.b))
  ?:  =(%wait b)
    wait+p
  mute+p
++  scry
  |*  [r=mold p=path]
  ^-  (seer r r)
  scry+p^|=(a=r done+a)
++  make-view
  |*  v=mold
  |=  kvs=(list (pair path (seen v)))
  =/  m  (~(gas by *(map path (seen v))) kvs)
  |=  =path
  ^-  (seen v)
  =/  got  (~(get by m) path)
  ?~  got  %wait
  u.got
--
