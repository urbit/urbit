::  cyst: list ops for recursive lists where the container is an item too
::
|*  [open=$-(* (list)) save=$-([* (list)] *)]
|%
::
::TODO  we _can_ do this, but can't do +slag, because we can't start halfway..
++  scag
  |*  [a=@ud b=(list)]
  =-  ->
  |-  ^-  [_a _b]
  ?~  b  [a ~]
  ?:  =(0 a)  [0 ~]
  =/  nex  (open i.b)
  ?~  nex
    =/  nex  $(a (dec a), b t.b)
    [-.nex i.b +.nex]
  =/  new  $(a (dec a), b nex)
  ?:  =(0 -.new)
    ::TODO  actually doesn't work either, if internal list must be a lest
    [0 (save i.b +.new) ~]
  =/  nex  $(a -.new, b t.b)
  :: $(a -.nex, b)
  [-.nex i.b +.nex]
::
++  turn
  |*  [a=(list) b=gate]
  ?~  a  ~
  =.  i.a  (b i.a)
  =.  i.a
    =+  (open i.a)
    ?~  -  i.a
    (save i.a (turn - b))
  [i=i.a t=$(a t.a)]
::
++  prod
  |*  [a=@ud b=(list) c=gate]
  =-  ?>(=(0 -<) ->)
  |-  ^-  [_a _b]
  :: ?~  b  [a ~]
  ?:  =(0 a)
    ?~  b  !!
    [0 (c i.b) t.b]
  ?~  b  [a ~]
  =/  nex  (open i.b)
  ?~  nex
    =/  nex  $(a (dec a), b t.b)
    [-.nex i.b +.nex]
  =/  new  $(a (dec a), b (open i.b))
  ?:  =(0 -.new)
    [0 (save i.b +.new) t.b]
  =/  nex  $(a -.new, b t.b)
  [-.nex i.b +.nex]
::
++  snag
  |*  [a=@ud b=(list)]
  =-  ?>(?=(^ -) ->)
  |-  ^-  $@(@ud [~ _?>(?=(^ b) i.b)])
  ?~  b  a
  ?:  =(0 a)  `i.b
  =.  a  (dec a)
  =/  new  $(b (open i.b))
  ?^  new  new
  $(a new, b t.b)
::
++  lent
  |*  a=(list)
  ?~  a  0
  %+  add  1
  %+  add  $(a (open i.a))
  $(a t.a)
--
