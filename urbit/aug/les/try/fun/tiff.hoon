!:
::  /=try=/toy/diff/hoon
::
|=  [who=seat est=time eny=@uw was=path]
|=  *
|=  ~
:_  ~
=<  main
|%
++  main
  ^-  (list gift)
  =+  pod=test
  |-  ^-  (list gift)
  ?~  pod  ~
  =+  dis=(loss p.i.pod q.i.pod)
  ?>  =((lurk p.i.pod (lusk p.i.pod q.i.pod dis)) q.i.pod)
  :-  [%la %leaf "{p.i.pod} to {q.i.pod}: loss {dis}"]
  $(pod t.pod)
::
++  fake
  =+  ram=(shax eny)
  =+  inx=0
  |-  ^-  tape
  ?:  =(inx 128)  ~
  [[(add 'a' (end 0 2 ram))] $(inx +(inx), ram (rsh 0 2 ram))]
::
++  test
  =+  inx=0
  |-  ^-  (list ,[p=tape q=tape])
  ?:  =(inx 32)  ~
  [[fake(eny (add 1 eny)) fake(eny (add 2 eny))] $(inx +(inx), eny (add 3 eny))]
--
