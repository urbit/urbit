!:
::  /=main=/bin/with/hoon
::
|=  [est=@da *]
|=  arg=(list path)
^-  bowl
:_  ~
^-  (list gift)
=<  abet
|%
++  abet  ?:(=(~ arg) [[%sc ~] ~] (turn arg |=(a=path `gift`[%sc ~ (with a)])))
++  with
  |=  pax=path  ^-  skit
  ?>  ?=([@ @ @ *] pax)
  :+  ?:(=((scot %da est) i.t.t.pax) ~ [~ i.t.t.pax])
    [i.pax i.t.pax ~]
  t.t.t.pax
--
