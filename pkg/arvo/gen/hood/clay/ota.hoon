::  Kiln: Continuously merge local desk from (optionally-)foreign one
::
::::  /hoon/ota/hood/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        arg=?([%disable ~] [her=@p sud=?(~ [@tas ~])])
        ~
    ==
:-  %kiln-install
?:  ?=([%disable ~] arg)
  [%base p.bec %base]
:+  %base  her.arg
?@(sud.arg %kids -.sud.arg)
