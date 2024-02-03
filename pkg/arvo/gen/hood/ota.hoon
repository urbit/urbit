::  Kiln: Continuously merge local desk from (optionally-)foreign one
::
::::  /hoon/ota/hood/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  $:  [@ @ @ our=@p ^]
        arg=?([%disable ~] [her=@p sud=?(~ [@tas ~])])
        ~
    ==
:-  %kiln-install
?:  ?=([%disable ~] arg)
  [%base our %base]
:+  %base  her.arg
?@(sud.arg %kids -.sud.arg)
