::  Find list of currently running Behn timers
:-  %say
|=  [[now=@da tick=@ud @ our=@p ^] ~ ~]
=;  timers
  [%tang >timers< ~]
.^  (list [date=@da =duct])
    %bx  (en-bema [our %$ [da+now ud+tick]] /debug/timers)
==
