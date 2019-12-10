::  DNS: configure automatically
::
::::  /hoon/auto/dns/gen
  ::
/-  *sole
/+  *generators
:-  %ask
|=  [[now=@da eny=@uvJ bec=beak] ~ ~]
^-  (sole-result [%dns-auto ~])
=*  our  p.bec
=/  rac  (clan:title our)
::
?:  ?=(?(%earl %pawn) rac)
  =/  msg1  "domain names are not provided for comets and moons"
  =/  msg2  "see XX for BYOD"
  %+  print  leaf+msg2
  (print leaf+msg1 no-product)
::
?.  ?=(%czar rac)
  =/  msg1  ":dns|auto is only supported for galaxies"
  =/  msg2  "use :dns|request with your ship's public IP address"
  =/  msg3  "see XX for more details, or to BYOD"
  %+  print  leaf+msg3
  %+  print  leaf+msg2
  (print leaf+msg1 no-product)
(produce [%dns-auto ~])
