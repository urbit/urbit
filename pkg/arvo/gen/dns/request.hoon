::  DNS: configure ip address
::
::::  /hoon/request/dns/gen
  ::
/-  *dns, *sole
/+  *generators
:-  %ask
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=$@(~ [addr=@if ~])]
        ~
    ==
^-  (sole-result [%dns-address address])
=*  our  p.bec
=/  rac  (clan:title our)
::
?:  ?=(%czar rac)
  =/  msg1  "galaxy domain requests must be made out-of-band"
  =/  msg2  "use :dns|auto if you already have an urbit domain"
  =/  msg3  "see XX for more details or to BYOD"
  %+  print  leaf+msg3
  %+  print  leaf+msg2
  (print leaf+msg1 no-product)
::
?:  ?=(?(%earl %pawn) rac)
  =/  msg1  "domain names are not provided for comets and moons"
  =/  msg2  "see XX for BYOD"
  %+  print  leaf+msg2
  (print leaf+msg1 no-product)
::  invoke parser with arg if present
::
=-  ?~  arg  -
    (fun.q.q addr.arg)
%+  prompt
  [%& %dns-address "ipv4 address: "]
%+  parse
  ^-  $-(nail (like @if))
  ;~(pfix ;~(pose dot (easy ~)) lip:ag)
|=  addr=@if
?:  (reserved:eyre addr)
  =/  msg  "unable to bind reserved ipv4 address {(scow %if addr)}"
  (print leaf+msg no-product)
%-  produce
[%dns-address %if addr]
