::  DNS: configure ip address
::
::::  /hoon/authority/dns/gen
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
=-  ?~  arg  -
    (fun.q.q addr.arg)
%+  prompt
  [%& %dns-address "ipv4 address: "]
%+  parse
  `$-(nail (like @if))`;~(pfix ;~(pose dot (easy ~)) lip:ag)
|=  addr=@if
?:  (reserved:eyre addr)
  =/  msg  "unable to bind reserved ipv4 address {(scow %if addr)}"
  (print leaf+msg no-product)
%-  produce
[%dns-address %if addr]
