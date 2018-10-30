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
^-  (sole-result [%dns-command command])
=*  our  p.bec
=-  ?~  arg  -
    (fun.q.q addr.arg)
%+  prompt
  [%& %dns-address "ipv4 address: "]
%+  parse
  `$-(nail (like @if))`;~(pfix dot lip:ag)
|=  addr=@if
%-  produce
[%dns-command %ip %if addr]
