::  :dns-collector: manually "complete" (fulfill) requests
::
::::  /hoon/complete/dns-collector/gen
  ::
/-  *dns, *sole
/+  *generators
:-  %say
|=  [^ [who=@p addr=@if =turf ~] ~]
:-  %dns-complete
^-  [ship binding]
[who [%if addr] turf]
