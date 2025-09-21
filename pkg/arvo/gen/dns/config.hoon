::  DNS: configure ip address
::
::::  /hoon/request/dns/gen
  ::
/-  dns, *sole
/+  *generators
:-  %ask
|=  [[now=@da eny=@uvJ bec=beak] ~ ~]
|^  ^-  (sole-result (cask [(each address:dns @t) dock ? ?]))
?.  ?=(?(%king %duke) (clan:title p.bec))
  (print leaf+"dns: only planets and stars may use this service" no-product)
=/  msg1  "dns: This tool will configure a domain for your urbit."
=/  msg2  "dns: Your urbit's HTTP interface must be accessible on port 80"
=/  msg3  "dns: from the public internet. This tool has the option of skipping"
=/  msg4  "dns: self-checks but you should not do so unless you have an"
=/  msg5  "dns: unusual configuration and know exactly what you're doing."
%+  print  leaf+msg5
%+  print  leaf+msg4
%+  print  leaf+msg3
%+  print  leaf+msg2
%+  print  leaf+msg1
=+  .^([ins=@ud sec=(unit @ud)] %e /(scot %p p.bec)/ports/(scot %da now))
=+  .^(turfs=(set turf) %e /(scot %p p.bec)/domains/(scot %da now))
%^  maybe  !=(80 ins)  (port-check ins)
|=  ~
%+  prompt  [%& %dns-address "IP address (leave empty for automatic): "]
%+  parse  ;~(pose (stag ~ parse-ipv4) (easy ~))
|=  ip=(unit @if)
?:  &(?=(^ ip) (reserved:eyre u.ip))
  =/  msg  "dns: unable to bind reserved IPv4 address {(tail (scow %if u.ip))}"
  (print leaf+msg no-product)
%^  maybe  ?=(^ turfs)  reset-check
|=  reset=_|
%+  prompt  [%& %dns-self-check "Perform recommended self-checks (Y/n)? "]
%+  parse  (parse-yn &)
|=  self-check=?
=/  addr=(each address:dns @t)
  ?~  ip  [%| 'https://zod.urbit.org/~/ip']
  [%& %if u.ip]
=/  collector=dock  [~deg %dns-collector]
(produce %helm-dns-config addr collector self-check reset)
::
++  maybe
  |*  [=flag fun=$-(* *) mor=$-(* *)]
  ?.  flag  (mor)
  (fun mor)
::
++  parse-ipv4
  =+  tod=(sear |=(a=@ `(unit @)`?:((gth a 255) ~ `a)) (ape:ag ted:ab))
  (bass 256 ;~(plug tod (stun [3 3] ;~(pfix dot tod))))
::
++  parse-yn
  |=  default=?
  ;~  pose
    %+  cold  |
    ;~(plug (mask "nN") ;~(pose (mask "oO") ;~(less next (easy ~))))
  ::
    %+  cold  &
    ;~  plug
      (mask "yY")
      ;~(pose ;~(plug (mask "eE") (mask "sS")) ;~(less next (easy ~)))
    ==
    (cold default (easy ~))
  ==
::
++  port-check
  |=  ins=@ud
  |*  mor=$-(~ *)
  %+  prompt
    :+  %&  %dns-address
    :~  :-  [~ `%r `%w]
        "HTTP port {(a-co:co ins)} bound instead of 80. Continue (y/N)?"
        ' '
    ==
  %+  parse  (parse-yn |)
  |=  cont=?
  ?:  cont
    (mor)
  (print leaf+"dns: aborted" no-product)
::
++  reset-check
  |*  mor=$-(_| *)
  %+  prompt
    [%& %dns-reset "Reset existing DNS configuration (y/N)? "]
  (parse (parse-yn |) mor)
--
  

