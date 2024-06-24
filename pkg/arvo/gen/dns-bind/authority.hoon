::  DNS: configure zone authority
::
::::  /hoon/authority/dns/gen
  ::
/-  *dns-bind, *sole
/+  *generators
:-  %ask
|=  $:  [now=@da tick=@ud @ our=@p ^]
        [arg=$@(~ [dom=turf ~])]
        ~
    ==
^-  (sole-result [%dns-authority authority])
::  XX must be evaluated outside tapp core due to +mule
::
=/  =hart:eyre  .^(hart:eyre %e /(scot %p our)/host/real)
::  XX terrible
=/  domain  /com/googleapis
=/  code
  %-  crip
  +:(scow %p .^(@p %j (en-bema [our %code [da+now ud+tick]] /(scot %p our))))
=/  secrets
  .^(@t %cx (en-bema [our %base [da+now ud+tick]] :(weld /sec domain /atom)))
::
=-  ?~  arg  -
    (fun.q.q [%& dom.arg])
%+  prompt
  [%& %dns-domain "dns domain: "]
%+  parse  thos:de-purl:html
|=  hot=host:eyre
?:  ?=(%| -.hot)
  ~|(%ips-unsupported !!)
%+  prompt
  [%& %project "gcloud project: "]
%+  parse  urs:ab
|=  project=@ta
%+  prompt
  [%& %zone "dns zone: "]
%+  parse  urs:ab
|=  zone=@ta
%-  produce
[%dns-authority [p.hot %gcloud project zone [code hart secrets] ~]]
