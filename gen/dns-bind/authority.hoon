::  DNS: configure zone authority
::
::::  /hoon/authority/dns/gen
  ::
/-  *dns, *sole
/+  *generators
:-  %ask
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=$@(~ [dom=turf ~])]
        ~
    ==
^-  (sole-result [%dns-command %authority authority])
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
[%dns-command %authority [p.hot %gcloud project zone ~]]
