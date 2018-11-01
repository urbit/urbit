::  API: input oauth2 application credentials for domain
::
::::  /hoon/init-oauth2/hood/gen
  ::
/?  314
/-  sole
/+  generators
::
::::
  ::
=,  generators
:-  %ask
|=  $:  {now/@da eny/@uvJ bec/beak}
        {arg/$@(~ {dom/path ~})}
        ~
    ==
^-  (sole-result:sole {$write-sec-atom p/host:eyre q/@})
=-  ?~  arg  -
    (fun.q.q [%& dom.arg])
%+  prompt
  [%& %oauth-hostname "api hostname: https://"]
%+  parse  thos:de-purl:html
|=  hot/host:eyre
?:  ?=(%| -.hot)
  ~|(%ips-unsupported !!)
%+  prompt
  [%& %oauth-client "client id: "]
%+  parse  (boss 256 (star prn))
|=  cid/@t
%+  prompt
  [%& %oauth-secret "client secret: "]
%+  parse  (boss 256 (star prn))
|=  cis/@t
%+  produce  %write-sec-atom    :: XX typed pair
[hot (of-wain:format cid cis ~)]
