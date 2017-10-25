::  API: input auth2 client_secret_*.json for .googleapis.com
::
::::  /hoon/google/init-oauth2/hood/gen
  ::
/?  314
/-  sole
/+  old-zuse
=,  old-zuse
::
::::
  ::
=,  sole
=,  html
=,  format
:-  %ask
|=  $:  {now/@da eny/@uvJ bec/beak}
        {arg/$@($~ {jon/json $~})}
        $~
    ==
^-  (sole-result {$write-sec-atom p/host:eyre q/@})
%+  sole-yo  leaf+"Accepting credentials for https://*.googleapis.com"
=+  hot=[%& /com/googleapis]
=-  ?~  arg  -
    (fun.q.q jon.arg)
%+  sole-lo
  [%& %oauth-json "json credentials: "]
%+  sole-go  apex:de-json
|=  jon/json
=+  ~|  bad-json+jon
    =-  `{cid/@t cis/@t}`(need (rep jon))
    rep=(ot web+(ot 'client_id'^so 'client_secret'^so ~) ~):dejs-soft:format
%+  sole-so  %write-sec-atom    :: XX typed pair
[hot (of-wain:format cid cis ~)]
