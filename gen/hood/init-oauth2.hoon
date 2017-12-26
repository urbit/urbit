::  API: input oauth2 application credentials for domain
::
::::  /hoon/init-oauth2/hood/gen
  ::
/?  314
/-  sole
/+  old-zuse
=,  old-zuse
::
::::
  ::
=,  sole
:-  %ask
|=  $:  {now/@da eny/@uvJ bec/beak}
        {arg/$@($~ {dom/path $~})}
        $~
    ==
^-  (sole-result {$write-sec-atom p/host:eyre q/@})
=-  ?~  arg  -
    (fun.q.q [%& dom.arg])
%+  sole-lo
  [%& %oauth-hostname "api hostname: https://"]
%+  sole-go  thos:de-purl:html
|=  hot/host:eyre
?:  ?=($| -.hot)
  ~|(%ips-unsupported !!)
%+  sole-lo
  [%& %oauth-client "client id: "]
%+  sole-go  (boss 256 (star prn))
|=  cid/@t
%+  sole-lo  
  [%& %oauth-secret "client secret: "]
%+  sole-go  (boss 256 (star prn))
|=  cis/@t
%+  sole-so  %write-sec-atom    :: XX typed pair
[hot (of-wain:format cid cis ~)]
