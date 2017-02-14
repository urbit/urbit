::  API: input oauth2 application credentials for domain
::
::::  /hoon/init-oauth2/hood/gen
  ::
/?  314
/-  sole
::
::::
  ::
=,  sole
:-  %ask
|=  $:  {now/@da eny/@uvJ bec/beak}
        {arg/$@($~ {dom/path $~})}
        $~
    ==
^-  (sole-result {$write-sec-atom p/host q/@})
=-  ?~  arg  -
    (fun.q.q [%& dom.arg])
%+  sole-lo
  [%& %oauth-hostname "api hostname: https://"]
%+  sole-go  thos:urlp
|=  hot/host
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
[hot (of-wain cid cis ~)]
