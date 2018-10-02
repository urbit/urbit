::  API: input oauth1 application credentials for domain
::
::::  /hoon/init-oauth1/hood/gen
  ::
/?  314
/+  sole, old-zuse
=,  old-zuse
::
::::
  ::
=,  ask:sole
:-  %ask
|=  $:  {now/@da eny/@uvJ bec/beak}
        {arg/$@(~ {dom/path ~})}
        ~
    ==
^-  (sole-result:sole {$write-sec-atom p/host q/@})
=-  ?~  arg  -
    (fun.q.q [%& dom.arg])
%+  prompt
  [%& %oauth-hostname "api hostname: https://"]
%+  parse  thos:urlp
|=  hot/host
?:  ?=(%| -.hot)
  ~|(%ips-unsupported !!)
%+  prompt
  [%& %oauth-client "consumer key: "]
%+  parse  (boss 256 (star prn))
|=  key/@t
%+  prompt  
  [%& %oauth-secret "consumer secret: "]
%+  parse  (boss 256 (star prn))
|=  sec/@t
%+  output  %write-sec-atom    :: XX typed pair
[hot (of-wain:format key sec ~)]
