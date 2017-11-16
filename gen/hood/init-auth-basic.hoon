::  API: input basic auth credentials for domain
::
::::  /hoon/init-auth-basic/hood/gen
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
  [%& %auth-user "username: "]
%+  sole-go  (boss 256 (star ;~(less col prn)))
|=  usr/@t
%+  sole-lo
  [%| %auth-passwd "password: "]
%+  sole-go  (boss 256 (star prn))
|=  pas/@t
%+  sole-so  %write-sec-atom    :: XX typed pair
[hot (crip (en-base64:mimes:html (rap 3 usr ':' pas ~)))]
