::  API: input basic auth credentials for domain
::
::::  /hoon/init-auth-basic/hood/gen
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
  [%& %auth-user "username: "]
%+  parse  (boss 256 (star ;~(less col prn)))
|=  usr/@t
%+  prompt
  [%| %auth-passwd "password: "]
%+  parse  (boss 256 (star prn))
|=  pas/@t
%+  produce  %write-sec-atom    :: XX typed pair
[hot (crip (en-base64:mimes:html (rap 3 usr ':' pas ~)))]
