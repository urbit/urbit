::  Create a galactic generator and ames-ready fingerprint
::
::::  /hoon/pope/gen
  ::
/?    310
::  Input twitter keys
/-  sole
/+  generators
=,  [sole generators]
::
=+  cryp=crub:crypto
:-  %ask
|=  $:  {now/@da eny/@uvJ bec/beak}
        {{who/ship ~} ~}
    ==
^-  (sole-result (cask tang))
%+  print  leaf+"generating carrier {(scow %p who)} (#{(scow %ud who)})"
%+  prompt  [%| %pope-pass "passphrase: "]
%+  parse  (boss 256 (star prn))
|=  fra/@t
=+  bur=(shaz (add who (shaz fra)))
=+  arc=(pit:nu:cryp 512 bur)
%+  produce  %tang
:~  leaf+"generator: {(scow %uw bur)}"
    leaf+"fingerprint: {(scow %uw fig:ex:arc)}"
==
