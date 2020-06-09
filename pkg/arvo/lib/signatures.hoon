/-  post
^?
=<  [post .]
=,  post
|%
::
::  sha256 noun hash
::  
++  sha256-mug
  |=  yux/*  ^-  @ux  ^-  @
  ?@  yux
    (shax yux)
  (shax (jam yux))
::
++  is-signature-valid
  |=  [=signature =hash now=time]
  ^-  ?
  =/  deed=(unit [a=life b=pass c=(unit @ux)])
    .^  (unit [life pass (unit @ux)])
      %j
      /=deed/(scot %da now)/(scot %p q.signature)/(scot %ud p.signature)
    ==
  ::  we do not have a public key from ship
  ::
  ?~  deed  %.y
  ::  we do not have a public key from ship at this life
  ::
  ?.  =(a.u.deed r.signature)  %.y
  ::  verify signature from ship at life
  ::
  ?=(^ (tear:as:crub:crypto b.u.deed p.signature))
::
++  are-signatures-valid
  |=  [=signatures =hash now=time]
  ^-  ?
  =/  signature-list  ~(tap in signatures)
  |-
  ?~  signature-list
    %.y
  ?:  (is-signature-valid i.signature-list hash now)
    $(signature-list t.signature-list)
  %.n
--
