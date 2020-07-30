/-  post
^?
=<  [post .]
=,  post
|%
++  sign
  |=  [our=ship now=time =hash]
  ^-  signature
  =/  =life  .^(life %j /=life/(scot %da now)/(scot %p our))
  =/  =ring  .^(ring %j /=vein/(scot %da now)/(scot %ud life))
  :+  `@ux`(sign:as:(nol:nu:crub:crypto ring) hash)
    our
  life
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
  =(`hash (tear:as:crub:crypto b.u.deed p.signature))
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
