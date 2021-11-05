/-  post
^?
=<  [post .]
=,  post
|%
++  jael-scry
  |*  [=mold our=ship desk=term now=time =path]
  .^  mold
    %j
    (scot %p our)
    desk
    (scot %da now)
    path
  ==
++  sign
  |=  [our=ship now=time =hash]
  ^-  signature
  =+  (jael-scry ,=life our %life now /(scot %p our))
  =+  (jael-scry ,=ring our %vein now /(scot %ud life))
  :+  `@ux`(sign:as:(nol:nu:crub:crypto ring) hash)
    our
  life
::
++  is-signature-valid
  |=  [our=ship =signature =hash now=time]
  ^-  ?
  =+  (jael-scry ,lyf=(unit @) our %lyfe now /(scot %p q.signature))
  ::  we do not have a public key from ship at this life
  ::
  ?~  lyf  %.y
  ?.  =(u.lyf r.signature)  %.y
  =+  %:  jael-scry
        ,deed=[a=life b=pass c=(unit @ux)]
        our  %deed  now  /(scot %p q.signature)/(scot %ud r.signature)
      ==
  ::  if signature is from a past life, skip validation
  ::  XX: should be visualised on frontend, not great.
  ?.  =(a.deed r.signature)  %.y
  ::  verify signature from ship at life
  ::
  =/  them
    (com:nu:crub:crypto b.deed)
  =(`hash (sure:as.them p.signature))
::
++  are-signatures-valid
  |=  [our=ship =signatures =hash now=time]
  ^-  ?
  =/  signature-list  ~(tap in signatures)
  |-
  ?~  signature-list
    %.y
  ?:  (is-signature-valid our i.signature-list hash now)
    $(signature-list t.signature-list)
  %.n
--
