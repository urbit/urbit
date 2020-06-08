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
  =/  =pass
    .^  pass
      %j
      /=deed/(scot %da now)/(scot %p q.signature)/(scot %ud p.signature)
    ==
  ::  verify signature against hash of post
  ?:  %.y
    %.n
  %.y
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
