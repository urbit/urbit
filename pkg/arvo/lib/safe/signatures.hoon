/-  *ring, safe-applet, common=safe-common, client=safe-client, tapp-sur=tapp
/+  async, lib-ring=ring
::
=+  (async [sign card contract]:(tapp-sur ,_!! ,_!!))
::
::  Signature verification utilities for Safe
::
|%
++  async-verify-signature
  |=  $:  our=@p
          now=@da
          invited=(set @p)
          =signature-type:safe-applet
          =full-signature:safe-applet
          noun=*
      ==
  =/  m  (async ,?)
  ^-  form:m
  ::  %inherit isn't a real signature type, but a directive to use the parent's
  ::  type. there should be no instances of it.
  ::
  ?>  !=(%inherit signature-type)
  ::  if the signature-type is ship, you must have a ship signature
  ::
  ?:  =(%ship signature-type)
    ?.  ?=([%ship *] full-signature)
      (pure:m %.n)
    ::
    ?.  (~(has in invited) ship.full-signature)
      ~&  [%uninvited-ship ship.full-signature]
      (pure:m %.n)
    ::
    ~&  %todo-verify-ship-signature
    (pure:m %.y)
  ::  all other signatures are variants on ring signatures
  ::
  ?.  ?=([%ring *] full-signature)
    (pure:m %.n)
  ::  the signature set must be equivalent to the current invited set
  ::
  ?.  =(invited (~(run in participants.ring-signature.full-signature) head))
    ~&  %invalid-invited-set
    (pure:m %.n)
  ::
  (verify-async:lib-ring our now noun ring-signature.full-signature)
::  +message-sign: signs an outbound message
::
::    Applets are able to request how signatures are performed and thus our
::    signing function needs to deal with all the concretized ways we can sign
::    something.
::
++  async-sign-message
  |=  [our=@p now=@da eny=@uvJ =signature-type-request:common data=*]
  =/  m  (async ,full-signature:safe-applet)
  ^-  form:m
  ?-    -.signature-type-request
      %ship
    ::  if the signature type is just %ship, we sign data with our
    ::  authentication key.
    ::
    (pure:m [%ship ~zod 5])
  ::
      %unlinked
    ::  if the signature is unlinked, we don't actually have to use
    ::  :requested-route in our calculation.
    ::
    ;<  sig=ring-signature  bind:m
      (sign-async:lib-ring our now eny data ~ invited.signature-type-request)
    ::
    (pure:m [%ring sig])
  ::
      %linked
    ::  if the signature is linked, we link on the requested scope.
    ::
    ;<  sig=ring-signature  bind:m
      %-  sign-async:lib-ring  :*
        our
        now
        eny
        data
        `scope.signature-type-request
        invited.signature-type-request
      ==
    ::
    (pure:m [%ring sig])
  ==
::  +sign-request: actually performs the signing.
::
::    TODO: I should just get the data from tapp instead of passing along?
::
::    TODO: Is using entropy twice actually safe here? Unlikely!
::
++  async-sign-request
  |=  $:  our=@p
          now=@da
          eny=@uvJ
          =signing-request:common
      ==
  =/  m  (async ,client-to-server:common)
  ^-  form:m
  ::  build the two signatures
  ::
  ::    the inner-signature is passed to the target node. the outer-signature
  ::    is passed to the root node. the outer-signature signs the inner-signature.
  ::
  ;<  inner-signature=full-signature:safe-applet  bind:m
    %-  async-sign-message  :*
      our
      now
      (shas eny %msg)
      path-request.signing-request
      [route user-event]:signing-request
    ==
  ::
  ;<  outer-signature=full-signature:safe-applet  bind:m
    %-  async-sign-message  :*
      our
      now
      (shas eny %root)
      root-request.signing-request
      [inner-signature route.signing-request user-event.signing-request]
    ==
  ::
  %-  pure:m
  [outer-signature inner-signature route.signing-request user-event.signing-request]
--
