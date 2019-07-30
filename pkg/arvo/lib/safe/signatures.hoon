/-  ring, safe-applet, common=safe-common, client=safe-client
/+  ring
::
::  Signature verification utilities for Safe
::
|%
++  verify-signature
  |=  $:  =signature-type:safe-applet
          route=path
          =full-signature:safe-applet
          noun=*
      ==
  ^-  ?
  ::  %inherit isn't a real signature type, but a directive to use the parent's
  ::  type. there should be no instances of it.
  ::
  ?>  !=(%inherit signature-type)
  ::  TODO: We don't want to go full signature verification until we have the
  ::  parts of the inter-ship replication system going, since we'll have to
  ::  write the code which 
  ::
  %.y
  ::  ::  if the signature-type is ship, you must have a ship signature
  ::  ::
  ::  ?:  =(%ship signature-type)
  ::    ?.  ?=([%ship *] full-signature)
  ::      %.n
  ::    ::
  ::    ~&  %todo-verify-ship-signature
  ::    %.y
  ::  ::  all other signatures are variants on ring signatures
  ::  ::
  ::  ?.  ?=([%ring *] full-signature)
  ::    %.n
  ::  ::  todo: this rest here.
  ::  ::
  ::  %.y
::  +message-sign: signs an outbound message
::
::    Applets are able to request how signatures are performed and thus our
::    signing function needs to deal with all the concretized ways we can sign
::    something.
::
++  sign-message
  |=  [our=@p now=@da eny=@uvJ =signature-request:common data=*]
  ^-  full-signature:safe-applet
  ?-    -.signature-request
      %ship
    ::  if the signature type is just %ship, we sign data with our
    ::  authentication key.
    ::
    [%ship ~zod 5]
  ::
      %unlinked
    ::  if the signature is unlinked, we don't actually have to use
    ::  :requested-route in our calculation.
    ::
    ::  TODO: Real ring signatures don't work on fakezods!?
    [%ring *ring-signature:ring]
    ::  [%ring (sign:ring our now eny data ~ invited.signature-request)]
  ::
      %linked
    ::  if the signature is linked, we link on the requested scope.
    ::
    ::  TODO: Real ring signatures don't work on fakezods!?
    [%ring *ring-signature:ring]
    ::  :-  %ring
    ::  (sign:ring our now eny data `scope.signature-request invited.signature-request)
  ==
::  +sign-user-event: performs the low level signing on 
::
++  sign-user-event
  |=  $:  our=@p
          now=@da
          eny=@uvJ
          root-request=signature-request:common
          path-request=signature-request:common
          route=path
          user-event=*
      ==
  ^-  [full-signature:safe-applet full-signature:safe-applet path *]
  ::  build the two signatures
  ::
  ::    the inner-signature is passed to the target node. the outer-signature
  ::    is passed to the root node. the outer-signature signs the inner-signature.
  ::
  =/  inner-signature
    (sign-message our now eny path-request [route user-event])
  =/  outer-signature
    (sign-message our now eny root-request [inner-signature route user-event])
  ::
  [outer-signature inner-signature route user-event]
--
