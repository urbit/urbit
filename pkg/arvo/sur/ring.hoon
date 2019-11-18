|%
::  +raw-ring-signature: low level ring signature type
::
::    The :s field of a ring signature grows O(n) with the number of
::    participants in the ring.
::
++  raw-ring-signature
  $:  ch0=@
      ::
      s=(list @)
      ::  linked ring signature tag
      ::
      ::    Two linked ring signatures with the same link scope can be shown to
      ::    have been made by the same private key, leading to Sybil
      ::    resistance...but if your private keys are compromised, your
      ::    adversary can determine which signatures you made.
      ::
      y=(unit @udpoint)
  ==
::  +ring-signature: higher level ring signature type
::
::    This contains all the identifying information to verify a ring signature
::    in an urbit context.
::
++  ring-signature
  $:  ::  a ring signature is computed over a set of public keys. the
      ::  participants set is not those keys, but static references to them.
      ::
      participants=(set [ship=@p =life])
      ::  the linkage scope this signature was made on
      ::
      link-scope=(unit *)
      ::  the rest of the low level ring signature is appended
      ::
      raw=raw-ring-signature
  ==
::
+$  ring-group
  $:  ::  a ring signature is computed over a set of public keys. the
      ::  participants set is not those keys, but static references to them.
      ::
      participants=(set [ship=@p =life])
      ::  the linkage scope this signature was made on
      ::
      link-scope=(unit *)
  ==
--
