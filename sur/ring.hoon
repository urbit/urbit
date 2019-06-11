|%
::  +ring-signature: low level ring signature type
::
::    The :s field of a ring signature grows O(n) with the number of
::    participants in the ring.
::
++  ring-signature
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
--
