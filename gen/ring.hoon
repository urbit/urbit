/+  *ring
::
::
:-  %say
|=  [[now=time eny=@ our=ship ^] ~ ~]
:-  %noun
::  deterministically generate keys with insecure numbers for testing purposes
::
=/  key-num=@  2
::  create a list of public/private keypairs
::
=/  keys=(list [pk=@udpoint sk=@udscalar])
  =|  count=@
  =|  keys=(list [pk=@udpoint sk=@udscalar])
  ::
  |-
  ?:  =(count 60)
    keys
  ::  using key-num as our "secret" key.
  ::
  =/  pk=@udpoint  (scalarmult-base:ed:crypto key-num)
  ::
  $(keys [[pk key-num] keys], count +(count), key-num +(key-num))
::  create the key set the interface expects
::
=/  key-set=(set @udpoint)
  (sy (turn keys head))
::
=/  my-key  (snag 13 keys)
=/  my-public-key=@udpoint  (head my-key)
=/  my-private-key=@udscalar  (tail my-key)
::
~&  %start----------signing
::
=/  message  "blah"
=/  scope  [~ [%link-scope 52]]
::=/  scope  ~
::
=/  signature=ring-signature
  (sign message scope key-set my-public-key my-private-key eny)
~&  [%signature signature]
::
~&  %start----------verification
::
=/  verified
  (verify message scope key-set signature)
::
~&  [%verified verified]
verified
