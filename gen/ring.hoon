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
=/  keys=(list [pk=point sk=@])
  =|  count=@
  =|  keys=(list [pk=point sk=@])
  ::
  |-
  ?:  =(count 3)
    keys
  ::
  ::  In the end, what was the problem with the key generation was that
  ::  etch/scam/deco/puck don't return things in the real format we need. The
  ::  math above assumes that the public key above is priv-key * G. 
  ::
  ::  =/  sk=@  (etch:ed:crypto (scam:ed:crypto bb:ed:crypto key-num))
  ::  =/  pk=point  (need (deco:ed (puck:ed:crypto sk)))
  =/  pk=point  (point-base-mul key-num)
  ::
  ::
  $(keys [[pk key-num] keys], count +(count), key-num +(key-num))
::  create the key set the interface expects
::
=/  key-set=(set point)
  (sy (turn keys head))
::
=/  my-key  (snag 0 keys)
=/  my-public-key=point  (head my-key)
=/  my-private-key=@  (tail my-key)
::
~&  %start----------signing
::
=/  message  "blah"
=/  scope  [~ [%link-scope 52]]
::  =/  scope  ~
::
=/  signature
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
