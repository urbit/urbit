::  Basic authentication
::
::::  /hoon/basic-auth/lib
  ::
|%
++  keys  @t
--
::
::::
  ::
|_  bal/(bale keys)
++  auth-header
  ^-  {term cord}
  ?~  key.bal
    ~_  leaf+"Run |init-auth-basic {<`path`dom.bal>}"
    ~|(%basic-auth-no-key !!)
  [%authorization (cat 3 'Basic ' key.bal)]
::
++  standard
  |%
  ++  out-adding-header
    |=  a/hiss  ^-  sec-move
    =+  aut=auth-header
    ~&  aut=aut
    [%send %_(a q.q (~(add ja q.q.a) -.aut +.aut))]
  --
--
