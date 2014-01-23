!:
::  /=try=/bin/ed/hoon
::
::  ed25519 as a toy in Hoon.
::
::  Needless to say, don't use this for anything real. Not only will it be
::  embarrassingly slow, but it'll probably mail your private key to the NSA.
::
=>  %=    .
        +
      =>  +
      ^/=main=/lib/cryo
    ==
|=  [est=time eny=@uw]
|=  [sk=@ m=@ ~]
^-  bowl
=+  pk=(puck:ed sk)
~&  [%pk `@ux`pk]
=+  si=(sign:ed m sk pk)
~&  [%si `@ux`si]
:_  ~  :_  ~
:-  %$
!>
=+  ^=  sis
    ?:  (veri:ed si m pk)
  'valid sig'
'invalid sig'
=+  ^=  fos
    ?.  (veri:ed si +(m) pk)
  'detected forgery'
'undetected forgery'
[sis fos]
