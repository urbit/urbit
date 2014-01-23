!:
::  /=try=/bin/ed/hoon
::
::  ed25519 signature toy.
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
|=  [sk=@ pk=@ m=@ ~]
^-  bowl
=+  si=(sign:ed m sk pk)
:_  ~  :_  ~
:-  %$
!>  `@ux`si
