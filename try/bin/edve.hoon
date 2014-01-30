!:
::  /=try=/bin/edpk/hoon
::
::  ed25519 signature verification toy.
::
=>  %=    .
        +
      =>  +
      ^/===/lib/cryo
    ==
|=  [est=time eny=@uw]
|=  [pk=@ s=@ m=@ ~]
^-  bowl
:_  ~  :_  ~
:-  %$
!>  ?:  (veri:ed s m pk)
      'valid sig'
    'invalid sig'
