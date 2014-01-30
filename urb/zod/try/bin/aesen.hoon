!:
::  /=try=/bin/aesen/hoon
::
::  AES encryption toy. Don't use this for anything real.
::
=>  %=    .
        +
      =>  +
      ^/=main=/lib/cryo
    ==
|=  *
|=  [k=@I m=@ ~]
^-  bowl
=+  c=(en:aes k m)
:_  ~  :_  ~
:-  %$
!>  c
