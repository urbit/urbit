!:
::  /=try=/bin/aesen/hoon
::
::  AES encryption toy. Don't use this for anything real.
::  It will threaten non-termination and send your private key to
::  General Alexander via singing telegram.
::
=>  %=    .
        +
      =>  +
      ^/===/lib/cryo
    ==
|=  *
|=  [k=@I m=@ ~]
^-  bowl
=+  c=(en:aes k m)
:_  ~  :_  ~
:-  %$
!>  c
