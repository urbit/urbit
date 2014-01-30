!:
::  /=try=/bin/aesde/hoon
::
::  AES decryption toy. Don't use this for anything real.
::  It will threaten non-termination and send your private key to
::  General Alexander via singing telegram.
::
=>  %=    .
        +
      =>  +
      ^/===/lib/cryo
    ==
|=  *
|=  [k=@I c=@ ~]
^-  bowl
=+  m=(de:aes k c)
:_  ~  :_  ~
:-  %$
!>  m
