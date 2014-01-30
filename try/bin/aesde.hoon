!:
::  /=try=/bin/aesde/hoon
::
::  AES decryption toy. Don't use this for anything real.
::
=>  %=    .
        +
      =>  +
      ^/=main=/lib/cryo
    ==
|=  *
|=  [k=@I c=@ ~]
^-  bowl
=+  m=(de:aes k c)
:_  ~  :_  ~
:-  %$
!>  m
