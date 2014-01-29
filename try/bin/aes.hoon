!:
::  /=try=/bin/aes/hoon
::
=>  %=    .
        +
      =>  +
      ^/=main=/lib/cryo
    ==
|=  *
|=  ~
^-  bowl
=+  k=0x1f1e.1d1c.1b1a.1918.1716.1514.1312.1110.0f0e.0d0c.0b0a.0908.0706.0504.0302.0100
=+  m=0xffee.ddcc.bbaa.9988.7766.5544.3322.1100
=+  e=(en:aes k m)
:_  ~  :_  ~
:-  %$
=+  o=(en:aes 2 1)
=+  d=(de:aes 2 o)
!>
[`@ux`e `@ux`(de:aes k e) `@ux`o `@ux`d]
