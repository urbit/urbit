::
::  /=try=/bin/aestest/hoon
::
|=  [est=time eny=@uw]
|=  ~
:_  ~  :_  ~
=+  i=0
:-  %$
!>
|-  ^-  @
?:  =(1.000.000 i)  0
=+  n=`@rd`(end 6 1 (en:aesc eny i))
=+  m=`@rd`(rsh 6 1 (en:aesc +(eny) i))
~&  [%try `@ux`n `@ux`m]
=+  a=(add:rd n m)
=+  b=(sub:rd n m)
=+  c=(mul:rd n m)
=+  d=(div:rd n m)
=+  suc=%.y
?.  suc
  ~|  [%fail i `@ux`n `@ux`m]
  !!
?:  &(!=(0 i) =(0 (mod i 1.000)))
  $(i +(i))
$(i +(i))
