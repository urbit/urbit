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
=+  k=(con (lsh 3 16 (en:aesc eny i)) (en:aesc +(eny) i))
=+  m=(en:aesc (add 2 eny) i)
=+  suc==(m (de:aesc k (en:aesc k m)))
?.  suc
  ~|  [%fail `@ux`k `@ux`m]
  !!
?:  &(!=(0 i) =(0 (mod i 10.000)))
  ~&  [i `@ux`k `@ux`m]
  $(i +(i))
$(i +(i))
