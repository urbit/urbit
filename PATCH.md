zuze patch:
```hoon
++  decompress-point
  |=  dat=@
  ^-  pont
::  ~&  "custom decompress point"
  =+  x=(end 3 w dat)
  ?>  =(3 (mod ^p 4))
  =+  y=(exp.p (div +(^p) 4) :(sum.p (exp.p 3 x) (pro.p a x) b))
  =+  s=(rsh 3 32 dat)
  ~|  [`@ux`s `@ux`dat]
  ?>  |(=(2 s) =(3 s))
  ::  check parity
  ::
  =?  y  !=((sub s 2) (mod y 2))
    (sub ^p y)
  [x y]
```
