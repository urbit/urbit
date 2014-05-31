::
::  /=main=/bin/curve/hoon
::
=>  %=    .
        +
      =>  +
      ^/===/lib/cryo
    ==
|=  [est=time eny=@uw]
|=  ~
^-  bowl
:_  ~  :_  ~
:-  %$
!>
=+  pk=48.084.050.389.777.770.101.701.157.326.923.977.117.
       307.187.144.965.043.058.462.938.058.489.685.090.437
=+  sk=(cla:curv 19.076.158.533.740.022.697.853.188.432.810.
                 029.468.508.100.820.210.985.396.154.491.514.718.125.885.303)
=+  ska=(gen:curv eny)
=+  sky=(gen:curv (shax eny))
:*
  %public-test
  :+
    [%reference-public pk]
    [%calculate-public (curve:curv sk 9)]
    [%equals =((curve:curv sk 9) pk)]
  %diffie-hellman
  [%alice-secret ska]
  [%bob-secret sky]
  [%alice-calcs (curve:curv sky (curve:curv ska 9))]
  [%bobp-calcs (curve:curv ska (curve:curv sky 9))]
  %equals
  =((curve:curv sky (curve:curv ska 9)) (curve:curv ska (curve:curv sky 9)))
==
