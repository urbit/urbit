::  Transcendental functions library, compatible with @rs
::
=/  tau  .6.28318530717
=/  pi   .3.14159265358
=/  e    .2.718281828
=/  rtol  .1e-5
~%  %trig  ..part  ~
|%
:: Factorial, $x!$
::
++  factorial
  ::~%  %factorial  ..$  ~
  ~/  %factorial
  |=  x=@rs  ^-  @rs
  =/  t=@rs  .1
  ~&  "In Hoon version"
  |-  ^-  @rs
  ?:  =(x .0)  t
  ?:  =(x .1)  t
  $(x (sub:rs x .1), t (mul:rs t x))
:: Absolute value, $|x|$
::
++  absolute
  |=  x=@rs  ^-  @rs
  ?:  (gth:rs x .0)
    x
  (sub:rs .0 x)
:: Exponential function, $\exp(x)$
::
++  exp
  ~/  %exp
  |=  x=@rs  ^-  @rs
  =/  rtol  .1e-5
  =/  p   .1
  =/  po  .-1
  =/  i   .1
  |-  ^-  @rs
  ?:  (lth:rs (absolute (sub:rs po p)) rtol)
    p
  $(i (add:rs i .1), p (add:rs p (div:rs (pow-n x i) (factorial i))), po p)
:: Integer power, $x^n$
::
++  pow-n
  ~/  %pow-n
  |=  [x=@rs n=@rs]  ^-  @rs
  ?:  =(n .0)  .1
  =/  p  x
  |-  ^-  @rs
  ?:  (lth:rs n .2)
    p
  ::~&  [n p]
  $(n (sub:rs n .1), p (mul:rs p x))
--

