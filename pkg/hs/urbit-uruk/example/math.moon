::  Twos Exponent
=/  bex
  ~/  1  bex
  |=  a
  (a (mul 2) 1)

::  Left Shift
=/  lsh
  ~/  2  lsh
  (S (K mul) bex)
  ::  |=  (exponent num)
  ::  (mul (bex exponent) num)

::  sub->fub, like dec->fec
=/  fub
  ~/  2  fub
  |=  (a b)
  ?-  (sub a b)
    x  0
    y  y
  ==

::  Natural Less Than Equal
=/  lte
  ~/  2  lte
  |=  (a b)
  ?-  (sub a b)
    x  yea
    y  (zer y)
  ==

::  Natural Less Than
=/  lt
  ~/  2  lt
  |=  (a b)
  ?-  (sub a b)
    x  yea
    y  nah
  ==

::  Natural Number Division
=/  div
  ::
  ~/  2  div
  |=  (dividend divisor)
  ?:  (zer divisor)
    (ded 'divide-by-zero')
  =/  loop
    ..  $
    |=  (dividend count)
    ?:  (lt dividend divisor)
      count
    ($ (fub dividend divisor) (inc count))
  ::
  (loop dividend 0)

::  Right Shift
=/  rsh
  ~/  2  rsh
  |=  (exponent num)
  (div num (bex exponent))

::
::  Note: I'm modifying all the things which take a bloq to take a real number
::  of bits instead. +bloq was a mistake.
::

::  Measure the number of bits in a natural numbers.
::
::  (Equivalent to hoon's `(met 0 a)`.)
=/  met
  ~/  1  met
  |=  x
  =/  loop
    ..  $
    |=  (x count)
    ?:  (zer x)
      count
    ($ (rsh 1 x) (inc count))
  (loop x 0)

:: Concatenate natural numbers
=/  cat
  ~/  2  cat
  |=  (b c)
  (add (lsh (met b) c) b)


:: TODO list for tape to cord:
::
:: rap
:: crip


::  Exports
:*
  :-  bex  (bex 128)
  :-  lsh  (lsh 8 1)
==
