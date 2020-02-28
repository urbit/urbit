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
    x  %.y
    y  (zer y)
  ==

::  Natural Less Than
=/  lt
  ~/  2  lt
  |=  (a b)
  ?-  (sub a b)
    x  %.y
    y  %.n
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

::  Unsigned modulus
=/  mod
  ~/  2  mod
  |=  (a b)
  (fub a (mul b (div a b)))

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

::  Rounds up to the nearest :multiple
::
=/  round-up
  ~/  2  dceil
  |=  (to-round multiple)
  ::
  ?:  (zer multiple)
    to-round
  ::
  =/  remainder  (mod to-round multiple)
  ?:  (zer remainder)
    to-round
  ::
  (fub (add to-round multiple) remainder)

::  Concatenate natural numbers, aligned in blocks of :a bits
=/  cat
  ~/  2  cat
  |=  (a b c)
  (add (lsh (round-up (met b) a) c) b)

::  Returns the last b bits of c as a new natural number
=/  end
  ~/  2  end
  |=  (b c)
  (mod c (bex b))


::  Assemble a list of natural numbers, aligned to the nearest :a bit blocks
=/  rap
  ~/  2  rap
  ..  $
  |=  (a b)
  ?-  b
    l  (cat a (car l) ($ a (cdr l)))
    r  0
  ==

:: Tape to cord
=/  crip
  ~/  1  crip
  (rap 8)

:: Directly stolen from turn.moon for testing
=/  cons
  |=  (head tail)
  (lef [head tail])
=/  null
  (rit uni)

:: > `@u`(crip ['a' 'a' 'a' ~])
:: 6.381.921
::
::
::(crip (cons 97 (cons 97 (cons 97 null))))


:: ---------------------------------------------------------------------------

::  ?&
=/  bool-and
  ~/  2  bool-and
  |=  (a b)
  (a (b %.y %.n) %.n)

=/  bool-xor
  ~/  2  bool-xor
  |=  (a b)
  (a (b %.n %.y) b)

::  binary xor on arbitrary natural numbers [WRITE JET]
=/  mix
  ~/  2  mix
  |=  (a b)
  =/  loop
    ..  $
    |=  (a b c d)
    ?:  (bool-and (zer a) (zer b))
      d
    ($ (rsh 1 a) (rsh 1 b) (inc c) (add d (lsh c ((eql (end 1 a) (end 1 b)) 0 1))))
  (loop a b 0 0)


:: ---------------------------------------------------------------------------

:: TODO: sum:si, dif:si, fra:si

::  New signed integer value from a sign bit and a natural number
=/  new-si
  ~/  2  new-si
  |=  (sign value)
  %+  sign
    (mul 2 value)
  ?:  (zer value)
    0
  (inc (mul 2 (fec value)))

::  Sign test
=/  syn-si
  ~/  1  syn-si
  |=  a
  (zer (end 1 a))

::  Absolute value of a signed integer as a natural
=/  abs-si
  ~/  1  abs-si
  |=  a
  (add (end 1 a) (rsh 1 a))


::
::  Testing Examples
::

=/  pos  (new-si %.y)
=/  neg  (new-si %.n)

=/  si-pair
  |=  x
  [(syn-si x) (abs-si x)]

[(si-pair (pos 3)) (si-pair (neg 9))]


:: TODO: There's something wrong with these implementations, and I'm not really
:: sure what. This might implicate some of the other si functions I've written
:: above.
::
:: ::  Signed multiplication
:: =/  pro-si
::   ~/  2  pro-si
::   |=  (a b)
::   (new-si (bool-xor (syn-si a) (syn-si b)) (mul (abs-si a) (abs-si b)))
::
:: ::  Signed division
:: =/  fra-si
::   ~/  2  div-si
::   |=  (a b)
::   (new-si (bool-xor (syn-si a) (syn-si b)) (div (abs-si a) (abs-si b)))

:: ::  Exports
:: :*
::   :-  bex  (bex 128)
::   :-  lsh  (lsh 8 1)
:: ==
