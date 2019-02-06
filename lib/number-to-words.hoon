::  |number-to-words: conversion of unsigned integers to a tape
::
::  returns a unit because not all numbers can always be represented
::
|%
++  numbers
  |%
  ++  ten                    10
  ++  one-hundred            100
  ++  one-thousand           (pow 10 3)
  ++  one-million            (pow 10 6)
  ++  one-billion            (pow 10 9)
  ++  one-trillion           (pow 10 12)
  ++  one-quadrillion        (pow 10 15)
  ++  one-quintillion        (pow 10 18)
  ++  one-sextillion         (pow 10 21)
  ++  one-septillion         (pow 10 24)
  ++  one-octillion          (pow 10 27)
  ++  one-nonillion          (pow 10 30)
  ++  one-decillion          (pow 10 33)
  ++  one-undecillion        (pow 10 36)
  ++  one-duodecillion       (pow 10 39)
  ++  one-tredecillion       (pow 10 42)
  ++  one-quattuordecillion  (pow 10 45)
  ++  one-quindecillion      (pow 10 48)
  ++  one-sexdecillion       (pow 10 51)
  ++  one-septendecillion    (pow 10 54)
  ++  one-octodecillion      (pow 10 57)
  ++  one-novemdecillion     (pow 10 60)
  ++  one-vigintillion       (pow 10 63)
  ++  max                    (pow 10 66)
  --
++  eng-us
  |%
  ++  to-words
    |=  num=@u
    ^-  (unit tape)
    =+  numbers
    ?:  (gte num max)
      ~
    :-  ~
    |-
    ^-  tape
    ::  0-19
    ?:  =(num 0)   "zero"
    ?:  =(num 1)   "one"
    ?:  =(num 2)   "two"
    ?:  =(num 3)   "three"
    ?:  =(num 4)   "four"
    ?:  =(num 5)   "five"
    ?:  =(num 6)   "six"
    ?:  =(num 7)   "seven"
    ?:  =(num 8)   "eight"
    ?:  =(num 9)   "nine"
    ?:  =(num 10)  "ten"
    ?:  =(num 11)  "eleven"
    ?:  =(num 12)  "twelve"
    ?:  =(num 13)  "thirteen"
    ?:  =(num 14)  "fourteen"
    ?:  =(num 15)  "fifteen"
    ?:  =(num 16)  "sixteen"
    ?:  =(num 17)  "seventeen"
    ?:  =(num 18)  "eighteen"
    ?:  =(num 19)  "nineteen"
    ::  20-99
    ::
    ::  tpl: tens place
    ::  rem: ones place
    ::  sfx: suffix
    ::
    =/  tpl  (div num ten)
    =/  rem  (mod num ten)
    =/  sfx
      ?:  |(=(rem 0) (gte tpl 10))
        ~
      ['-' $(num rem)]
    ?:  =(tpl 2)  (weld "twenty" sfx)
    ?:  =(tpl 3)  (weld "thirty" sfx)
    ?:  =(tpl 4)  (weld "forty" sfx)
    ?:  =(tpl 5)  (weld "fifty" sfx)
    ?:  =(tpl 6)  (weld "sixty" sfx)
    ?:  =(tpl 7)  (weld "seventy" sfx)
    ?:  =(tpl 8)  (weld "eighty" sfx)
    ?:  =(tpl 9)  (weld "ninety" sfx)
    ::  100-max
    ::
    ::  num-break: repeated pattern from 100 on
    ::
    =/  num-break
      ::
      ::  min: minimum to qualify for this break
      ::  str: english word for this break
      ::
      |=  [min=@u str=tape]
      =/  rem  (mod num min)
      ;:  weld
        ^$(num (div num min))
        [' ' str]
        ?:  =(rem 0)
          ~
        %+  weld
          ?:((lth rem one-hundred) " and " ", ")
        ^$(num rem)
      ==
    ::
    ?:  (lth num one-thousand)
      (num-break one-hundred "hundred")
    ?:  (lth num one-million)
      (num-break one-thousand "thousand")
    ?:  (lth num one-billion)
      (num-break one-million "million")
    ?:  (lth num one-trillion)
      (num-break one-billion "billion")
    ?:  (lth num one-quadrillion)
      (num-break one-trillion "trillion")
    ?:  (lth num one-quintillion)
      (num-break one-quadrillion "quadrillion")
    ?:  (lth num one-sextillion)
      (num-break one-quintillion "quintillion")
    ?:  (lth num one-septillion)
      (num-break one-sextillion "sextillion")
    ?:  (lth num one-octillion)
      (num-break one-septillion "septillion")
    ?:  (lth num one-nonillion)
      (num-break one-octillion "octillion")
    ?:  (lth num one-decillion)
      (num-break one-nonillion "nonillion")
    ?:  (lth num one-undecillion)
      (num-break one-decillion "decillion")
    ?:  (lth num one-duodecillion)
      (num-break one-undecillion "undecillion")
    ?:  (lth num one-tredecillion)
      (num-break one-duodecillion "duodecillion")
    ?:  (lth num one-quattuordecillion)
      (num-break one-tredecillion "tredecillion")
    ?:  (lth num one-quindecillion)
      (num-break one-quattuordecillion "quattuordecillion")
    ?:  (lth num one-sexdecillion)
      (num-break one-quindecillion "quindecillion")
    ?:  (lth num one-septendecillion)
      (num-break one-sexdecillion "sexdecillion")
    ?:  (lth num one-octodecillion)
      (num-break one-septendecillion "septendecillion")
    ?:  (lth num one-novemdecillion)
      (num-break one-octodecillion "octodecillion")
    ?:  (lth num one-vigintillion)
      (num-break one-novemdecillion "novemdecillion")
    (num-break one-vigintillion "vigintillion")
  --
--
