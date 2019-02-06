::  tests for number-to-words
::
/+  number-to-words, *test
::
|%
++  test-eng
  ;:  weld
    %+  expect-eq
      !>  `(unit tape)``"zero"
      !>  (eng:number-to-words 0)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one"
      !>  (eng:number-to-words 1)
  ::
    %+  expect-eq
      !>  `(unit tape)``"twelve"
      !>  (eng:number-to-words 12)
  ::
    %+  expect-eq
      !>  `(unit tape)``"twenty"
      !>  (eng:number-to-words 20)
  ::
    %+  expect-eq
      !>  `(unit tape)``"ninety-nine"
      !>  (eng:number-to-words 99)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one hundred"
      !>  (eng:number-to-words 100)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one hundred and eleven"
      !>  (eng:number-to-words 111)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one thousand"
      !>  (eng:number-to-words 1.000)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one thousand, one hundred and eleven"
      !>  (eng:number-to-words 1.111)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one million and one"
      !>  (eng:number-to-words 1.000.001)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one trillion"
      !>  (eng:number-to-words (pow 10 12))
  ::
    %+  expect-eq
      !>
        ^-  (unit tape)
        :-  ~
        ;:  weld
          "eighteen quintillion, four hundred and forty-six quadrillion, seven "
          "hundred and forty-four trillion, seventy-three billion, seven "
          "hundred and nine million, five hundred and fifty-one thousand, six "
          "hundred and sixteen"
        ==
        !>  (eng:number-to-words 18.446.744.073.709.551.616)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one vigintillion"
      !>  (eng:number-to-words (pow 10 63))
  ==
--
