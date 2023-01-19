::  tests for number-to-words
::
/+  number-to-words, *test
::
|%
++  test-eng-us
  =+  eng-us:number-to-words
  ;:  weld
    %+  expect-eq
      !>  `(unit tape)``"zero"
      !>  (to-words 0)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one"
      !>  (to-words 1)
  ::
    %+  expect-eq
      !>  `(unit tape)``"twelve"
      !>  (to-words 12)
  ::
    %+  expect-eq
      !>  `(unit tape)``"twenty"
      !>  (to-words 20)
  ::
    %+  expect-eq
      !>  `(unit tape)``"ninety-nine"
      !>  (to-words 99)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one hundred"
      !>  (to-words 100)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one hundred and eleven"
      !>  (to-words 111)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one thousand"
      !>  (to-words 1.000)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one thousand, one hundred and eleven"
      !>  (to-words 1.111)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one million and one"
      !>  (to-words 1.000.001)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one trillion"
      !>  (to-words (pow 10 12))
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
      !>  (to-words 18.446.744.073.709.551.616)
  ::
    %+  expect-eq
      !>  `(unit tape)``"one vigintillion"
      !>  (to-words (pow 10 63))
  ==
--
