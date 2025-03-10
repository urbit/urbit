/+  *test
|%
++  test-add
  ;:  weld
    ::  1+1 equals 2
    %+  expect-eq
      !>  2
      !>  (add 1 1)
    ::  Zero augend returns second operand
    %+  expect-eq
      !>  5
      !>  (add 0 5)
  ==
++  test-dec
  ;:  weld
    ::  Underflow: decrementing zero fails
    %-  expect-fail
      |.  (dec 0)
    ::  Normal decrement: 1 decrements to 0
    %+  expect-eq
      !>  0
      !>  (dec 1)
  ==
++  test-div
  ;:  weld
    ::  Division by zero fails
    %-  expect-fail
      |.  (div 1 0)
    ::  Even division without remainder
    %+  expect-eq
      !>  3
      !>  (div 6 2)
    ::  Division where dividend isn’t a multiple of divisor (rounding/truncation)
    %+  expect-eq
      !>  3
      !>  (div 7 2)
  ==
++  test-dvr
  ;:  weld
    ::  Division by zero fails
    %-  expect-fail
      |.  (dvr 5 0)
    ::  Dividend less than divisor: should return [0 dividend]
    %+  expect-eq
      !>  [0 3]
      !>  (dvr 3 5)
    ::  Dividend equal to divisor: returns [1 0]
    %+  expect-eq
      !>  [1 0]
      !>  (dvr 5 5)
    ::  Division with nonzero remainder: 10 ÷ 3 -> [3, 1]
    %+  expect-eq
      !>  [3 1]
      !>  (dvr 10 3)
  ==
++  test-gte
  ;:  weld
    ::  Equal operands: 3 >= 3 is true
    %-  expect
      !>  (gte 3 3)
    ::  Greater than: 5 >= 3 is true
    %-  expect
      !>  (gte 5 3)
    ::  Less than: 3 >= 5 is false
    %-  expect
      !>  !(gte 3 5)
  ==
++  test-gth
  ;:  weld
    ::  Equal operands: 3 > 3 is false
    %-  expect
      !>  !(gth 3 3)
    ::  Greater than: 5 > 3 is true
    %-  expect
      !>  (gth 5 3)
    ::  Lesser operand: 3 > 5 is false
    %-  expect
      !>  !(gth 3 5)
  ==
++  test-lte
  ;:  weld
    ::  Equal operands: 3 <= 3 is true
    %-  expect
      !>  (lte 3 3)
    ::  Lesser operand: 3 <= 5 is true
    %-  expect
      !>  (lte 3 5)
    ::  Greater operand: 5 <= 3 is false
    %-  expect
      !>  !(lte 5 3)
  ==
++  test-lth
  ;:  weld
    ::  Equal operands yield false: 3 < 3 is false
    %-  expect
      !>  !(lth 3 3)
    ::  Zero compared with a positive number: 0 < 3 is true
    %-  expect
      !>  (lth 0 3)
    ::  Positive vs zero: 3 < 0 is false
    %-  expect
      !>  !(lth 3 0)
    ::  Standard cases: 3 < 5 is true, 5 < 3 is false
    %-  expect
      !>  (lth 3 5)
    %-  expect
      !>  !(lth 5 3)
  ==
++  test-max
  ;:  weld
    ::  First operand greater: max(5, 3) returns 5
    %+  expect-eq
      !>  5
      !>  (max 5 3)
    ::  Second operand greater: max(3, 5) returns 5
    %+  expect-eq
      !>  5
      !>  (max 3 5)
    ::  Equal operands: max(4, 4) returns 4
    %+  expect-eq
      !>  4
      !>  (max 4 4)
  ==
++  test-min
  ;:  weld
    ::  First operand lesser: min(3, 5) returns 3
    %+  expect-eq
      !>  3
      !>  (min 3 5)
    ::  Second operand lesser: min(5, 3) returns 3
    %+  expect-eq
      !>  3
      !>  (min 5 3)
    ::  Equal operands: min(4, 4) returns 4
    %+  expect-eq
      !>  4
      !>  (min 4 4)
  ==
++  test-mod
  ;:  weld
    ::  Zero divisor should fail
    %-  expect-fail
      |.  (mod 5 0)
    ::  Dividend less than divisor: mod(3, 5) equals 3
    %+  expect-eq
      !>  3
      !>  (mod 3 5)
    ::  Exact division: mod(6, 3) equals 0
    %+  expect-eq
      !>  0
      !>  (mod 6 3)
    ::  Nonzero remainder: mod(10, 3) equals 1
    %+  expect-eq
      !>  1
      !>  (mod 10 3)
  ==
++  test-mul
  ;:  weld
    ::  Zero multiplied by any number yields 0
    %+  expect-eq
      !>  0
      !>  (mul 0 5)
    ::  Any number multiplied by zero yields 0
    %+  expect-eq
      !>  0
      !>  (mul 5 0)
    ::  Multiplication by one should return the other operand
    %+  expect-eq
      !>  5
      !>  (mul 1 5)
    %+  expect-eq
      !>  5
      !>  (mul 5 1)
    ::  Regular multiplication: 3 * 4 equals 12
    %+  expect-eq
      !>  12
      !>  (mul 3 4)
  ==
--