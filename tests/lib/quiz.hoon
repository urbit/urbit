/+  quiz, *test

|%
++  size  1.000
++  test-dec
  =+  fate=!>(|=(@ud ^-(? =(+6 (dec +(+6))))))
  %-  expect  !>((check:quiz fate size))
++  test-tas
  :: Check we only generate valid tas when restricted.
  =+  fate=!>(|=(%foo ^-(? =(+6 %foo))))
  %-  expect  !>((check:quiz fate size))
++  test-face
  =+  fate=!>(|=(a=@ ^-(? =(a (dec +(a))))))
  %-  expect  !>((check:quiz fate size))
--
