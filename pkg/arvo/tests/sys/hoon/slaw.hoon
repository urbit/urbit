::  tests for scot and slaw
::
/+  *test
::
|%
++  test-slaw-patp-galaxy
  ;:  weld
    %+  expect-eq
      !>  [~ ~zod]
      !>  `(unit @p)`(slaw %p ~.~zod)
  ::
    %+  expect-eq
      !>  [~ ~fes]
      !>  `(unit @p)`(slaw %p ~.~fes)
  ==
++  test-slaw-patp-star
  ;:  weld
    %+  expect-eq
      !>  [~ ~marzod]
      !>  `(unit @p)`(slaw %p ~.~marzod)
  ::
    %+  expect-eq
      !>  [~ ~fipfes]
      !>  `(unit @p)`(slaw %p ~.~fipfes)
  ==
++  test-slaw-patp-planet
  ;:  weld
    %+  expect-eq
      !>  [~ ~dapnep-ronmyl]
      !>  `(unit @p)`(slaw %p ~.~dapnep-ronmyl)
  ::
    %+  expect-eq
      !>  [~ ~dostec-risfen]
      !>  `(unit @p)`(slaw %p ~.~dostec-risfen)
  ==
++  test-slaw-patp-moon
  ;:  weld
    %+  expect-eq
      !>  [~ ~doznec-dozzod-dozzod]
      !>  `(unit @p)`(slaw %p ~.~doznec-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  [~ ~fipfes-fipfes-dostec-risfen]
      !>  `(unit @p)`(slaw %p ~.~fipfes-fipfes-dostec-risfen)
  ==
++  test-slaw-patp-comet
  ;:  weld
    %+  expect-eq
      !>  [~ ~doznec--dozzod-dozzod-dozzod-dozzod]
      !>  `(unit @p)`(slaw %p ~.~doznec--dozzod-dozzod-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  [~ ~doznec-dozzod--dozzod-dozzod-dozzod-dozzod]
      !>  `(unit @p)`(slaw %p ~.~doznec-dozzod--dozzod-dozzod-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  [~ ~doznec-dozzod-dozzod--dozzod-dozzod-dozzod-dozzod]
      !>  `(unit @p)`(slaw %p ~.~doznec-dozzod-dozzod--dozzod-dozzod-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  [~ ~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes]
      !>  `(unit @p)`(slaw %p ~.~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes)
  ==
++  test-slaw-patud
  ;:  weld
    %+  expect-eq
      !>  [~ 0]
      !>  (slaw %ud ~.0)
    %+  expect-eq
      !>  [~ 1]
      !>  (slaw %ud ~.1)
    %+  expect-eq
      !>  [~ 10]
      !>  (slaw %ud ~.10)
    %+  expect-eq
      !>  [~ 1.000]
      !>  (slaw %ud ~.1.000)
    %+  expect-eq
      !>  [~ 65.536]
      !>  (slaw %ud ~.65.536)
  ==
++  test-slaw-patuv
  ;:  weld
    %+  expect-eq
      !>  [~ 0v0]
      !>  `(unit @uv)`(slaw %uv ~.0v0)
    %+  expect-eq
      !>  [~ 0v1]
      !>  `(unit @uv)`(slaw %uv ~.0v1)
    %+  expect-eq
      !>  [~ 0vg]
      !>  `(unit @uv)`(slaw %uv ~.0vg)
    %+  expect-eq
      !>  [~ 0v10]
      !>  `(unit @uv)`(slaw %uv ~.0v10)
    %+  expect-eq
      !>  [~ 0vv8]
      !>  `(unit @uv)`(slaw %uv ~.0vv8)
    %+  expect-eq
      !>  [~ 0v2000]
      !>  `(unit @uv)`(slaw %uv ~.0v2000)
    %+  expect-eq
      !>  [~ 0v4.00000.qc20d.3kqnj.7vvvv.vvvvv]
      !>  `(unit @uv)`(slaw %uv ~.0v4.00000.qc20d.3kqnj.7vvvv.vvvvv)
    %+  expect-eq
      !>  [~ 0v4.00000.qc20d.3kqnj.80000.00000]
      !>  `(unit @uv)`(slaw %uv ~.0v4.00000.qc20d.3kqnj.80000.00000)
  ==
++  test-slaw-patuw
  ;:  weld
    %+  expect-eq
      !>  [~ 0w0]
      !>  `(unit @uw)`(slaw %uw ~.0w0)
    %+  expect-eq
      !>  [~ 0w1]
      !>  `(unit @uw)`(slaw %uw ~.0w1)
    %+  expect-eq
      !>  [~ 0ww]
      !>  `(unit @uw)`(slaw %uw ~.0ww)
    %+  expect-eq
      !>  [~ 0w10]
      !>  `(unit @uw)`(slaw %uw ~.0w10)
    %+  expect-eq
      !>  [~ 0wfE]
      !>  `(unit @uw)`(slaw %uw '0wfE')
    %+  expect-eq
      !>  [~ 0wg00]
      !>  `(unit @uw)`(slaw %uw ~.0wg00)
    %+  expect-eq
      !>  [~ 0w20.000dc.41EWq.Lc~~~.~~~~~]
      !>  `(unit @uw)`(slaw %uw '0w20.000dc.41EWq.Lc~~~.~~~~~')
    %+  expect-eq
      !>  [~ 0w20.000dc.41EWq.Ld000.00000]
      !>  `(unit @uw)`(slaw %uw '0w20.000dc.41EWq.Ld000.00000')
  ==
++  test-slaw-patda
  ;:  weld
    %+  expect-eq
      !>  [~ ~292277024401-.1.1]
      !>  `(unit @da)`(slaw %da ~.~292277024401-.1.1)
    %+  expect-eq
      !>  [~ ~1970.1.1]
      !>  `(unit @da)`(slaw %da ~.~1970.1.1)
    %+  expect-eq
      !>  [~ ~1970.1.1..12.00.00]
      !>  `(unit @da)`(slaw %da ~.~1970.1.1..12.00.00)
    %+  expect-eq
      !>  [~ ~2021.11.27..16.23.05]
      !>  `(unit @da)`(slaw %da ~.~2021.11.27..16.23.05)
    %+  expect-eq
      !>  [~ ~2021.11.27..16.23.05..abcd]
      !>  `(unit @da)`(slaw %da ~.~2021.11.27..16.23.05..abcd)
  ==
--
