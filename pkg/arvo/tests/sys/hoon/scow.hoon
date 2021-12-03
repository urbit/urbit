::  tests for scow
::
/+  *test
::
|%
++  test-scow-patp-galaxy
  ;:  weld
    %+  expect-eq
      !>  "~zod"
      !>  (scow %p ~zod)
  ::
    %+  expect-eq
      !>  "~zod"
      !>  (scow %p 0x0)
  ::
    %+  expect-eq
      !>  "~fes"
      !>  (scow %p ~fes)
  ::
    %+  expect-eq
      !>  "~fes"
      !>  (scow %p 0xff)
  ==
++  test-scow-patp-star
  ;:  weld
    %+  expect-eq
      !>  "~marzod"
      !>  (scow %p ~marzod)
  ::
    %+  expect-eq
      !>  "~marzod"
      !>  (scow %p 0x100)
  ::
    %+  expect-eq
      !>  "~fipfes"
      !>  (scow %p ~fipfes)
  ::
    %+  expect-eq
      !>  "~fipfes"
      !>  (scow %p 0xffff)
  ==
++  test-scow-patp-planet
  ;:  weld
    %+  expect-eq
      !>  "~dapnep-ronmyl"
      !>  (scow %p ~dapnep-ronmyl)
  ::
    %+  expect-eq
      !>  "~dapnep-ronmyl"
      !>  (scow %p 0x1.0000)
  ::
    %+  expect-eq
      !>  "~dostec-risfen"
      !>  (scow %p ~dostec-risfen)
  ::
    %+  expect-eq
      !>  "~dostec-risfen"
      !>  (scow %p 0xffff.ffff)
  ==
++  test-scow-patp-moon
  ;:  weld
    %+  expect-eq
      !>  "~doznec-dozzod-dozzod"
      !>  (scow %p ~doznec-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  "~doznec-dozzod-dozzod"
      !>  (scow %p 0x1.0000.0000)
  ::
    %+  expect-eq
      !>  "~fipfes-fipfes-dostec-risfen"
      !>  (scow %p ~fipfes-fipfes-dostec-risfen)
  ::
    %+  expect-eq
      !>  "~fipfes-fipfes-dostec-risfen"
      !>  (scow %p 0xffff.ffff.ffff.ffff)
  ==
++  test-scow-patp-comet
  ;:  weld
    %+  expect-eq
      !>  "~doznec--dozzod-dozzod-dozzod-dozzod"
      !>  (scow %p ~doznec--dozzod-dozzod-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  "~doznec--dozzod-dozzod-dozzod-dozzod"
      !>  (scow %p 0x1.0000.0000.0000.0000)
  ::
    %+  expect-eq
      !>  "~doznec-dozzod--dozzod-dozzod-dozzod-dozzod"
      !>  (scow %p ~doznec-dozzod--dozzod-dozzod-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  "~doznec-dozzod--dozzod-dozzod-dozzod-dozzod"
      !>  (scow %p 0x1.0000.0000.0000.0000.0000)
  ::
    %+  expect-eq
      !>  "~doznec-dozzod-dozzod--dozzod-dozzod-dozzod-dozzod"
      !>  (scow %p ~doznec-dozzod-dozzod--dozzod-dozzod-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  "~doznec-dozzod-dozzod--dozzod-dozzod-dozzod-dozzod"
      !>  (scow %p 0x1.0000.0000.0000.0000.0000.0000)
  ::
    %+  expect-eq
      !>  "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes"
      !>  (scow %p ~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes)
  ::
    %+  expect-eq
      !>  "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes"
      !>  (scow %p 0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff)
  ==
++  test-scow-patud
  ;:  weld
    %+  expect-eq
      !>  "0"
      !>  (scow %ud 0)
    %+  expect-eq
      !>  "1"
      !>  (scow %ud 1)
    %+  expect-eq
      !>  "10"
      !>  (scow %ud 10)
    %+  expect-eq
      !>  "1.000"
      !>  (scow %ud 1.000)
    %+  expect-eq
      !>  "65.536"
      !>  (scow %ud 0x1.0000)
  ==
++  test-scow-patuv
  ;:  weld
    %+  expect-eq
      !>  "0v0"
      !>  (scow %uv 0v0)
    %+  expect-eq
      !>  "0v1"
      !>  (scow %uv 0v1)
    %+  expect-eq
      !>  "0vg"
      !>  (scow %uv 16)
    %+  expect-eq
      !>  "0v10"
      !>  (scow %uv 32)
    %+  expect-eq
      !>  "0vv8"
      !>  (scow %uv 1.000)
    %+  expect-eq
      !>  "0v2000"
      !>  (scow %uv 0x1.0000)
    %+  expect-eq
      !>  "0v4.00000.qc20d.3kqnj.7vvvv.vvvvv"
      !>  (scow %uv 170.141.184.505.368.491.577.588.730.566.065.258.495)
    %+  expect-eq
      !>  "0v4.00000.qc20d.3kqnj.80000.00000"
      !>  (scow %uv 170.141.184.505.368.491.577.588.730.566.065.258.496)
  ==
++  test-scow-patuw
  ;:  weld
    %+  expect-eq
      !>  "0w0"
      !>  (scow %uw 0w0)
    %+  expect-eq
      !>  "0w1"
      !>  (scow %uw 0w1)
    %+  expect-eq
      !>  "0ww"
      !>  (scow %uw 32)
    %+  expect-eq
      !>  "0w10"
      !>  (scow %uw 64)
    %+  expect-eq
      !>  "0wfE"
      !>  (scow %uw 1.000)
    %+  expect-eq
      !>  "0wg00"
      !>  (scow %uw 0x1.0000)
    %+  expect-eq
      !>  "0w20.000dc.41EWq.Lc~~~.~~~~~"
      !>  (scow %uw 170.141.184.505.368.491.577.588.730.566.065.258.495)
    %+  expect-eq
      !>  "0w20.000dc.41EWq.Ld000.00000"
      !>  (scow %uw 170.141.184.505.368.491.577.588.730.566.065.258.496)
  ==
++  test-scow-patda
  ;:  weld
    %+  expect-eq
      !>  "~292277024401-.1.1"
      !>  (scow %da 0)
    %+  expect-eq
      !>  "~1970.1.1"
      !>  (scow %da 170.141.184.475.152.167.957.503.069.145.530.368.000)
    %+  expect-eq
      !>  "~1970.1.1..12.00.00"
      !>  (scow %da 170.141.184.475.152.964.856.847.053.398.160.179.200)
    %+  expect-eq
      !>  "~2021.11.27..16.23.05"
      !>  (scow %da 170.141.184.505.368.491.565.209.179.615.353.896.960)
    %+  expect-eq
      !>  "~2021.11.27..16.23.05..abcd"
      !>  (scow %da 170.141.184.505.368.491.577.588.730.566.065.258.496)
  ==
--
