::  tests for scot
::
/+  *test
::
|%
++  test-scot-patp-galaxy
  ;:  weld
    %+  expect-eq
      !>  ~.~zod
      !>  (scot %p ~zod)
  ::
    %+  expect-eq
      !>  ~.~zod
      !>  (scot %p 0x0)
  ::
    %+  expect-eq
      !>  ~.~fes
      !>  (scot %p ~fes)
  ::
    %+  expect-eq
      !>  ~.~fes
      !>  (scot %p 0xff)
  ==
++  test-scot-patp-star
  ;:  weld
    %+  expect-eq
      !>  ~.~marzod
      !>  (scot %p ~marzod)
  ::
    %+  expect-eq
      !>  ~.~marzod
      !>  (scot %p 0x100)
  ::
    %+  expect-eq
      !>  ~.~fipfes
      !>  (scot %p ~fipfes)
  ::
    %+  expect-eq
      !>  ~.~fipfes
      !>  (scot %p 0xffff)
  ==
++  test-scot-patp-planet
  ;:  weld
    %+  expect-eq
      !>  ~.~dapnep-ronmyl
      !>  (scot %p ~dapnep-ronmyl)
  ::
    %+  expect-eq
      !>  ~.~dapnep-ronmyl
      !>  (scot %p 0x1.0000)
  ::
    %+  expect-eq
      !>  ~.~dostec-risfen
      !>  (scot %p ~dostec-risfen)
  ::
    %+  expect-eq
      !>  ~.~dostec-risfen
      !>  (scot %p 0xffff.ffff)
  ==
++  test-scot-patp-moon
  ;:  weld
    %+  expect-eq
      !>  ~.~doznec-dozzod-dozzod
      !>  (scot %p ~doznec-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  ~.~doznec-dozzod-dozzod
      !>  (scot %p 0x1.0000.0000)
  ::
    %+  expect-eq
      !>  ~.~fipfes-fipfes-dostec-risfen
      !>  (scot %p ~fipfes-fipfes-dostec-risfen)
  ::
    %+  expect-eq
      !>  ~.~fipfes-fipfes-dostec-risfen
      !>  (scot %p 0xffff.ffff.ffff.ffff)
  ==
++  test-scot-patp-comet
  ;:  weld
    %+  expect-eq
      !>  ~.~doznec--dozzod-dozzod-dozzod-dozzod
      !>  (scot %p ~doznec--dozzod-dozzod-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  ~.~doznec--dozzod-dozzod-dozzod-dozzod
      !>  (scot %p 0x1.0000.0000.0000.0000)
  ::
    %+  expect-eq
      !>  ~.~doznec-dozzod--dozzod-dozzod-dozzod-dozzod
      !>  (scot %p ~doznec-dozzod--dozzod-dozzod-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  ~.~doznec-dozzod--dozzod-dozzod-dozzod-dozzod
      !>  (scot %p 0x1.0000.0000.0000.0000.0000)
  ::
    %+  expect-eq
      !>  ~.~doznec-dozzod-dozzod--dozzod-dozzod-dozzod-dozzod
      !>  (scot %p ~doznec-dozzod-dozzod--dozzod-dozzod-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  ~.~doznec-dozzod-dozzod--dozzod-dozzod-dozzod-dozzod
      !>  (scot %p 0x1.0000.0000.0000.0000.0000.0000)
  ::
    %+  expect-eq
      !>  ~.~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes
      !>  (scot %p ~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes)
  ::
    %+  expect-eq
      !>  ~.~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes
      !>  (scot %p 0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff)
  ==
++  test-scot-patud
  ;:  weld
    %+  expect-eq
      !>  ~.0
      !>  (scot %ud 0)
    %+  expect-eq
      !>  ~.1
      !>  (scot %ud 1)
    %+  expect-eq
      !>  ~.10
      !>  (scot %ud 10)
    %+  expect-eq
      !>  ~.1.000
      !>  (scot %ud 1.000)
    %+  expect-eq
      !>  ~.65.536
      !>  (scot %ud 0x1.0000)
  ==
++  test-scot-patuv
  ;:  weld
    %+  expect-eq
      !>  ~.0v0
      !>  (scot %uv 0v0)
    %+  expect-eq
      !>  ~.0v1
      !>  (scot %uv 0v1)
    %+  expect-eq
      !>  ~.0vg
      !>  (scot %uv 16)
    %+  expect-eq
      !>  ~.0v10
      !>  (scot %uv 32)
    %+  expect-eq
      !>  ~.0vv8
      !>  (scot %uv 1.000)
    %+  expect-eq
      !>  ~.0v2000
      !>  (scot %uv 0x1.0000)
    %+  expect-eq
      !>  ~.0v4.00000.qc20d.3kqnj.7vvvv.vvvvv
      !>  (scot %uv 170.141.184.505.368.491.577.588.730.566.065.258.495)
    %+  expect-eq
      !>  ~.0v4.00000.qc20d.3kqnj.80000.00000
      !>  (scot %uv 170.141.184.505.368.491.577.588.730.566.065.258.496)
  ==
++  test-scot-patuw
  ;:  weld
    %+  expect-eq
      !>  ~.0w0
      !>  (scot %uw 0w0)
    %+  expect-eq
      !>  ~.0w1
      !>  (scot %uw 0w1)
    %+  expect-eq
      !>  ~.0ww
      !>  (scot %uw 32)
    %+  expect-eq
      !>  ~.0w10
      !>  (scot %uw 64)
    %+  expect-eq
      !>  '0wfE'
      !>  (scot %uw 1.000)
    %+  expect-eq
      !>  ~.0wg00
      !>  (scot %uw 0x1.0000)
    %+  expect-eq
      !>  '0w20.000dc.41EWq.Lc~~~.~~~~~'
      !>  (scot %uw 170.141.184.505.368.491.577.588.730.566.065.258.495)
    %+  expect-eq
      !>  '0w20.000dc.41EWq.Ld000.00000'
      !>  (scot %uw 170.141.184.505.368.491.577.588.730.566.065.258.496)
  ==
++  test-scot-patda
  ;:  weld
    %+  expect-eq
      !>  ~.~292277024401-.1.1
      !>  (scot %da 0)
    %+  expect-eq
      !>  ~.~1970.1.1
      !>  (scot %da 170.141.184.475.152.167.957.503.069.145.530.368.000)
    %+  expect-eq
      !>  ~.~1970.1.1..12.00.00
      !>  (scot %da 170.141.184.475.152.964.856.847.053.398.160.179.200)
    %+  expect-eq
      !>  ~.~2021.11.27..16.23.05
      !>  (scot %da 170.141.184.505.368.491.565.209.179.615.353.896.960)
    %+  expect-eq
      !>  ~.~2021.11.27..16.23.05..abcd
      !>  (scot %da 170.141.184.505.368.491.577.588.730.566.065.258.496)
  ==
--
