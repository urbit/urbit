/+  ethereum, *test
|%
::TODO  copy this:
::  https://github.com/ethereum/web3.js/blob/master/test/coder.encodeParam.js
::
++  test-static-args
  %+  expect-eq
    !>  %-  encode-args:abi:ethereum
        :~  [%string "dave"]
            [%bool &]
            [%array [%uint 1] [%uint 2] [%uint 3] ~]
        ==
    !>  %-  zing
        :~  "0000000000000000000000000000000000000000000000000000000000000060"
            "0000000000000000000000000000000000000000000000000000000000000001"
            "00000000000000000000000000000000000000000000000000000000000000a0"
            "0000000000000000000000000000000000000000000000000000000000000004"
            "6461766500000000000000000000000000000000000000000000000000000000"
            "0000000000000000000000000000000000000000000000000000000000000003"
            "0000000000000000000000000000000000000000000000000000000000000001"
            "0000000000000000000000000000000000000000000000000000000000000002"
            "0000000000000000000000000000000000000000000000000000000000000003"
        ==
::
++  test-dynamic-args
  %+  expect-eq
    !>  %-  encode-args:abi:ethereum
        :~  [%uint `@ud`0x123]
            [%array [%uint `@ud`0x456] [%uint `@ud`0x789] ~]
            [%bytes-n (as-octt:mimes:encoding (flop "1234567890"))]
            [%bytes (as-octt:mimes:encoding (flop "Hello, world!"))]
        ==
    !>  %-  zing
        :~  "0000000000000000000000000000000000000000000000000000000000000123"
            "0000000000000000000000000000000000000000000000000000000000000080"
            "3132333435363738393000000000000000000000000000000000000000000000"
            "00000000000000000000000000000000000000000000000000000000000000e0"
            "0000000000000000000000000000000000000000000000000000000000000002"
            "0000000000000000000000000000000000000000000000000000000000000456"
            "0000000000000000000000000000000000000000000000000000000000000789"
            "000000000000000000000000000000000000000000000000000000000000000d"
            "48656c6c6f2c20776f726c642100000000000000000000000000000000000000"
        ==
--
