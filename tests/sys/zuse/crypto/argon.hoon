::  tests for argon2
::
/+  *test
=,  argon2:crypto
::
|%
::
++  test-argon2d
  %+  expect-eq
    !>  0x2e2e.5e05.fe57.ac2c.f472.ecd0.45ef.687e.
          562a.980f.d503.39b3.89c8.70e1.962b.bc45
    !>  ^-  @ux
        %+  (argon2 32 %d 0x13 1 1.024 1 0^0 0^0)
          8^'drowssap'
        8^'tlasemos'
::
++  test-argon2i
  %+  expect-eq
    !>  0x9546.6cc4.f92f.8749.5461.7eec.0aa1.195d.
          2298.0abd.625e.5cac.4476.3ae3.a9cb.6ab7
    !>  ^-  @ux
        %+  (argon2 32 %i 0x13 1 1.024 1 0^0 0^0)
          8^'drowssap'
        8^'tlasemos'
--
