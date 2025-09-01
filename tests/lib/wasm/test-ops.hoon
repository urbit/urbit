/+  *test
/+  *wasm-runner-engine
::
|%
++  test-int
  ;:  weld
    %+  expect-eq  ::  -2^(N-1) / -1 = {}
      !>  ~
      =,  op-def
      !>  %+  (bina:fetch:op-def [%div %i32 ~ %s])
            (en-si 32 (new:si | (bex 31)))
          (en-si 32 -1)
  ::
  ==
::
++  test-float
  ;:  weld
    %+  expect-eq
      !>  `@`.inf
      !>  (need ((unar:fetch:op-def [%abs %f32]) .-inf))
  ::
    %+  expect-eq
      !>  `@`.~inf
      !>  (need ((unar:fetch:op-def [%abs %f64]) .~-inf))
  ::
    %+  expect-eq
      !>  `@`.nan
      !>  (need ((unar:fetch:op-def [%abs %f32]) (add .nan (bex 31))))
  ::
    %+  expect-eq
      !>  `@`.128
      !>  (need ((unar:fetch:op-def [%abs %f32]) .-128))
  ::
    %+  expect-eq
      !>  `@`.~128
      !>  (need ((unar:fetch:op-def [%abs %f64]) .~-128))
  ::
    %+  expect-eq
      !>  `@`1.077.936.128
      !>  (need ((bina:fetch:op-def [%add %f32]) .1 .2))
  ::
    %+  expect-eq
      !>  `@`1.051.372.203
      !>  (need ((bina:fetch:op-def [%div %f32 ~]) .1 .3))
  ::
    %+  expect-eq
      !>  `@`1.150.191.188
      !>  (need ((unar:fetch:op-def [%sqrt %f32]) .1300051))
  ::
    %+  expect-eq
      !>  `@`4.698.334.488.248.188.928
      !>  (need ((unar:fetch:op-def [%promote ~]) .1300051))
  ::
    %+  expect-eq
      !>  `@`4.653.432.500.322.828.288
      !>  (need ((unar:fetch:op-def [%promote ~]) .1300.051))
  ::
    %+  expect-eq
      !>  `@`1.027.804.209
      !>  %-  need
          %+  (bina:fetch:op-def [%div %f32 ~])
            %-  need  %-  (bina:fetch:op-def [%div %f32 ~])
            [.1 .3]
          .7
  ::
    %+  expect-eq
      !>  `@`4.587.023.449.039.406.616
      !>  %-  need
          %+  (bina:fetch:op-def [%div %f64 ~])
            %-  need  %-  (bina:fetch:op-def [%div %f64 ~])
            [.~1 .~3]
          .~7
  ::
    %+  expect-eq
      !>  `@`1.077.936.128
      !>  %-  need
          %-  (unar:fetch:op-def [%trunc %f32 ~ ~ |])
          .3.9
  ::
    %+  expect-eq
      !>  `@`3
      !>  %-  need
          %-  (unar:fetch:op-def [%trunc %i32 `%f32 `%u |])
          .3.9
  ::
    %+  expect-eq
      !>  ~
      !>  %-  (unar:fetch:op-def [%trunc %i32 `%f32 `%u |])
          .4294967296
  ::
    %+  expect-eq
      !>  (sub (bex 32) 3)
      !>  %-  need
          %-  (unar:fetch:op-def [%trunc %i32 `%f32 `%s |])
          .-3.4
  ::
    %+  expect-eq
      !>  (sub (bex 32) 3)
      !>  %-  need
          %-  (unar:fetch:op-def [%trunc %i32 `%f32 `%s |])
          .-3.7
  ::
    %+  expect-eq
      !>  `@`4.294.967.296
      !>  %-  need
          %-  (unar:fetch:op-def [%trunc %i64 `%f32 `%u |])
          .4294967296
  ::
    %+  expect-eq
      !>  (dec (bex 32))
      !>  %-  need
          %-  (unar:fetch:op-def [%trunc %i32 `%f32 `%u &])
          .4294967296
  ::
    %+  expect-eq
      !>  (dec (bex 31))
      !>  %-  need
          %-  (unar:fetch:op-def [%trunc %i32 `%f32 `%s &])
          .4294967296
  ::
    %+  expect-eq
      !>  (bex 31)
      !>  %-  need
          %-  (unar:fetch:op-def [%trunc %i32 `%f32 `%s &])
          .-4294967296
  ::
    %+  expect-eq
      !>  1.082.130.432
      !>  %-  need
          %-  (unar:fetch:op-def [%ceil %f32])
          .3.7
  ::
    %+  expect-eq
      !>  3.225.419.776
      !>  %-  need
          %-  (unar:fetch:op-def [%ceil %f32])
          .-3.7
  ::
    %+  expect-eq
      !>  3.229.614.080
      !>  %-  need
          %-  (unar:fetch:op-def [%floor %f32])
          .-3.7
  ::
    %+  expect-eq
      !>  1.077.936.128
      !>  %-  need
          %-  (unar:fetch:op-def [%floor %f32])
          .3.7
  ::
    %+  expect-eq
      !>  4.290.772.992
      !>  %-  need
          %-  (unar:fetch:op-def [%neg %f32])
          .nan
  ==
::
++  test-vec
  =|  l=local-state:engine-sur
  =.  mem.store.l  `[0 1]
  ;:  weld
    %+  expect-eq
      !>  (rep 6 ~[7.241.902.928.051.372.565 241.972.029.883.607])
      !>
      =;  [v1=@ v2=@]
        =<  -:va.stack
        %-  (plain:simd:op-def [%avgr %i16 %u])
        l(va.stack [v2 v1 va.stack.l])
      :-
         %+  rep  4
         ~[334 999 30.000 27.999 42 42.424 59.999 0]
      %+  rep  4
      ~[732 1.234 23.423 23.456 2.435 23.566 52.677 0]
  ::
    %+  expect-eq
      !>  31
      !>
      =/  [v1=@ v2=@]  [1.000 1.000]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  1.068
      !>
      =/  [v1=@ v2=@]  [5.000 7.000]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  8.192
      !>
      =/  [v1=@ v2=@]  [16.383 16.384]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  0
      !>
      =/  [v1=@ v2=@]  [1 (dec (bex 16))]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  (dec (bex 16))
      !>
      =/  [v1=@ v2=@]  [65.535 32.767]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  0
      !>
      =/  [v1=@ v2=@]  [(dec (bex 16)) (dec (bex 16))]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  32.766
      !>
      =/  [v1=@ v2=@]  [32.767 32.767]
      =<  -:va.stack
      %-  (plain:simd:op-def [%q15mul-r-sat ~])
      l(va.stack [v2 v1 va.stack.l])
  ::
    %+  expect-eq
      !>  (rep 3 ~[233 0 161 0 137 0 252 0 6 0 92 0 247 0 95 0])
      !>  =<  -  %-  need  =<  mem.store
          %-  (fetch-gate:op-def [%vec %store [0 0]])
          %-  (fetch-gate:op-def [%vec %load [0 0] `[%8 %extend %u]])
          %-  (fetch-gate:op-def [%const %i32 0])
          %-  (fetch-gate:op-def [%store %i64 [0 0] ~])
          %-  (fetch-gate:op-def [%const %i64 6.915.096.937.927.123.433])
          %-  (fetch-gate:op-def [%const %i32 0])
          %-  (fetch-gate:op-def [%const %i32 0])
          l
  ::
    %+  expect-eq
      !>  (rep 3 ~[233 161 137 252 6 92 247 95 233 161 137 252 6 92 247 95])
      !>  =<  -  %-  need  =<  mem.store
          %-  (fetch-gate:op-def [%vec %store [0 0]])
          %-  (fetch-gate:op-def [%vec %load [0 0] `[%64 %splat]])
          %-  (fetch-gate:op-def [%const %i32 0])
          %-  (fetch-gate:op-def [%store %i64 [0 0] ~])
          %-  (fetch-gate:op-def [%const %i64 6.915.096.937.927.123.433])
          %-  (fetch-gate:op-def [%const %i32 0])
          %-  (fetch-gate:op-def [%const %i32 0])
          l
  ::
    %+  expect-eq
      !>  (rep 3 (reap 5 13))
      !>  =<  -  %-  need  =<  mem.store
        %-  (fetch-gate:op-def [%memory-fill %0])
        %-  (fetch-gate:op-def [%const %i32 5])
        %-  (fetch-gate:op-def [%const %i32 13])
        %-  (fetch-gate:op-def [%const %i32 0])
        l
  ::
  ==
--