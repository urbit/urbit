/+  *test
|%
++  test-lug-fl
  %+  expect-eq
  !>  `fn`(lug:fl %fl [-3 (add (add (bex 113) (bex 1)) (bex 0))] %.y)
  !>  `fn`[%f s=%.y e=-2 a=5.192.296.858.534.827.628.530.496.329.220.097]
++  test-lug-ce
  %+  expect-eq
  !>  `fn`(lug:fl %ce [-3 (add (add (bex 113) (bex 1)) (bex 0))] %.y)
  !>  `fn`[%f s=%.y e=-2 a=5.192.296.858.534.827.628.530.496.329.220.098]
++  test-lug-sm
  %+  expect-eq
  !>  `fn`(lug:fl %sm [-3 (add (add (bex 113) (bex 1)) (bex 0))] %.y)
  !>  `fn`[%f s=%.y e=-2 a=5.192.296.858.534.827.628.530.496.329.220.097] 
++  test-lug-lg
  %+  expect-eq
    !>  `fn`(lug:fl %lg [-3 (add (add (bex 113) (bex 1)) (bex 0))] %.y)
    !>  `fn`[%f s=%.y e=-2 a=5.192.296.858.534.827.628.530.496.329.220.098]
++  test-lug-ne
  %+  expect-eq
    !>  `fn`(lug:fl %ne [-3 (add (add (bex 113) (bex 1)) (bex 0))] %.y)
    !>  `fn`[%f s=%.y e=-2 a=5.192.296.858.534.827.628.530.496.329.220.098]
++  test-lug-na
  %+  expect-eq
    !>  `fn`(lug:fl %na [-3 (add (add (bex 113) (bex 1)) (bex 0))] %.y)
    !>  `fn`[%f s=%.y e=-2 a=5.192.296.858.534.827.628.530.496.329.220.098]
++  test-lug-nt
  %+  expect-eq
    !>  `fn`(lug:fl %nt [-3 (add (add (bex 113) (bex 1)) (bex 0))] %.y)
    !>  `fn`[%f s=%.y e=-2 a=5.192.296.858.534.827.628.530.496.329.220.097]
--