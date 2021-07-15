/+  *test, *ast-generator, h=hoon-parser
|%
++  expect-match-hoon
  |=  =cord
  %+  expect-eq
    !>  (all -:(scan:h (trip cord) tall:h))
  !>  (ream cord)
::
++  brts-case  ^-  cord
  '|=  [a=@ud b=@ud]  (add a b)'

++  test-transform-brts
  (expect-match-hoon brts-case)

::
++  skin-case  ^-  cord
  '=/  xs=(list @)  [1 2 3 ~]  xs'
++  test-transform-skin
  (expect-match-hoon skin-case)
--
