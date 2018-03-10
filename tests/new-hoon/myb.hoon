/+  new-hoon, tester
=,  myb:new-hoon
|_  tester-type:tester
++  test-from-list-null
  (expect-eq (from-list ~) ~)
::
++  test-from-list-real
  (expect-eq (from-list [5 ~]) [~ 5])
::
++  test-to-list-null
  (expect-eq (to-list ~) ~)
::
++  test-to-list-real
  (expect-eq (to-list [~ 5]) [5 ~])
::
++  test-concat-null
  (expect-eq (concat ~) ~)
::
++  test-concat-real
  ::  wait, if i pull the cast out from below, the concat implementation
  ::  doesn't compile anymore?
  (expect-eq (concat `(list (maybe @ud))`[~ [~ 1] ~ [~ 2] ~]) [1 2 ~])
::
++  test-map
  %+  expect-eq
    %+  map:myb
      [1 2 3 2 ~]
    |=(a/@u ?:(=(2 a) [~ 2] ~))
  [2 2 ~]
--

