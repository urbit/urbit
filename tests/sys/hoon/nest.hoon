/+  *test
|%
++  test-fitz
  ;:  weld
    %+  expect-eq
      !>  %.y
      !>  (fitz ~. ~.tas)
    ::
    %+  expect-eq
      !>  %.y
      !>  (fitz ~.ud ~.)
    ::
    %+  expect-eq
      !>  %.n
      !>  (fitz ~.p ~.q)
    ::
    %+  expect-eq
      !>  %.n
      !>  (fitz ~.ux ~.ud)
    ::
    %+  expect-eq
      !>  %.y
      !>  (fitz ~.tas ~.ta)
    ::
    %+  expect-eq
      !>  %.n
      !>  (fitz 'uvD' 'uvE')
    ::
    %+  expect-eq
      !>  %.y
      !>  (fitz 'uvE' 'uvD')
    ::
    %+  expect-eq
      !>  %.n
      !>  (fitz 'AD' 'CB')
    ::
    %+  expect-eq
      !>  %.n
      !>  (fitz 'AC' 'CB')
  ==
--
