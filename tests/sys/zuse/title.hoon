::  tests for |title
::
/+  *test
=,  title
|%
++  test-cite
  ;:  weld
    %+  expect-eq
      !>  "~zod^"
      !>  (cite ~dister-dozzod-dozzod)
  ::
    %+  expect-eq
      !>  "~marzod^"
      !>  (cite ~dister-dozzod-marzod)
  ::
    %+  expect-eq
      !>  "~palfun^foslup"
      !>  (cite ~littel-palfun-foslup)
  ::
    %+  expect-eq
      !>  "~palfun^foslup"
      !>  (cite ~littel-bittel-palfun-foslup)
  ::
    %+  expect-eq
      !>  "~sampel_sampel"
      !>  (cite ~sampel--dozzod-dozzod-dozzod-sampel)
  ==
--
