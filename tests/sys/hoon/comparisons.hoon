/+  *test
|%
::  comparison gates don't crash if one argument
::  is a cell and another is a zero 
::
++  test-one-cell
  ;:  weld
    %+  expect-eq
      !>  `*`|
      !>  (slum lth 0^0 0)
    ::
    %+  expect-eq
      !>  `*`&
      !>  (slum lth 0 0^0)
    ::
    %+  expect-eq
      !>  `*`&
      !>  (slum gth 0^0 0)
    ::
    %+  expect-eq
      !>  `*`|
      !>  (slum gth 0 0^0)
    ::
    %+  expect-eq
      !>  `*`|
      !>  (slum lte 0^0 0)
    ::
    %+  expect-eq
      !>  `*`&
      !>  (slum lte 0 0^0)
    ::
    %+  expect-eq
      !>  `*`&
      !>  (slum gte 0^0 0)
    ::
    %+  expect-eq
      !>  `*`|
      !>  (slum gte 0 0^0)
    ::
    %+  expect-eq
      !>  `*`[0 0]
      !>  (slum max (slum max 0 0^0) 0)
    ::
    %+  expect-eq
      !>  `*`0
      !>  (slum min (slum min 0^0 0) 0^0)
  ==
::  comparison gates don't crash if the inputs are equal cells
::
++  test-eq-cells
  ;:  weld
    %+  expect-eq
      !>  `*`|
      !>  (slum lth 0^0 0^0)
    ::
    %+  expect-eq
      !>  `*`|
      !>  (slum gth 0^0 0^0)
    ::
    %+  expect-eq
      !>  `*`&
      !>  (slum lte 0^0 0^0)
    ::
    %+  expect-eq
      !>  `*`&
      !>  (slum gte 0^0 0^0)
    ::
    %+  expect-eq
      !>  [0 0]
      !>  (slum max 0^0 0^0)
    ::
    %+  expect-eq
      !>  [0 0]
      !>  (slum min 0^0 0^0)
  ==
--