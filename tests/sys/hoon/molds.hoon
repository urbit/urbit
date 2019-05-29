/+  *test
|%
++  test-set
  ;:  weld
    %+  expect-eq
      !>  (some (sy 1 2 ~))
      !>  ((soft (set)) [2 [1 ~ ~] ~])
  ::
    %+  expect-eq
      !>  ~
      !>  ((soft (set)) [2 ~ [1 ~ ~]])
  ==
::
++  test-map
  ;:  weld
    %+  expect-eq
      !>  (some (my [1 2] [3 4] ~))
      !>  ((soft (map)) [[1 2] ~ [[3 4] ~ ~]])
  ::
    %+  expect-eq
      !>  ~
      !>  ((soft (map)) [[1 2] [[3 4] ~ ~] ~])
  ==
--
