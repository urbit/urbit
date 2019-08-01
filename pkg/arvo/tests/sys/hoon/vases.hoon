/+  *test
|%
++  test-slam-wa  ^-  tang
  ::
  ;:  weld
    %+  expect-eq
      !>(7)
      -:(~(slam wa *worm) !>(add) !>([3 4]))
  ::
    %+  expect-eq
      !>(~[3 2 1])
      -:(~(slam wa *worm) !>(flop) !>(~[1 2 3]))
  ::
    =|  =worm
    =^  res1  worm  (~(slam wa worm) !>(flop) !>(`(list @)`~[1 2 3]))
    =/  worm1  worm
    =^  res2  worm  (~(slam wa worm) !>(flop) !>(`(list @)`~[4 5 6]))
    ::
    ;;  weld
      %+  expect-eq
        res1
        !>(`(list @)`~[3 2 1])
    ::
      %+  expect-eq
        res2
        !>(`(list @)`~[6 5 4])
    ::
      %+  expect-eq
        worm
        worm1
    ==
  ==
--
