/+  versioning, *test
|%
++  ver  ~(. versioning [*bowl:gall %update 2 1])
++  test-is-root
  ;:  weld
    %+  expect-eq  !>  %.y
    !>  (is-root:ver %update-0)
    ::
    %+  expect-eq  !>  %.y
    !>  (is-root:ver %update)

    ::
    %+  expect-eq  !>  %.n
    !>  (is-root:ver %not-update-0)
  ==
::
++  test-read-version
  ;:  weld
    %+  expect-eq  !>  0
    !>  (read-version:ver %update-0)
    ::
    %+  expect-eq  !>  0
    !>  (read-version:ver %update)
    ::
    %+  expect-eq  !>  1
    !>  (read-version:ver %update-1)
  ==
::
++  test-append-version
  ;:  weld
    %+  expect-eq  !>  %update-0
    !>  (append-version:ver 0)
    ::
    %+  expect-eq  !>  %update-1
    !>  (append-version:ver 1)
  ==
::
++  test-current-version
  %+  expect-eq  !>  %update-2
  !>  current-version:ver
::
++  test-supported
  ;:  weld
    (expect !>((supported:ver %update-2)))
    (expect !>((supported:ver %update-1)))
    (expect !>(!(supported:ver %update-0)))
  ==
--

