::  tests for lib/arkhe.hoon
::
/+  arkhe, *test
::
|%
++  test-make-node
  =/  n  (make-node:arkhe ~zod 42)
  ;:  weld
    %+  expect-eq
      !>  ~zod
      !>  id.n
  ::
    %+  expect-eq
      !>  42
      !>  state.n
  ::
    %+  expect-eq
      !>  .1.0
      !>  coherence.n
  ==
::
++  test-verify-golden-winding
  ;:  weld
    %+  expect-eq
      !>  %.y
      !>  (verify-golden-winding:arkhe 5 3)    :: 1.666... approx phi
  ::
    %+  expect-eq
      !>  %.y
      !>  (verify-golden-winding:arkhe 3 5)    :: 0.6 approx inv-phi
  ::
    %+  expect-eq
      !>  %.n
      !>  (verify-golden-winding:arkhe 10 1)   :: 10 too far from phi
  ==
::
++  test-handover-local-constitutional
  =/  n  (make-node:arkhe ~zod 42)
  =.  winding.n  [5 3]                        :: ensure constitutional
  =/  handler
    |=  [int=intent:arkhe state=*]
    ^-  [result=* state=*]
    =/  s  (,@ud state)
    [+(s) +(s)]
  =.  n  (register-capability:arkhe n %inc handler)
  =/  int  [%inc ~ ~]
  =/  ret  (handover-local:arkhe n int ~2024.1.1)
  ?~  ret  (expect-eq !>(%.y) !>(%.n))  :: fail if ~
  =+  [res new-node]=u.ret
  ;:  weld
    %+  expect-eq
      !>  43
      !>  res
  ::
    %+  expect-eq
      !>  43
      !>  state.new-node
  ==
::
++  test-handover-local-unconstitutional
  =/  n  (make-node:arkhe ~zod 42)
  =.  winding.n  [10 1]                       :: unconstitutional
  =/  handler
    |=  [int=intent:arkhe state=*]
    ^-  [result=* state=*]
    =/  s  (,@ud state)
    [+(s) +(s)]
  =.  n  (register-capability:arkhe n %inc handler)
  =/  int  [%inc ~ ~]
  =/  ret  (handover-local:arkhe n int ~2024.1.1)
  (expect-eq !>(~) !>(ret))
--
