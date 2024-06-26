/+  *test
=,  lss
|%
+$  bilt  [root=@ux proof=(list @ux) pairs=(list (unit (pair @ux @ux)))]
++  test-builder-empty
  =/  msg=octs  0^0
  =|  =state:builder
  %+  weld
    %+  expect-eq
    !>  (finalize:builder state)
    !>  (build msg)
  %+  expect-eq
  !>  `bilt`[(blake3:blake:crypto 32 msg) ~ ~]
  !>  (finalize:builder state)
::
++  test-builder-foo
  =/  msg=octs  3^'foo'
  =|  =state:builder
  =.  state  (add-leaf:builder state msg)
  %+  weld
    %+  expect-eq
    !>  (finalize:builder state)
    !>  (build msg)
  %+  expect-eq
  !>   `bilt`[(blake3:blake:crypto 32 msg) ~ ~[~]]
  !>  (finalize:builder state)

++  test-builder-two
  =/  msg=octs  2.047^0
  =|  =state:builder
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.023^0)
  %+  weld
    %+  expect-eq
    !>  (finalize:builder state)
    !>  (build msg)
  %+  expect-eq
  !>   ^-  bilt
    =,  blake:crypto
    :+  (blake3 32 msg)
      :~
        (output-cv:blake3 (chunk-output:blake3 0 1.024^0))
        (output-cv:blake3 (chunk-output:blake3 1 1.023^0))
      ==
    ~[~ ~]
  !>  (finalize:builder state)
++  test-builder-three
  =/  msg=octs  3.071^0
  =|  =state:builder
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.023^0)
  %+  weld
    %+  expect-eq
    !>  (finalize:builder state)
    !>  (build msg)
  %+  expect-eq
  !>   ^-  bilt
    =,  blake:crypto
    :+  (blake3 32 msg)
      :~
        (output-cv:blake3 (chunk-output:blake3 0 1.024^0))
        (output-cv:blake3 (chunk-output:blake3 1 1.024^0))
        (output-cv:blake3 (chunk-output:blake3 2 1.023^0))
      ==
    ~[~ ~ ~]
  !>  (finalize:builder state)
++  test-builder-four
  =/  msg=octs  4.095^0
  =|  =state:builder
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.023^0)
  %+  weld
    %+  expect-eq
    !>  (finalize:builder state)
    !>  (build msg)
  %+  expect-eq
  !>   ^-  bilt
    =,  blake:crypto
    :+  (blake3 32 msg)
      :~
        (output-cv:blake3 (chunk-output:blake3 0 1.024^0))
        (output-cv:blake3 (chunk-output:blake3 1 1.024^0))
        %-  output-cv:blake3
        %+  parent-output:blake3
        (output-cv:blake3 (chunk-output:blake3 2 1.024^0))
        (output-cv:blake3 (chunk-output:blake3 3 1.023^0))
      ==
    :~
      ~
      :+  ~
        (output-cv:blake3 (chunk-output:blake3 2 1.024^0))
        (output-cv:blake3 (chunk-output:blake3 3 1.023^0))
      ~
      ~
    ==
  !>  (finalize:builder state)
::
++  test-verify-empty
  =/  msg=octs  0^0
  =|  bstate=state:builder
  =/  p  (finalize:builder bstate)
  %+  expect-eq
  !>  (blake3:blake:crypto 32 msg)
  !>  root.p
++  test-verify-foo
  =/  msg=octs  3^'foo'
  =|  bstate=state:builder
  =.  bstate  (add-leaf:builder bstate msg)
  =/  p  (finalize:builder bstate)
  %+  expect-eq
  !>  (blake3:blake:crypto 32 msg)
  !>  root.p
++  test-verify-two
  =/  msg=octs  2.047^0
  =|  bstate=state:builder
  =.  bstate  (add-leaf:builder bstate 1.024^0)
  =.  bstate  (add-leaf:builder bstate 1.023^0)
  =/  p  (finalize:builder bstate)
  =/  vstate=state:verifier  (init:verifier (lent pairs.p) proof.p)
  =.  vstate  (verify-msg:verifier vstate 1.024^0 &1:pairs.p)
  =.  vstate  (verify-msg:verifier vstate 1.023^0 &2:pairs.p)
  ~
++  test-verify-three
  =/  msg=octs  3.071^0
  =|  bstate=state:builder
  =.  bstate  (add-leaf:builder bstate 1.024^0)
  =.  bstate  (add-leaf:builder bstate 1.024^0)
  =.  bstate  (add-leaf:builder bstate 1.023^0)
  =/  p  (finalize:builder bstate)
  =/  vstate=state:verifier  (init:verifier (lent pairs.p) proof.p)
  =.  vstate  (verify-msg:verifier vstate 1.024^0 &1:pairs.p)
  =.  vstate  (verify-msg:verifier vstate 1.024^0 &2:pairs.p)
  =.  vstate  (verify-msg:verifier vstate 1.023^0 &3:pairs.p)
  ~
++  test-verify-four
  =/  msg=octs  4.095^0
  =|  bstate=state:builder
  =.  bstate  (add-leaf:builder bstate 1.024^0)
  =.  bstate  (add-leaf:builder bstate 1.024^0)
  =.  bstate  (add-leaf:builder bstate 1.024^0)
  =.  bstate  (add-leaf:builder bstate 1.023^0)
  =/  p  (finalize:builder bstate)
  =/  vstate=state:verifier  (init:verifier (lent pairs.p) proof.p)
  =.  vstate  (verify-msg:verifier vstate 1.024^0 &1:pairs.p)
  =.  vstate  (verify-msg:verifier vstate 1.024^0 &2:pairs.p)
  =.  vstate  (verify-msg:verifier vstate 1.024^0 &3:pairs.p)
  =.  vstate  (verify-msg:verifier vstate 1.023^0 &4:pairs.p)
  ~
--
