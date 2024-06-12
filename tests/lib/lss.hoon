/+  *test
|%
++  lss
  =,  blake:crypto
  |%
  ::
  ++  ctz
    |=  a=@
    ?<  =(0 a)  :: infinite!
    =|  i=@ud
    |-(?:(=(1 (cut 0 [i 1] a)) i $(i +(i))))
  ::
  ::  +builder: stateful core for computing proof data
  ::
  ++  builder
    =<
      |%
      ::
      ++  add-leaf
        |=  [=state leaf=octs]
        ^+  state
        ?>  (lte p.leaf 1.024)
        =/  =output:blake3  (chunk-output:blake3 leaves.state leaf)
        =|  height=@ud
        |-
        ?.  (has-tree state height)
          state(leaves +(leaves.state), trees (~(put by trees.state) height output))
        =/  l  (output-cv:blake3 (~(got by trees.state) height))
        =/  r  (output-cv:blake3 output)
        %=  $
          state   (add-pair state height l r)
          output  (parent-output:blake3 l r)
          height  +(height)
        ==
      ::
      ++  finalize
        |=  =state
        ^-  [root=@ux proof=(list @ux) pairs=(list (unit (pair @ux @ux)))]
        =^  root  state
          ?:  =(0 leaves.state)
            [(blake3 32 0^0) state]
          =/  height  (ctz leaves.state)
          =/  =output:blake3  (~(got by trees.state) height)
          =.  height  +(height)
          |-
          ?:  =(height (xeb leaves.state))
            [(output-cv:blake3 (set-flag:blake3 f-root:blake3 output)) state]
          ?.  (has-tree state height)
            $(height +(height))
          =/  l  (output-cv:blake3 (~(got by trees.state) height))
          =/  r  (output-cv:blake3 output)
          %=  $
            state   (add-pair state height l r)
            output  (parent-output:blake3 l r)
            height  +(height)
          ==
        =/  indices  ?:(=(0 leaves.state) ~ (gulf 0 (dec leaves.state)))
        [root (flop proof.state) (turn indices ~(get by pairs.state))]
      --
    |%
    +$  state
        $:  trees=(map @ud output:blake3)
            leaves=@ud
            proof=(list @ux)
            pairs=(map @ud (pair @ux @ux))
        ==
    ::
    ++  has-tree
      |=  [=state height=@ud]
      ^-  ?
      =(1 (cut 0 [height 1] leaves.state))
    ::
    ++  add-pair
      |=  [=state height=@ud l=@ux r=@ux]
      ^+  state
      ?:  (lth +(height) (met 0 leaves.state))
        =/  i  (sub leaves.state (add (mod leaves.state (bex +(height))) (bex height)))
        state(pairs (~(put by pairs.state) i [l r]))
      ?~  proof.state
        state(proof ~[r l])
      state(proof [r proof.state])
    --
  ::
  ::  +verifier: stateful core for sequentially verifying messages
  ::
  ++  verifier
    =<
      |%
      ::
      ++  init
        |=  [leaves=@ud proof=(list @ux)]
        ^-  state
        ?>  ?=([@ @ *] proof)  :: need at least two leaves to have a proof
        =/  pairs  (my [0 [i.proof i.t.proof]] ~)
        =/  proof  t.t.proof
        =/  height  1
        |-
        ?~  proof
          [leaves 0 pairs]
        %=  $
          pairs   (~(put by pairs) height [*@ux i.proof])
          proof   t.proof
          height  +(height)
        ==
      ::
      ++  verify-msg
        |=  [=state [leaf=octs pair=(unit [l=@ux r=@ux])]]
        ^+  state
        ?>  (lte p.leaf 1.024)
        ?>  =(?=(^ pair) (expect-pair state))
        ?>  (verify-leaf state leaf)
        ?~  pair
          state(counter +(counter.state))
        ?>  (verify-pair state u.pair)
        %=  state
          counter  +(counter.state)
          pairs    (~(put by pairs.state) (ctz counter.state) u.pair)
        ==
      --
    |%
    +$  state
        $:  leaves=@ud
            counter=@ud
            pairs=(map @ud [l=@ux r=@ux])
        ==
    ::
    ++  expect-pair
      |=  state
      &(!=(0 counter) (lth (add counter (bex +((ctz counter)))) leaves))
    ::
    ++  have-cv
      |=  [=state height=@ud sel=@ud cv=@ux]
      ^-  ?
      =/  p  (~(get by pairs.state) height)
      ?:  &(?=(^ p) =(?~(sel l.u.p r.u.p) cv))
        %.y
      (~(any by pairs.state) |=([l=@ux r=@ux] =(r cv)))
    ::
    ++  verify-leaf
      |=  [=state leaf=octs]
      ^-  ?
      =/  cv  (output-cv:blake3 (chunk-output:blake3 counter.state leaf))
      (have-cv state 0 (mod counter.state 2) cv)
    ::
    ++  verify-pair
      |=  [=state pair=[l=@ux r=@ux]]
      ^-  ?
      =/  height  +((ctz counter.state))
      =/  sel  (mix 1 (cut 0 [height 1] counter.state))
      =/  cv  (output-cv:blake3 (parent-output:blake3 pair))
      (have-cv state height sel cv)
    --
  --
--
=,  lss
|%
+$  bilt  [root=@ux proof=(list @ux) pairs=(list (unit (pair @ux @ux)))]
++  test-build-empty
  =/  msg=octs  0^0
  =|  =state:builder
  %+  expect-eq
  !>  `bilt`[(blake3:blake:crypto 32 msg) ~ ~]
  !>  (finalize:builder state)
::
++  test-build-foo
  =/  msg=octs  3^'foo'
  =|  =state:builder
  =.  state  (add-leaf:builder state msg)
  %+  expect-eq
  !>   `bilt`[(blake3:blake:crypto 32 msg) ~ ~[~]]
  !>  (finalize:builder state)

++  test-build-two
  =/  msg=octs  2.047^0
  =|  =state:builder
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.023^0)
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
++  test-build-three
  =/  msg=octs  3.071^0
  =|  =state:builder
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.023^0)
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
++  test-build-four
  =/  msg=octs  4.095^0
  =|  =state:builder
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.024^0)
  =.  state  (add-leaf:builder state 1.023^0)
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
