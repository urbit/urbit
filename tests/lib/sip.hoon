::  Tests for the +sip lazy jam cursor.
::
::    Ground truth is +cue / +jam / +slot.  The unit tests jam hand-picked
::    nouns from the USTJ serialization tables; the property tests jam random
::    DAG-shaped nouns (deliberate sharing forces backrefs, including atom
::    backrefs); the hostile tests confirm malformed input is a deterministic
::    crash, never a wrong answer -- parity with +cue includes crash parity.
::
/+  sip, *test
::
=,  sip
|%
::  +grabs: assert +grab matches an explicit [axis expected] table
::
++  grabs
  |=  [n=* pairs=(list [ax=@ud val=@])]
  ^-  tang
  =/  dat  (jam n)
  %+  roll  pairs
  |=  [[ax=@ud val=@] acc=tang]
  %+  weld  acc
  %+  expect-eq
    !>  val
    !>  (grab [dat 0] ax)
::  +root-kind: assert +peek at the root equals :k
::
++  root-kind
  |=  [n=* k=kind]
  ^-  tang
  %+  expect-eq
    !>  k
    !>  (peek [(jam n) 0])
::  +root-span: assert +hop of the whole noun equals its total bit length
::
++  root-span
  |=  n=*
  ^-  tang
  =/  dat  (jam n)
  %+  expect-eq
    !>  (met 0 dat)
    !>  (hop [dat 0])
::  +cells: assert +dive classifies each axis as %cell (after resolving refs)
::
++  cells
  |=  [n=* axes=(list @ud)]
  ^-  tang
  =/  dat  (jam n)
  %+  roll  axes
  |=  [ax=@ud acc=tang]
  %+  weld  acc
  %+  expect-eq
    !>  %cell
    !>  (peek (dive [dat 0] ax))
::
::  ---  property-test machinery  ---
::
::  +big: a large atom whose backref is far shorter than its +mat encoding, so
::  a second occurrence is guaranteed to be cached as an atom backref by +jam.
::
++  big  `@`(dec (bex 256))
::  +step: advance a deterministic pseudo-random seed
::
++  step  |=(s=@ `@`(shas %sip-step s))
::  +pick: deterministic pseudo-random value in [0 m)
::
++  pick  |=([s=@ m=@] `@`(mod (shas %sip-pick s) m))
::  +gen: grow a random DAG-shaped noun; returns [noun next-seed]
::
::    Leaves are biased to small atoms (0-15) but 1/6 of the time emit +big,
::    forcing repeats (atom backrefs).  Structural repeats of small subtrees
::    arise naturally and are cached as cell/atom backrefs by +jam.
::
++  gen
  |=  [s=@ dep=@ud]
  ^-  [* @]
  =.  s  (step s)
  ?:  |(=(0 dep) =(0 (pick s 3)))
    =.  s  (step s)
    :_  s
    ?:(=(0 (pick s 6)) big (pick s 16))
  =^  l  s  (gen (step s) (dec dep))
  =^  r  s  (gen s (dec dep))
  [[l r] s]
::  +walk: enumerate every axis of a noun as [axis is-atom value]
::
++  walk
  |=  [ax=@ud n=*]
  ^-  (list [ax=@ud atom=? val=@])
  ?@  n  ~[[ax & n]]
  %-  zing
  :~  ~[[ax | 0]]
      $(ax (mul 2 ax), n -.n)
      $(ax +((mul 2 ax)), n +.n)
  ==
::  +check: full parity check of one noun against +jam / +slot ground truth
::
::    * +hop of the whole buffer equals its total bit length
::    * +grab equals +slot at every atom axis
::    * +dive classifies every cell axis as %cell
::
++  check
  |=  n=*
  ^-  tang
  =/  dat  (jam n)
  %+  weld
    %+  expect-eq  !>((met 0 dat))  !>((hop [dat 0]))
  %+  roll  (walk 1 n)
  |=  [[ax=@ud atom=? val=@] acc=tang]
  %+  weld  acc
  ?:  atom
    %+  expect-eq  !>(val)  !>((grab [dat 0] ax))
  %+  expect-eq  !>(%cell)  !>((peek (dive [dat 0] ax)))
::  +back-buf: a raw backref node at offset 0 pointing at :tgt (for hostile use)
::
++  back-buf
  |=  tgt=@
  ^-  @
  (mix 3 (lsh [0 2] q:(mat tgt)))
--
::
|%
::  ---  M1: hand-computed unit tests  ---
::
::  jam of the trivial atom is 2 (bits: 0 atom-tag, 1 mat-of-0); anchor the
::  whole suite to a known encoding from the USTJ table.
::
++  test-jam-anchor
  ;:  weld
    %+  expect-eq  !>(`@`2)   !>((jam 0))
    %+  expect-eq  !>(%atom)  !>((peek [2 0]))
    %+  expect-eq  !>(`@`0)   !>((grab [2 0] 1))
  ==
::  atom root: `0`
::
++  test-atom
  ;:  weld
    (root-kind 0 %atom)
    (root-span 0)
    (grabs 0 ~[[1 0]])
  ==
::  cell of two zeros: `[0 0]`  (tail 0 is an atom backref to the head 0)
::
++  test-cell-zeros
  ;:  weld
    (root-kind [0 0] %cell)
    (root-span [0 0])
    (cells [0 0] ~[1])
    (grabs [0 0] ~[[2 0] [3 0]])
  ==
::  `[1 0]`
::
++  test-one-zero
  ;:  weld
    (root-kind [1 0] %cell)
    (root-span [1 0])
    (grabs [1 0] ~[[2 1] [3 0]])
  ==
::  `[2 1 0]` == [2 [1 0]]
::
++  test-two-one-zero
  ;:  weld
    (root-kind [2 1 0] %cell)
    (root-span [2 1 0])
    (cells [2 1 0] ~[1 3])
    (grabs [2 1 0] ~[[2 2] [6 1] [7 0]])
  ==
::  `[[2 3] 1 0]` == [[2 3] [1 0]]
::
++  test-nested
  ;:  weld
    (root-kind [[2 3] 1 0] %cell)
    (root-span [[2 3] 1 0])
    (cells [[2 3] 1 0] ~[1 2 3])
    (grabs [[2 3] 1 0] ~[[4 2] [5 3] [6 1] [7 0]])
  ==
::  `[3 3 3]` == [3 [3 3]]  (no caching: mat(3) is not longer than a backref)
::
++  test-three-cubed
  ;:  weld
    (root-kind [3 3 3] %cell)
    (root-span [3 3 3])
    (cells [3 3 3] ~[3])
    (grabs [3 3 3] ~[[2 3] [6 3] [7 3]])
  ==
::  `[4 4 4]` == [4 [4 4]]  (caching: repeated 4s emit atom backrefs)
::
++  test-four-cubed
  ;:  weld
    (root-kind [4 4 4] %cell)
    (root-span [4 4 4])
    (cells [4 4 4] ~[3])
    (grabs [4 4 4] ~[[2 4] [6 4] [7 4]])
  ==
::  `[[0 0] 0 0]` == [[0 0] [0 0]]  (cell backref: the tail [0 0] refers to head)
::
++  test-cell-backref
  ;:  weld
    (root-kind [[0 0] 0 0] %cell)
    (root-span [[0 0] 0 0])
    (cells [[0 0] 0 0] ~[1 2 3])
    (grabs [[0 0] 0 0] ~[[4 0] [5 0] [6 0] [7 0]])
  ==
::  atom backref: a large atom repeated forces a +mat-cached atom backref
::
++  test-atom-backref
  ;:  weld
    (root-kind [big big] %cell)
    (root-span [big big])
    (grabs [big big] ~[[2 big] [3 big]])
    (grabs [big big big] ~[[2 big] [6 big] [7 big]])
  ==
::
::  ---  M2: property tests against +cue / +jam / +slot  ---
::
::  200 random DAG-shaped nouns; for each, +grab parity at every atom axis,
::  +dive %cell parity at every cell axis, +hop root span == total bit length.
::
++  test-property
  =/  count  200
  =/  s  `@`0xca11.ab1e.5eed
  =|  acc=tang
  |-  ^-  tang
  ?:  =(0 count)  acc
  =^  n  s  (gen s 6)
  $(count (dec count), acc (weld acc (check n)))
::  a second corpus with a different seed and shallower, wider trees
::
++  test-property-shallow
  =/  count  200
  =/  s  `@`0xf00d.face.d00d
  =|  acc=tang
  |-  ^-  tang
  ?:  =(0 count)  acc
  =^  n  s  (gen s 4)
  $(count (dec count), acc (weld acc (check n)))
::
::  ---  M2: hostile-input tests (all must crash)  ---
::
::  axis 0 is out of range
::
++  test-hostile-axis-zero
  %-  expect-fail  |.((grab [(jam [1 2]) 0] 0))
::  axis past the tree: diving into an atom
::
++  test-hostile-axis-oob
  %-  expect-fail  |.((grab [(jam [1 2]) 0] 4))
::  fetching an atom at an axis that lands on a cell
::
++  test-hostile-fetch-cell
  %-  expect-fail  |.((grab [(jam [[1 2] 3]) 0] 2))
::  a backref pointing forward (target >= cursor) must crash, never loop
::
++  test-hostile-forward-backref
  %-  expect-fail  |.((gaze [(back-buf 1) 0]))
::  a backref pointing at itself (target == cursor) must crash
::
++  test-hostile-self-backref
  %-  expect-fail  |.((gaze [(back-buf 0) 0]))
::  truncation: chopping any of the trailing structure crashes deterministically.
::
::    node 8 (value 8) is the last-emitted node of a right spine; its start
::    offset is the largest.  Removing it -- or any structure at or before it --
::    forces traversal to read a node at an offset >= the buffer length, which
::    overruns +rub's unary length and crashes.  (Clipping only the final atom's
::    payload bits does not crash: it mis-decodes to a smaller atom, exactly as
::    +cue would.  That is honest parity, not a defect, and is not asserted.)
::
++  test-hostile-truncation
  =/  n  [1 2 3 4 5 6 7 8]
  =/  d  (jam n)
  =/  o  off:(dive [d 0] 255)                       ::  start bit of node 8
  =/  t  1
  =|  acc=tang
  |-  ^-  tang
  ?:  (gth t o)  acc
  =/  d2  (end [0 t] d)
  =.  acc  (weld acc (expect-fail |.((hop [d2 0]))))
  $(t +(t))
--
