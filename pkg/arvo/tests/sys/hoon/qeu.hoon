::  Tests for +to (queue logic)
::
/+  *test
::
=>  ::  Test Data
    ::
    |%
    +|  %test-suite
    ++  l-uno  ~[42]
    ++  l-dos  ~[6 9]
    ++  l-tre  ~[1 0 1]
    ++  l-tri  ~[1 2 3]
    ++  l-tra  ~[3 2 1]
    ++  l-asc  ~[1 2 3 4 5 6 7]
    ++  l-des  ~[7 6 5 4 3 2 1]
    ++  l-uns  ~[1 6 3 5 7 2 4]
    ++  l-dup  ~[1 1 7 4 6 9 4]
    ::  Each entry in the test suite is tagged to identify
    ::  the +to arm that fails with a specific queue.
    ::
    ++  q-nul  [%nul (~(gas to *(qeu)) ~)]
    ++  q-uno  [%uno (~(gas to *(qeu)) l-uno)]
    ++  q-dos  [%dos (~(gas to *(qeu)) l-dos)]
    ++  q-tre  [%tre (~(gas to *(qeu)) l-tre)]
    ++  q-tri  [%tri (~(gas to *(qeu)) l-tri)]
    ++  q-tra  [%tra (~(gas to *(qeu)) l-tra)]
    ++  q-asc  [%asc (~(gas to *(qeu)) l-asc)]
    ++  q-des  [%des (~(gas to *(qeu)) l-des)]
    ++  q-uns  [%uns (~(gas to *(qeu)) l-uns)]
    ++  q-dup  [%dup (~(gas to *(qeu)) l-dup)]
    +|  %grouped-data
    ++  queues  ^-  (list [term (qeu)])
      :~  q-uno  q-dos  q-tre
          q-tri  q-tra  q-asc
          q-des  q-uns  q-dup
      ==
    ++  lists  ^-  (list (list))
      :~  l-uno  l-dos  l-tre
          l-tri  l-tra  l-asc
          l-des  l-uns  l-dup
      ==
    --
::  Testing arms
::
|%
::  Test check correctness
::
++  test-queue-apt  ^-  tang
  ::  Manually constructed queues with predefined vertical ordering
  ::  for the following three elements (1, 2, 3) the priorities are:
  ::    > (mug (mug 1))
  ::    1.405.103.437
  ::    > (mug (mug 2))
  ::    1.200.431.393
  ::    > (mug (mug 3))
  ::    1.576.941.407
  ::
  ::  and the ordering 2 < 1 < 3
  ::  a correctly balanced tree stored as a min-heap
  ::  should have 2 as the root
  ::
  =/  balanced-a=(qeu @)    [2 [3 ~ ~] [1 ~ ~]]
  =/  balanced-b=(qeu @)    [2 [1 ~ ~] [3 ~ ~]]
  =/  unbalanced-a=(qeu @)  [3 [2 ~ ~] [1 ~ ~]]
  =/  unbalanced-b=(qeu @)  [1 [3 ~ ~] [2 ~ ~]]
  =/  unbalanced-c=(qeu @)  [3 [1 ~ ~] [2 ~ ~]]
  =/  unbalanced-d=(qeu @)  [3 ~ [2 ~ ~]]
  =/  unbalanced-e=(qeu @)  [3 [1 ~ ~] ~]
  ;:  weld
    %+  expect-eq
      !>  [%b-a %.y]
      !>  [%b-a ~(apt to balanced-a)]
    %+  expect-eq
      !>  [%b-b %.y]
      !>  [%b-b ~(apt to balanced-b)]
    %+  expect-eq
      !>  [%u-a %.n]
      !>  [%u-a ~(apt to unbalanced-a)]
    %+  expect-eq
      !>  [%u-b %.n]
      !>  [%u-b ~(apt to unbalanced-b)]
    %+  expect-eq
      !>  [%u-c %.n]
      !>  [%u-c ~(apt to unbalanced-c)]
    %+  expect-eq
      !>  [%u-d %.n]
      !>  [%u-d ~(apt to unbalanced-d)]
    %+  expect-eq
      !>  [%u-e %.n]
      !>  [%u-e ~(apt to unbalanced-e)]
  ==
::
::  Test balancing the queue
::
++  test-queue-bal  ^-  tang
  ::  Manually created queues explicitly unbalanced
  ::  p(2) < p(1) < p(3)
  ::  that places nodes with higher priority as the root
  ::
  =/  unbalanced-a=(qeu @)  [3 [2 ~ ~] [1 ~ ~]]
  =/  unbalanced-b=(qeu @)  [1 [3 ~ ~] [2 ~ ~]]
  =/  unbalanced-c=(qeu @)  [3 [1 ~ ~] [2 ~ ~]]
  ;:  weld
    %+  expect-eq
      !>  [%u-a %.y]
      !>  [%u-a ~(apt to ~(bal to unbalanced-a))]
    %+  expect-eq
      !>  [%u-b %.y]
      !>  [%u-b ~(apt to ~(bal to unbalanced-b))]
    %+  expect-eq
      !>  [%u-c %.y]
      !>  [%u-c ~(apt to ~(bal to unbalanced-c))]
  ==
::
::  Test max depth of queue
::
++  test-queue-dep  ^-  tang
  ::  Manually created queues with known depth
  ::
  =/  length-a=(qeu @)  [3 [2 ~ ~] [1 ~ ~]]
  =/  length-b=(qeu @)  [1 ~ ~]
  =/  length-c=(qeu @)  [5 ~ [4 ~ [2 [3 ~ ~] [1 ~ ~]]]]
  =/  length-d=(qeu @)  [5 [4 [2 [3 ~ ~] [1 ~ ~]] ~] ~]
  =/  length-e=(qeu @)  [5 [4 [2 ~ [1 ~ ~]] ~] [3 ~ ~]]
  =/  length-f=(qeu @)  [5 [4 [1 ~ ~] [9 ~ ~]] [3 [6 ~ ~] [7 ~ ~]]]
  ;:  weld
    %+  expect-eq
      !>  [%l-a 2]
      !>  [%l-a ~(dep to length-a)]
    %+  expect-eq
      !>  [%l-b 1]
      !>  [%l-b ~(dep to length-b)]
    %+  expect-eq
      !>  [%l-c 4]
      !>  [%l-c ~(dep to length-c)]
    %+  expect-eq
      !>  [%l-d 4]
      !>  [%l-d ~(dep to length-d)]
    %+  expect-eq
      !>  [%l-e 4]
      !>  [%l-e ~(dep to length-e)]
    %+  expect-eq
      !>  [%l-f 3]
      !>  [%l-f ~(dep to length-f)]
  ==
::
::  Test insert list into queue
::
++  test-queue-gas  ^-  tang
  =/  actual=(list [term ?])
    %+  turn  queues
      |=  [t=term s=(qeu)]
      ::  We use +apt to check the correctness
      ::  of the queues created with +gas
      ::
      [t ~(apt to s)]
  %-  zing
  ;:  weld
    ::  Checks with all tests in the suite
    ::
    %+  turn  actual
      |=  [t=term f=?]
      %+  expect-eq
        !>  t^&
        !>  t^f
    ::  Checks appending >1 elements
    ::
    :_  ~
    %+  expect-eq
      !>  %.y
      !>  ~(apt to (~(gas to +:q-dos) ~[9 10]))
    ::  Checks adding existing elements
    ::
    :_  ~
    %+  expect-eq
      !>  (~(gas to *(qeu)) (weld (gulf 1 7) (gulf 1 3)))
      !>  (~(gas to +:q-asc) (gulf 1 3))
  ==
::
::  Test getting head-rest pair
::
++  test-queue-get  ^-  tang
  =/  expected=(map term [@ (qeu)])
    %-  my
    :~  uno+[42 ~]
        dos+[6 (~(gas to *(qeu)) ~[9])]
        tre+[1 (~(gas to *(qeu)) ~[0 1])]
        tri+[1 (~(gas to *(qeu)) ~[2 3])]
        tra+[3 (~(gas to *(qeu)) ~[2 1])]
        asc+[1 (~(gas to *(qeu)) ~[2 3 4 5 6 7])]
        des+[7 (~(gas to *(qeu)) ~[6 5 4 3 2 1])]
        uns+[1 (~(gas to *(qeu)) ~[6 3 5 7 2 4])]
        dup+[1 (~(gas to *(qeu)) ~[1 7 4 6 9 4])]
    ==
  =/  pairs=(list [term [* (qeu)]])
    %+  turn  queues
      |=([t=term q=(qeu)] [t ~(get to q)])
  %-  zing
  ;:  weld
    ::  All tests in the suite
    ::
    %+  turn  pairs
      |=  [t=term p=[* (qeu)]]
      %+  expect-eq
        !>  t^(~(got by expected) t)
        !>  t^p
    ::  Expects crash on empty list
    ::
    :_  ~
    %-  expect-fail
      |.  ~(get to +:q-nul)
  ==
::
::  Test removing the root (more specialized balancing operation)
::
++  test-queue-nip  ^-  tang
  =/  actual=(list [term ?])
    %+  turn  queues
      ::  The queue representation follows vertical ordering
      ::  of the tree nodes as a min-heap
      ::  [i.e. priority(parent node) < priority(children)]
      ::  after nip we check that the resulting tree is balanced
      ::
      |=([t=term q=(qeu)] [t ~(apt to ~(nip to q))])
    %-  zing
    ;:  weld
      ::  All tests in the suite
      ::
      %+  turn  actual
        |=  [t=term f=?]
        (expect-eq !>(t^&) !>(t^f))
      ::  Expects crash on empty list
      ::
      :_  ~
      %-  expect-fail
        |.  ~(nap to +:q-nul)
    ==
::
::  Test removing the root
::
::    Current comment at L:1788 to %/sys/hoon/hoon.hoon is wrong
::    For a longer explanation read:
::    https://github.com/urbit/urbit/issues/1577#issuecomment-483845590
::
++  test-queue-nap  ^-  tang
  =/  actual=(list [term ?])
    %+  turn  queues
      ::  The queue representation follows vertical ordering
      ::  of the tree nodes as a min-heap
      ::  [i.e. priority(parent node) < priority(children)]
      ::  after nip we check that the resulting tree is balanced
      ::
      |=([t=term q=(qeu)] [t ~(apt to ~(nap to q))])
    %-  zing
    ;:  weld
      ::  All tests in the suite
      ::
      %+  turn  actual
        |=  [t=term f=?]
        (expect-eq !>(t^&) !>(t^f))
      ::  Expects crash on empty list
      ::
      :_  ~
      %-  expect-fail
        |.  ~(nap to +:q-nul)
    ==
::
::  Test inserting new tail
::
++  test-queue-put  ^-  tang
  =/  q-uno  (~(gas to *(qeu)) ~[42])
  =/  q-asc  (~(gas to *(qeu)) (gulf 1 7))
  =/  q-dos  (~(gas to *(qeu)) ~[42 43])
  ;:  weld
    ::  Checks with empty queue
    ::
    %+  expect-eq
      !>  q-uno
      !>  (~(put to *(qeu)) 42)
    ::  Checks putting existing element
    ::
    =/  q-dup  (~(gas to *(qeu)) ~[1 2 3 4 5 6 7 6])
    %+  expect-eq
      !>  q-dup
      !>  (~(put to q-asc) 6)
    ::  Checks putting a new element
    ::
    %+  expect-eq
      !>  (~(gas to *(qeu)) (gulf 1 8))
      !>  (~(put to q-asc) 8)
  ==
::
::  Test producing a queue a as a list from front to back
::
++  test-queue-tap  ^-  tang
  ::  We ran all queues in the suite against the corresponding lists
  ::
  =/  queues=(list (qeu))
    %+  turn  lists
      |=(iq=(list) (~(gas to *(qeu)) iq))
  =/  actual=(list (list))
    %+  turn  queues
      |=(iq=(qeu) ~(tap to iq))
  (expect-eq !>(lists) !>(actual))
::
::  Test producing the head of the queue
::
++  test-queue-top  ^-  tang
  ::  In order to know beforehand which element of the +qeu will become
  ::  the head, we need to look at the way new nodes are added to the
  ::  tree and how it's rebalanced.
  ::
  ::  New nodes are appended to the left-most branch of the tree, and
  ::  then the resulting tree will be balanced following the heap property.
  ::  The idea is that the balancing will be applied to all the subtrees,
  ::  starting from the node whose left branch is the new node that we
  ::  have appended. We will then perform certain tree rotations, depending
  ::  on the different priorities of the nodes considered.
  ::
  ::  If the new node has lower priority, a right-rotation is performed.
  ::  This will push the node (which was the first node) to the right
  ::  branch and balance that sub-branch, while promoting the node in the
  ::  left branch as the new node.
  ::
  ::  If the new node has higher priority, we check the right branch to
  ::  ensure that the heap-priority is conserved. In the case of the first
  ::  insert, the right branch is empty, therefore, no rotations are needed.
  ::
  ::  This means that the first node inserted in the +qeu will be located
  ::  either as the node of the +qeu, or in the right-most branch.
  ::
  ::  By inspecting +top:to we can see that it perfoms a traversal on the right
  ::  branch of the tree returning the last node whose right branch is null,
  ::  which is what we are looking for.
  ::
  =/  expected=(map term @)
    (my ~[uno+42 dos+6 tre+1 tri+1 tra+3 asc+1 des+7 uns+1 dup+1])
  =/  heads=(list [term (unit)])
    %+  turn  queues
      |=([t=term iq=(qeu)] [t ~(top to iq)])
  %-  zing
  ;:  weld
    ::  All the tests in the suite
    ::
    %+  turn  heads
      |=  [t=term u=(unit)]
      %+  expect-eq
        !>  t^(~(get by expected) t)
        !>  t^u
    :_  ~
    ::  Top of an empty queue is ~
    ::
    %+  expect-eq
      !>  ~
      !>  ~(top to +:q-nul)
  ==
--
