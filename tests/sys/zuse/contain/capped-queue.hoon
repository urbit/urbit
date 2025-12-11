/+  *test
::
=,  contain
::
|%
++  test-basic-capped-queue
  ::
  =|  q=(capped-queue @u)
  =.  max-size.q  3
  ::  specialize type
  ::
  =+  to-capped-queue=(to-capped-queue @u)
  ::  push a single element
  ::
  =^  maybe1  q  (~(put to-capped-queue q) 5)
  ::
  =/  results1
    %+  expect-eq
      !>  ~
      !>  maybe1
  =/  results2
    %+  expect-eq
      !>  1
      !>  size.q
  =/  results3
    %+  expect-eq
      !>  [~ 5]
      !>  ~(top to queue.q)
  ::  remove the single element
  ::
  =^  maybe2  q  ~(get to-capped-queue q)
  ::
  =/  results4
    %+  expect-eq
      !>  5
      !>  maybe2
  =/  results5
    %+  expect-eq
      !>  0
      !>  size.q
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
  ==
::
++  test-put-returns-evicted-value
  ::
  =|  q=(capped-queue @u)
  =.  max-size.q  2
  ::  specialize type
  ::
  =+  to-capped-queue=(to-capped-queue @u)
  ::  push enough values to evict one
  ::
  =^  maybe1  q  (~(put to-capped-queue q) 5)
  =/  results1
    %+  expect-eq
      !>  ~
      !>  maybe1
  =/  results2
    %+  expect-eq
      !>  1
      !>  size.q
  ::
  =^  maybe2  q  (~(put to-capped-queue q) 6)
  =/  results3
    %+  expect-eq
      !>  ~
      !>  maybe2
  =/  results4
    %+  expect-eq
      !>  2
      !>  size.q
  ::
  =^  maybe3  q  (~(put to-capped-queue q) 7)
  =/  results5
    %+  expect-eq
      !>  [~ 5]
      !>  maybe3
  =/  results6
    %+  expect-eq
      !>  2
      !>  size.q
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
    results6
  ==
::
++  test-resize-evicts-on-shrink
  ::
  =|  q=(capped-queue @u)
  =.  max-size.q  5
  ::  specialize type
  ::
  =+  to-capped-queue=(to-capped-queue @u)
  ::
  =^  maybe1  q  (~(put to-capped-queue q) 1)
  =^  maybe2  q  (~(put to-capped-queue q) 2)
  =^  maybe3  q  (~(put to-capped-queue q) 3)
  =^  maybe4  q  (~(put to-capped-queue q) 4)
  =^  maybe5  q  (~(put to-capped-queue q) 5)
  ::  resize the size to 3; this should pop two items
  ::
  =^  pops  q  (~(resize to-capped-queue q) 3)
  ::
  =/  results1
    %+  expect-eq
      !>  [1 2 ~]
      !>  pops
  =/  results2
    %+  expect-eq
      !>  3
      !>  size.q
  ::
  ;:  weld
    results1
    results2
  ==
::
++  test-put-zero
  ::
  =|  q=(capped-queue @u)
  =.  max-size.q  0
  ::  specialize type
  ::
  =+  to-capped-queue=(to-capped-queue @u)
  ::  push enough values to evict one
  ::
  ::
  =^  maybe1  q  (~(put to-capped-queue q) 5)
  =/  results1
    %+  expect-eq
      !>  [~ 5]
      !>  maybe1
  =/  results2
    %+  expect-eq
      !>  0
      !>  size.q
  ::
  ;:  weld
    results1
    results2
  ==
--
