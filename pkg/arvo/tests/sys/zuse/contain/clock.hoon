/+  *test
::
=,  contain
::
|%
++  test-basic-clock
  ::
  =|  c=(clock @u tape)
  ::  make max-size reasonable for testing
  ::
  =.  max-size.c  3
  ::  specialize type
  ::
  =+  by-clock=(by-clock @u tape)
  ::  ensure we get a single key we put in
  ::
  =.  c  (~(put by-clock c) 1 "one")
  =^  maybe1  c  (~(get by-clock c) 1)
  =/  results1
    %+  expect-eq
      !>  [~ "one"]
      !>  maybe1
  ::
  =/  results2
    %+  expect-eq
      !>  1
      !>  size.c
  ::  push that key out of the cache
  ::
  =.  c  (~(put by-clock c) 2 "two")
  =.  c  (~(put by-clock c) 3 "three")
  =.  c  (~(put by-clock c) 4 "four")
  ::
  =/  results3
    %+  expect-eq
      !>  3
      !>  size.c
  ::
  =^  maybe2  c  (~(get by-clock c) 1)
  =/  results4
    %+  expect-eq
      !>  ~
      !>  maybe2
  ::
  ;:  weld
    results1
    results2
    results3
    results4
  ==
::
++  test-clock-purge
  ::
  =|  c=(clock @u tape)
  ::  make max-size reasonable for testing
  ::
  =.  max-size.c  3
  ::  specialize type
  ::
  =+  by-clock=(by-clock @u tape)
  ::  fill the clock
  ::
  =.  c  (~(put by-clock c) 1 "one")
  =.  c  (~(put by-clock c) 2 "two")
  =.  c  (~(put by-clock c) 3 "three")
  ::  purge the entire clock
  ::
  =.  c  ~(purge by-clock c)
  ::
  ;:  weld
    %+  expect-eq
      !>  0
      !>  size.c
  ::
    %+  expect-eq
      !>  3
      !>  max-size.c
  ::
    %+  expect-eq
      !>  ~
      !>  lookup.c
  ::
    %+  expect-eq
      !>  ~
      !>  queue.c
  ==
::
++  test-clock-trim
  ::
  =|  c=(clock @u tape)
  ::  make max-size reasonable for testing
  ::
  =.  max-size.c  3
  ::  specialize type
  ::
  =+  by-clock=(by-clock @u tape)
  ::  fill the clock
  ::
  =.  c  (~(put by-clock c) 1 "one")
  =.  c  (~(put by-clock c) 2 "two")
  =.  c  (~(put by-clock c) 3 "three")
  ::  trim 2/3 of the clock
  ::
  =.  c  (~(trim by-clock c) 2)
  ::
  ;:  weld
    %+  expect-eq
      !>  1
      !>  size.c
  ::
    =^  results1  c  (~(get by-clock c) 3)
    %+  expect-eq
      !>  [~ "three"]
      !>  results1
  ::
    %+  expect-eq
      !>  1
      !>  ~(wyt by lookup.c)
  ==
::
++  test-clock-resized-to-zero
  ::
  =|  c=(clock @u tape)
  ::  make max-size reasonable for testing
  ::
  =.  max-size.c  3
  ::  specialize type
  ::
  =+  by-clock=(by-clock @u tape)
  ::  fill the clock
  ::
  =.  c  (~(put by-clock c) 1 "one")
  =.  c  (~(put by-clock c) 2 "two")
  =.  c  (~(put by-clock c) 3 "three")
  ::  resize the clock so it has zero elements
  ::
  =.  c  (~(resize by-clock c) 0)
  ::
  =/  results1
    %+  expect-eq
      !>  0
      !>  size.c
  ::
  =/  results2
    %+  expect-eq
      !>  ~
      !>  lookup.c
  ::
  =/  results3
    %+  expect-eq
      !>  ~
      !>  queue.c
  ::
  =/  results4
    %+  expect-eq
      !>  0
      !>  max-size.c
  ::  trying to get an element just returns ~
  ::
  =^  maybe1  c  (~(get by-clock c) 3)
  =/  results5
    %+  expect-eq
      !>  ~
      !>  maybe1
  ::  trying to put an element in doesn't mutate the clock
  ::
  =.  c  (~(put by-clock c) 4 "four")
  ::
  =/  results6
    %+  expect-eq
      !>  0
      !>  size.c
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
    results6
  ==
--
