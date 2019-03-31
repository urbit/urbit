::  test a fix for a bug in +nap:to affecting three-item queues.
::
::    (see  https://github.com/urbit/arvo/issues/1100 for details)
::
::    the test creates several queues from known lists and recursively
::    unqueues the top, comparing it with the current element of the list.
::
/+  *test
::
=/  descending   ~[7 6 5 4 3 2 1]
=/  ascending    ~[1 2 3 4 5 6 7]
=/  unsorted     ~[1 6 3 5 7 2 4]
=/  duplicates   ~[1 1 7 4 6 9 4]
::
=>
|%
++  unqueue
  |=  [queue=(qeu @) test=(list @)]
  ^-  tang
  %-  zing
  |-  ^-  (list tang)
  ?~  test   ~
  ?~  queue  ~
  :_  $(queue ~(nap to queue), test t.test)
  %+  expect-eq
    !>  (need ~(top to queue))
    !>  i.test
--
::
|%
::
++  test-descending-sorted-list  ^-  tang
  ::
  =+  queue=(~(gas to `(qeu @)`~) descending)
  (unqueue [queue descending])
::
++  test-ascending-sorted-list  ^-  tang
  ::
  =+  queue=(~(gas to `(qeu @)`~) ascending)
  (unqueue [queue ascending])
::
++  test-unsorted-list  ^-  tang
  ::
  =+  queue=(~(gas to `(qeu @)`~) unsorted)
  (unqueue [queue unsorted])
::
++  test-duplicates-list  ^-  tang
  ::
  =+  queue=(~(gas to `(qeu @)`~) duplicates)
  (unqueue [queue duplicates])
--
