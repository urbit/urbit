::  Defines the ph monad.
::
::  A complete ph test has type data:(ph ,~).  This is a function that
::  accepts a new unix-effect and produces a list of ph-events to inject
::  back into the system.  It also produces one of four "next steps":
::
::    %wait: no change; on next unix-effect call this same function.
::    %cont: swap out this test for another one.  Mainly useful for
::           the implementation of +bind.
::    %fail: the test has failed.
::    %done: the test has finished successfully.
::
::  When producing %done, you may specify a value.  The ph app assumes
::  the value of each whole test will be ~.  During the test, though, it
::  may be useful to produce intermediate values.
::
::  We define two additional functions.  +return takes a value and
::  produces a test which immediately produces a %done with that value.
::
::  +bind takes a test and a function from the output type of that test
::  to another test.  This is useful to link tests together.  See
::  lib/ph/tests.hoon for examples of usage.
::
::  You may recognize monad terminology.  These functions satisfy the
::  monad laws:  If `f` and `g` are the sort of function that go in the
::  second argument to bind and `m` is a test, then:
::
::    (cork return (curr bind f)) = f
::    (bind m return) = m
::    ((bind m f) g) = (bind m (bind f g))
::
::  Maintaining these laws requires a particular interpretation of the
::  monad, which the ph app implements in +diff-aqua-effects.  Thus,
::  within the ph app the monad laws hold.
::
/-  aquarium
=,  aquarium
|%
+$  ph-input
  [now=@da who=ship uf=unix-effect]
::
++  ph
  |*  a=mold
  |%
  ++  ph-output  (ph-output-raw a)
  ++  ph-output-raw
    |*  a=mold
    $~  [& ~ %done *a]
    $:  thru=?
        events=(list ph-event)
        $=  next
        $%  [%wait ~]
            [%cont self=(data-raw a)]
            [%fail ~]
            [%done value=a]
        ==
    ==
  ::
  ++  data  (data-raw a)
  ++  data-raw
    |*  a=mold
    $-(ph-input (ph-output-raw a))
  ::
  ++  return
    |=  arg=a
    ^-  data
    |=  ph-input
    [& ~ %done arg]
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(data-raw b) fun=$-(b data)]
    ^-  data
    |=  input=ph-input
    =/  b-res=(ph-output-raw b)
      (m-b input)
    ^-  ph-output
    :+  thru.b-res  events.b-res
    ?-    -.next.b-res
      %wait  [%wait ~]
      %cont  [%cont ..$(m-b self.next.b-res)]
      %fail  [%fail ~]
      %done  [%cont (fun value.next.b-res)]
    ==
  --
--
