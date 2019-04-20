::  Wrap tests in stateful philters
::
/+  ph
=,  ph
|%
::
::  A philter is similar to a test in structure, but they don't
::  terminate and have a ++stay arm for saving their state.
::
::  They may be wrappped around a test with +wrap-philter.
::
++  philter
  |*  o=mold
  |%
  ++  output
    $~  [& ~ %wait ~]
    $:  thru=?
        events=(list ph-event)
        $=  next
        $%  [%wait ~]
            [%cont self=data]
        ==
    ==
  ++  data
    $_  ^?
    |%
    ++  stay  *o
    ++  run   |~(ph-input *output)
    --
  --
::
::  Run the inner test wrapped in the outer philter.  The philter may
::  respond to any event that the test didn't consume.  One use is to
::  mock outside services, like an Ethereum node or LetsEncrypt.
::
++  wrap-philter
  |*  [o=mold i=mold]
  |=  [outer=_*data:(philter o) inner=_*data:(ph i)]
  ^+  *data:(ph ,[o i])
  |=  input=ph-input
  =/  res-i=_*ph-output:(ph i)
    (inner input)
  ?.  thru.res-i
    :+  thru.res-i  events.res-i
    ?-  -.next.res-i
      %wait  [%wait ~]
      %cont  [%cont ..$(inner self.next.res-i)]
      %fail  [%fail ~]
      %done  [%done stay:outer value.next.res-i]
    ==
  =/  res-o=_*output:(philter o)
    (run:outer input)
  ^+  *ph-output:(ph ,[o i])
  :+  thru.res-o  (welp events.res-i events.res-o)
  ?-    -.next.res-i
      %wait
    ?-  -.next.res-o
      %wait  [%wait ~]
      %cont  [%cont ..$(outer self.next.res-o)]
    ==
  ::
      %cont
    =.  inner  self.next.res-i
    ?-  -.next.res-o
      %wait  [%cont ..$]
      %cont  [%cont ..$(outer self.next.res-o)]
    ==
  ::
      %fail  [%fail ~]
      %done
    ?-  -.next.res-o
      %wait  [%done stay:outer value.next.res-i]
      %cont  [%done stay:self.next.res-o value.next.res-i]
    ==
  ==
--
