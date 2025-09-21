::  |nuke: wipe agent state & subscriptions after confirmation
::
/+  *generators
:-  %ask
|=  $:  [now=@da eny=@uvJ bec=beak]
        [=term ~]
        [desk=_| hard=_|]
    ==
?:  hard  (produce %kiln-nuke term desk)
=/  m1
  'nuking agents will permanently delete all their state and subscriptions.'
=/  m2
  'if other agents depend on the one(s) you nuke, \
  /their behavior could be negatively impacted. \
  /if you do not understand the risks, you may \
  /want to contact the agent\'s developers.'
=/  m3
  %+  rap  3
  :~  'are you sure you want to continue and nuke '
    ::
      ?.  desk  (cat 3 '%' term)
      (cat 3 'all agents in ' term)
    ::
      '?'
  ==
::NOTE  yes, printing order is weird
%+  print  m3
%+  print  m2
%+  print  m1
%+  prompt   [%& %prompt "nuke? (y/N) "]
|=  in=tape
?.  |(=("y" in) =("Y" in) =("yes" in))
  no-product
(produce %kiln-nuke term desk)
