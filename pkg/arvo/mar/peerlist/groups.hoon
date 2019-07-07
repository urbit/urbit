/-  *peerlist
::
|_  =groups
::
++  grab
  |%
  ++  noun  ^groups
  ::
  ++  txt
    |=  lines=wain
    %-  ~(gas by *^groups)
    %+  turn  lines
    |=  line=cord
    ^-  (pair tag group)
    %+  rash  line
    |^  ;~((glue com) ta gr)
    ++  ta  ;~(pfix ;~(plug sig dot) urs:ab)
    ++  p   ;~(pfix sig fed:ag)
    ++  gr  (cook ~(gas in *group) (more com p))
    --
  ::
  ++  json
    =,  dejs:format
    |=  =json
    ^+  groups
    ?>  ?=([%o *] json)
    %-  ~(run by p.json)
    %+  corl  ~(gas in *group)
    (ar (su ;~(pfix sig fed:ag)))
  --
::
++  grow
  |%
  ++  noun  groups
  ::
  ++  txt
    ^-  wain
    %+  turn
      %+  sort  ~(tap by groups)
      |=  [[a=tag *] [b=tag *]]
      (lth a b)
    |=  [=tag =group]
    ^-  cord
    %-  crip
    %+  fuse  ","
    :-  (scow %ta tag)
    %+  turn
      (sort ~(tap in group) lth)
    (cury scow %p)
  ::
  ++  json
    =,  enjs:format
    %-  pairs
    =-  ~(tap by (~(run by groups) -))
    |=  =group
    ^-  ^json
    :-  %a
    %+  turn  ~(tap in group)
    (cork (cury scow %p) tape)
  --
--