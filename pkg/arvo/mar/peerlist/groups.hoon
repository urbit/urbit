/-  *peerlist
::
|_  =groups
::
++  grab
  |%
  ++  noun  ^groups
  ::
  ++  mime
    |=  ^mime
    ?>  ?=([%text %txt ~] p)
    (txt (to-wain:format q.q))
  ::
  ++  txt
    |=  lines=wain
    %-  ~(gas by *^groups)
    %+  turn  lines
    |=  line=cord
    ^-  (pair tag group)
    %+  rash  line
    |^  ;~(plug ;~(sfix urs:ab col) gr)
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
  ++  mime
    :-  /text/txt
    (as-octs:mimes:html (of-wain:format txt))
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
    %-  zing
    :+  (scow %tas tag)  ":"
    %+  join  ","
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