/-  neo
::  Layer 1
=>
|%
::  +nail: convert $over to $ever
++  nail
  |=  [=over:neo =case:neo]
  ^-  ever:neo
  [case p.why.over p.zed.over]
::  $bump: Apply child or descendant change to $land
::
++  bump
  |=  [=land:neo kind=?(%y %z)]
  ^+  land
  ?~  pie=(ram:on:land:neo land)
    land
  =/  [=case:neo =over:neo]  u.pie
  %^  put:on:land:neo  land  case
  ?:  ?=(%y kind)
    over(q.why +(q.why.over))
  over(q.zed +(q.zed.over))
::  $jolt: Apply self change to $land
::
++  jolt
  |=  [parent=land:neo =land:neo]
  ^+  land
  =/  [=case:neo =over:neo]
    ?^  pie=(ram:on:land:neo land)
      u.pie
    :-  0
    ?~  par=(ram:on:land:neo parent)
      [[0 0] [0 0]]
    [[. .]:key.u.par [. .]:q.zed.val.u.par]
  %^  put:on:land:neo  land   +(case)
  ^-  over:neo
  :*  [. .]:.+(q.why.over)
      [. .]:.+(q.zed.over)
  ==
::  $jerk: resolve $once to $ever with $land
::
++  jerk
  =|  res=(unit ever:neo)
  |=  [=land:neo =once:neo]
  ^+  res
  =<  -
  ^-  [(unit ever:neo) land:neo]
  %^  (dip:on:land:neo _res)  land  res
  |=  [state=(unit ever:neo) [=case:neo =over:neo]]
  ^-  [(unit over:neo) ? _res]
  :-  `over
  ^-  [? (unit ever:neo)]
  ?:  ?=(%x -.once)
    ?.  =(case p.once)
      [| ~]
    :-  %&
    `(nail over case)
  =/  hav=(pair @ud @ud)
    ?-  -.once
      %y  why.over
      %z  zed.over
    ==
  =/  wan=@ud  
    ?-  -.once
      %y   p.once
      %z   p.once
    ==
  ?.  &((lte p.hav wan) (gth q.hav wan))
    [| ~]
  [%& `[case p.why.over p.zed.over]]
::  +plow: operate on $soil
++  plow
  |_  =loam:dirt:neo
  ++  case
    ^-  case:neo
    ?~  fil.loam  0
    ?~  pie=(ram:on:soil:neo u.fil.loam)
      0
    key.u.pie
  ++  grow
    |=  [=pail:neo =oath:neo]
    ^-  (quip case:neo loam:dirt:neo)
    =/  =poem:neo  [[+(case) oath] `pail]
    (make poem)
  ++  make
    |=  =poem:neo
    ^-  (quip case:neo loam:dirt:neo)
    =?  fil.loam  ?=(~ fil.loam)
      `*soil:neo
    ?>  ?=(^ fil.loam)
    =/  new=case:neo  +(case)
    ?>  =(new p.p.poem)
    :-  ~[new]
    loam(fil `(put:on:soil:neo u.fil.loam new poem))
  ::
  ++  cull
    ?~  fil.loam
      [~ loam]
    (make [[+(case) *oath:neo] ~])
  ::
  ++  call
    |=  =card:dirt:neo
    ^-  (quip gift:dirt:neo _loam)
    =/  lom   (~(dip of:neo loam) p.card)
    =^  gifts=(list case:neo)  lom
      ?-  -.q.card
        %grow  (~(grow plow lom) +.q.card)
        %cull  ~(cull plow lom)
      ==
    :_  (~(rep of:neo loam) p.card lom)
    (turn gifts |=(@ `gift:dirt:neo`[p.card +< -.q.card]))
  ++  look
    |=  [=case:neo =pith:neo]
    ^-  (unit (unit poem:neo))
    [~ ~]
  ++  scry
    |=  [wan=case:neo =pith:neo]
    ^-  (unit (unit poem:neo))
    =/  lom  (~(dip of:neo loam) pith)
    ?~  fil.lom
      ~
    =/  latest=case:neo  case
    ?.  (lte wan latest)
      ~
    `(get:on:soil:neo u.fil.lom wan)
  --
--
::  layer 2
|%
++  till
  |_  =farm:neo
  ++  self
    |=  [=pith:neo case=@ud]
    =/  parent=land:neo
      ?~  par=(~(parent of:neo farm) pith)
        *land:neo
      (~(got of:neo farm) u.par)
    =/  me  (~(gut of:neo farm) pith *land:neo)
    (~(put of:neo farm) pith (jolt parent me))
  ::
  ++  eternal
    |=  [=pith:neo case=@ud]
    =.  farm  (heir pith)
    =.  farm  (chain pith)
    (self pith case)
  ++  chain
    |=  =pith:neo
    ?~  pith
      farm
    =/  kid  (~(gut by kid.farm) i.pith `farm:neo`[~ ~])
    =?  fil.farm   ?=(^ fil.farm)
      `(bump u.fil.farm %z)
    farm(kid (~(put by kid.farm) i.pith $(farm kid, pith t.pith)))
  ::
  ++  heir
    |=  =pith:neo
    ^-  farm:neo
    ?~  par=(~(parent of:neo farm) pith)
      farm
    =/  lan  (~(got of:neo farm) u.par)
    (~(put of:neo farm) u.par (bump lan %y))
  ::
  ++  take-cull
    |=  [=pith:neo =case:neo]
    ^-  farm:neo
    ?~  pith
      farm
    =/  kid  (~(gut by kid.farm) i.pith `farm:neo`[~ ~])
    farm(kid (~(put by kid.farm) i.pith $(farm kid, pith t.pith)))
  ::
  ++  take
    |=  gis=(list gift:dirt:neo)
    ^-  farm:neo
    ?~  gis
      farm
    $(farm (eternal [p q]:i.gis), gis t.gis)
  ::
  ++  scry
    |=  [=loam:dirt:neo =care:neo =case:neo =pith:neo]
    ^-  (unit (unit myth:neo))
    =+  pom=(~(scry plow loam) case pith)
    ?:  ?=($?(~ [~ ~]) pom)
      pom
    ?~  val=(~(get of:neo farm) pith)
      ~
    ?~  ver=(ram:on:land:neo u.val)
      ~
    =/  =ever:neo  (nail val.u.ver case)
    ``[q.u.u.pom ever q.p.u.u.pom]
  --
--

