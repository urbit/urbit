/-  neo
::  Layer 1
=>
|%
::  +nail: convert $over to $ever
++  nail
  |=  [=over:neo =plot:neo =case:neo]
  ^-  ever:neo
  :*  [case rift.over]
      [p.why p.why-mut]:over
      [p.zed p.zed-mut]:over
      life=0 
      ship-life=0
      rift=0
      *time
  ==
::  $bump: Apply child or descendant change to $land
::
++  bump
  |=  [=loam:dirt:neo =turf:neo kind=?(%y %z) rift=?]
  ^-  turf:neo
  ?~  pie=(ram:on:land:neo land.turf)
    turf
  =/  [=case:neo =over:neo]  u.pie
  =.  land.turf
    %^  put:on:land:neo  land.turf  case
    ?:  ?=(%y kind)
      over(q.why +(q.why.over))
    over(q.zed +(q.zed.over))
  ?.  rift  turf
  (jump loam turf kind)
::  $jolt: Apply self change to $land
::
++  jolt
  |=  [=turf:neo rift=?]
  ^+  turf
  =/  [=case:neo =over:neo]
    ?^  pie=(ram:on:land:neo land.turf)
      u.pie
    [0 *over:neo]
  =.  land.turf
    %^  put:on:land:neo  land.turf   +(case)
    ^-  over:neo
    :*  [. .]:.+(q.why.over)
        [. .]:.+(q.why-mut.over)
        [. .]:.+(q.zed.over)
        [. .]:.+(q.zed-mut.over)
        ?:(rift +(rift.over) rift.over)
    ==
  turf
::  $jerk: resolve $once to $ever with $land
::
++  jerk
  =|  res=(unit ever:neo)
  |=  [=turf:neo =once:neo]
  ^+  res
  =<  -
  ^-  [(unit ever:neo) land:neo]
  %^  (dip:on:land:neo _res)  land.turf  res
  |=  [state=(unit ever:neo) [=case:neo =over:neo]]
  ^-  [(unit over:neo) ? _res]
  :-  `over
  ^-  [? (unit ever:neo)]
  ?:  ?=(%x -.once)
    ?.  =(case p.once)
      [| ~]
    :-  %&
    `(nail over plot.turf case)
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
  [%& `(nail over plot.turf case)]
::  +jump: react to child/descendant shape change
++  jump
  |=  [=loam:dirt:neo =turf:neo kind=?(%y %z)]
  ^+  turf
  =/  =plan:neo
    ?-  kind
      %y  by-kids.plot.turf
      %z  by-desc.plot.turf
    ==
  =/  case=@ud
    ?~  res=(ram:on:plan:neo plan)
      0
    key.u.res
  =/  piths=(set pith:neo)
    =-  ~(key by -)
    ?-  kind
      %y   (~(kid of:neo loam) ~)
      %z   =.(fil.loam ~ ~(tar of:neo loam))
    ==
  =?  by-kids.plot.turf  ?=(%y kind)
    (put:on:plan:neo plan +(case) piths)
  =?  by-desc.plot.turf  ?=(%z kind)
    (put:on:plan:neo plan +(case) piths)
  turf
::  +plow: operate on $soil
++  plow
  |_  =loam:dirt:neo
  ++  case
    ^-  case:neo
    ?~  fil.loam  0
    ?~  pie=(ram:on:soil:neo u.fil.loam)
      0
    key.u.pie
  ++  rift
    |=  grow=?
    ^-  ?
    ?~  fil.loam
      grow
    ?~  old=(ram:on:soil:neo u.fil.loam)
      grow
    ?:  grow
      =(~ q.val.u.old)
    !=(~ q.val.u.old)
  ::
  ++  grow
    |=  [=pail:neo =oath:neo]
    ^-  (quip [case:neo ?] loam:dirt:neo)
    =/  =poem:neo  [[+(case) oath] `pail]
    (make poem)
  ++  make
    |=  =poem:neo
    ^-  (quip [case:neo ?] loam:dirt:neo)
    =?  fil.loam  ?=(~ fil.loam)
      `*soil:neo
    ?>  ?=(^ fil.loam)
    =/  new=case:neo  +(case)
    ?>  =(new p.p.poem)
    :-  [new (rift !=(q.poem ~))]^~
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
    =^  gifts=(list [case:neo ?])  lom
      ?-  -.q.card
        %grow  (~(grow plow lom) +.q.card)
        %cull  ~(cull plow lom)
      ==
    :_  (~(rep of:neo loam) p.card lom)
    (turn gifts |=([@ ?] `gift:dirt:neo`[p.card +<]))
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
  |_  [=loam:dirt:neo =farm:neo]
  ++  self
    |=  [=pith:neo case=@ud rift=?]
    ^+  farm
    =/  me  (~(gut of:neo farm) pith *turf:neo)
    =.  me  (jolt me rift)
    (~(put of:neo farm) pith me)
  ::
  ++  eternal
    |=  [=pith:neo case=@ud rif=?]
    ^+  farm
    =.  farm  (heir pith rif)
    =.  farm  (chain pith rif)
    (self pith case rif)
  ++  chain
    =|  here=pith:neo
    |=  [=pith:neo rift=?]
    ?~  pith
      farm
    =/  kid  (~(gut by kid.farm) i.pith `farm:neo`[~ ~])
    =.  here  (snoc here i.pith)
    =?  fil.farm   ?=(^ fil.farm)
      `(bump (~(dip of:neo loam) here) u.fil.farm %z rift)
    farm(kid (~(put by kid.farm) i.pith $(farm kid, pith t.pith)))
  ++  slip
    |=  [=pith:neo =ever:neo]
    ^-  (list pith:neo)
    !!

  ::
  ++  heir
    |=  [=pith:neo rift=?]
    ^-  farm:neo
    ?~  par=(~(parent of:neo farm) pith)
      farm
    =/  lan=turf:neo  (~(got of:neo farm) u.par)
    %+  ~(put of:neo farm)  u.par
    ^-  turf:neo
    (bump (~(dip of:neo loam) u.par) lan %y rift)
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
    $(farm (eternal [p q r]:i.gis), gis t.gis)
  ::
  ++  look
    |=  [=loam:dirt:neo =care:neo =case:neo =pith:neo]
    ^-  (unit (unit (axal saga:neo)))
    =+  pom=(~(scry plow loam) case pith)
    ?:  ?=($?(~ [~ ~]) pom)
      pom
    ?~  q.u.u.pom
      [~ ~]
    ?~  val=(~(get of:neo farm) pith)
      ~
    [~ ~]
::  ?~  ver=(ram:on:land:neo u.val)
::    ~
::  =/  =ever:neo  (nail val.u.ver case)
::  !! :: ``[q.u.u.pom ever q.p.u.u.pom]
  --
--

