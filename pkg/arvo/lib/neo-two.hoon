/-  neo
::  Layer 1
=>  
=/  verb  &
|%
++  lexe
  |=  res=(unit (unit epic:neo))
  ^-  (unit (unit saga:neo))
  ?:  ?=($@(~ [~ ~]) res)
    res
  ?~  fil.u.u.res
    [~ ~]
  ``u.fil.u.u.res
++  dall
  |*  [res=(unit (unit)) def=*] 
  ?~  res
    def
  ?~  u.res
    def
  u.u.res
::
++  trace
  |=  [info=tape =tang]
  ?.  verb
    same
  %.  [leaf/"neo: {info}" tang]
  %*  .  slog
    pri  3
  ==
++  print-card
  |=  =card:dirt:neo
  :-  leaf/(en-tape:pith:neo p.card)
  ?-    -.q.card
      %grow 
    :~  leaf/"%grow"
        ?:  =(%vase p.pail.q.card)
          leaf/"ford build"
        (sell q.pail.q.card)
    ==
      %cull
    ~[leaf/"%cull"]
  ==

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
  |=  [=loam:dirt:neo =turf:neo =pith:neo =case:neo kind=?(%y %z) rift=?]
  ^-  turf:neo
  =/  =tend:neo
    ?:  ?=(%y kind)
      by-kids.plot.turf
    by-desc.plot.turf
  =/  cone=@ud
    ?~  old=(ram:on:tend:neo tend)
      1
    +(key.u.old)
  =.  tend   (put:on:tend:neo tend cone (~(vest plow loam) kind))
  =?  by-kids.plot.turf  ?=(%y kind)
    tend
  =?  by-desc.plot.turf  ?=(%z kind)
    tend
  ?~  pie=(ram:on:land:neo land.turf)
    turf
  =/  [=case:neo =over:neo]  u.pie
  =.  land.turf
    %^  put:on:land:neo  land.turf  case 
    =?  q.why.over  ?=(%y kind)
      cone
    =?  q.zed.over  ?=(%z kind)
      cone
    =?  q.why-mut.over  &(?=(%y kind) rift)
      +(q.why-mut.over)
    =?  q.zed-mut.over  &(?=(%z kind) rift)
      +(q.zed-mut.over)
    over
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
        [. .]:?:(rift +(q.why-mut.over) q.why-mut.over)
        [. .]:.+(q.zed.over)
        ?.(rift zed-mut.over [. .]:+(q.zed-mut.over))
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
      %y  by-kids-mut.plot.turf
      %z  by-desc-mut.plot.turf
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
  =?  by-kids-mut.plot.turf  ?=(%y kind)
    (put:on:plan:neo plan +(case) piths)
  =?  by-desc-mut.plot.turf  ?=(%z kind)
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
  ++  vest
    |=  kind=?(%y %z)
    ^-  (map pith:neo case:neo)
    =.  fil.loam  ~
    =?  loam  ?=(%y kind)
      ~(snip of:neo loam)
    %-  ~(gas by *(map pith:neo @ud))
    %+  murn   ~(tap by ~(tar of:neo loam))
    |=  [=pith:neo =soil:neo]
    ^-  (unit [pith:neo case:neo])
    ?~  lat=(ram:on:soil:neo soil)
      ~
    ?~  q.val.u.lat
      ~
    `[pith key.u.lat]
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
    %-  (trace "call" (print-card card))
    =^  gifts=(list [case:neo ?])  lom
      ?-  -.q.card
        %grow  (~(grow plow lom) +.q.card)
        %cull  ~(cull plow lom)
      ==
    :_  (~(rep of:neo loam) p.card lom)
    (turn gifts |=([@ ?] `gift:dirt:neo`[p.card +<]))
  ::
  ++  look
    |=  =pith:neo
    ^-  (unit (unit poem:neo))
    (scry ~(case plow (~(dip of:neo loam) pith)) pith)
  ::
  ++  peek
    |=  =pith:neo
    ^-  (unit pail:neo)
    =/  res  (look pith)
    ?:  ?=($@(~ [~ ~]) res)
      ~
    q.u.u.res
  ::
  ++  scry
    |=  [wan=case:neo =pith:neo]
    ^-  (unit (unit poem:neo))
    =/  lom  (~(dip of:neo loam) pith)
    ?~  fil.lom
      ~
    =/  latest=case:neo  ~(case plow lom)
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
    =.  farm  (heir pith case rif)
    =.  farm  (chain pith case rif)
    (self pith case rif)
  ++  chain
    =|  here=pith:neo
    |=  [=pith:neo =case:neo rift=?]
    ?~  pith
      farm
    =/  kid  (~(gut by kid.farm) i.pith `farm:neo`[~ ~])
    =?  fil.farm   ?=(^ fil.farm)
      `(bump (~(dip of:neo loam) here) u.fil.farm pith case %z rift)
    =.  here  (snoc here i.pith)
    farm(kid (~(put by kid.farm) i.pith $(farm kid, pith t.pith)))
  ++  slip
    |=  [=pith:neo =ever:neo]
    ^-  (list pith:neo)
    !!

  ::
  ++  heir
    |=  [=pith:neo =case:neo rift=?]
    ^-  farm:neo
    ?~  par=(~(parent of:neo farm) pith)
      farm
    =/  lan=turf:neo  (~(got of:neo farm) u.par)
    %+  ~(put of:neo farm)  u.par
    ^-  turf:neo
    (bump (~(dip of:neo loam) u.par) lan pith case %y rift)
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
  ++  scry
    |=  [=case:neo =pith:neo]
    ^-  (unit (unit saga:neo))
    =/  mit  (~(scry plow loam) case pith)
    ?:  ?=($@(~ [~ ~]) mit)
      mit
    ?~  q.u.u.mit
      [~ ~]
    ?~  val=(~(get of:neo farm) pith)
      [~ ~] :: probably crash?
    ?~  ver=(get:on:land:neo land.u.val case)
      [~ ~]
    =/  =ever:neo  (nail u.ver plot.u.val case)
    ``[[ever q.p.u.u.mit] u.q.u.u.mit]
  ++  peek
    |=  [=care:neo =pith:neo]
    ^-  (unit (unit (axal:neo saga:neo)))
    ?~  val=(~(get of:neo farm) pith)
      ~
    =;  =once:neo
      (look care once pith)
    ~&  peek/[care pith]
    ?+    care  !!
        %x
      ?~  ove=(ram:on:land:neo land.u.val)
        x/0
      x/key.u.ove
    ::
        %y
      ?~  ove=(ram:on:tend:neo by-kids.plot.u.val)
        y/0
      y/key.u.ove
    ::
        %z
      ?~  ove=(ram:on:tend:neo by-desc.plot.u.val)
        z/0
      z/key.u.ove
    ==
  ::
  ++  look
    |=  [=care:neo =once:neo =pith:neo]
    ^-  (unit (unit (axal:neo saga:neo)))
    ?~  val=(~(get of:neo farm) pith)
      ~
    ?~  ver=(jerk u.val once)
      ~
    =/  =ever:neo  u.ver
    =|  axe=(axal:neo saga:neo)
    |^  
    :+  ~  ~
    ^+  axe
    ?+  care  axe
      %x  read-x
      %y  read-y
      %z  read-z
    ==
    ++  gas
      |=  res=(list (pair pith:neo saga:neo))
      ^+  axe
      ?~  res
        axe
      $(axe (~(put of:neo axe) p.i.res q.i.res), res t.res)
    ++  read-x  (gas read-x-raw)
    ++  read-x-raw
      ^-  (list (pair pith:neo saga:neo))
      =+  val=(scry p.exe.ever pith)
      ?:  ?=($@(~ [~ ~]) val)
        ~
      [pith u.u.val]^~
    ::
    ++  read-y
      =;  res=(list (pair pith:neo saga:neo))
        (gas res)
      ^-  (list (pair pith:neo saga:neo))
      %+  welp  read-x-raw
      =/  kids  (got:on:tend:neo by-kids.plot.u.val p.why.ever)
      %+  murn  ~(tap by kids)
      |=  [kid=pith:neo cas=@ud]
      ^-  (unit [pith:neo saga:neo])
      =/  pit  (welp pith kid)
      =/  child  (scry cas pit)
      ?:  ?=($@(~ [~ ~]) child)
        ~
      `[pit u.u.child]
    ::
    ++  read-z
      =;  res=(list (pair pith:neo saga:neo))
        (gas res)
      ^-  (list (pair pith:neo saga:neo))
      %+  welp  read-x-raw
      =/  kids  (got:on:tend:neo by-desc.plot.u.val p.zed.ever)
      %+  murn  ~(tap by kids)
      |=  [kid=pith:neo cas=@ud]
      ^-  (unit [pith:neo saga:neo])
      =/  pit  (welp pith kid)
      =/  child  (scry cas pit)
      ?:  ?=($@(~ [~ ~]) child)
        ~
      `[pit u.u.child]
    --
::  !! :: ``[q.u.u.pom ever q.p.u.u.pom]
  --
--

