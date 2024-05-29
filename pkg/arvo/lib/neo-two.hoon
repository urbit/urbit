/-  neo
::  Layer 1
=>  
=/  verb  |
|%
::
++  gas-leaf
  =|  =leaf:neo
  |=  lst=(list [pith:neo mode:neo])
  ^+  leaf
  ?~  lst
    leaf
  =.  leaf   (~(put of:neo leaf) i.lst)
  $(lst t.lst)

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
      [q.why q.why-mut]:over
      [q.zed q.zed-mut]:over
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
  |=  [=loam:dirt:neo =turf:neo =pith:neo rift=?]
  |^  ^+  turf
  =/  [=case:neo =over:neo]
    ?^  pie=(ram:on:land:neo land.turf)
      u.pie
    [0 *over:neo]
  =/  new=over:neo
    :*  [. .]:.+(q.why.over)
        [. .]:?:(rift +(q.why-mut.over) q.why-mut.over)
        [. .]:.+(q.zed.over)
        ?.(rift zed-mut.over [. .]:+(q.zed-mut.over))
        ?:(rift +(rift.over) rift.over)
    ==
  =.  land.turf
    (put:on:land:neo land.turf +(case) new)
  =?  by-kids-mut.plot.turf  rift
    (do-plan by-kids-mut.plot.turf q.why-mut.new)
  =?  by-desc-mut.plot.turf  rift
    (do-plan by-desc-mut.plot.turf q.zed-mut.new)
  =.  by-kids.plot.turf
    (do-tend by-kids.plot.turf q.why.new)
  =.  by-desc.plot.turf
    (do-tend by-desc.plot.turf q.zed.new)
  turf
  ++  do-plan
    |=  [=plan:neo =case:neo]
    ^-  plan:neo
    =/  prev=(pair @ud (set pith:neo))
      (fall (ram:on:plan:neo plan) [0 *(set pith:neo)])
    ?>  =(case +(p.prev))
    (put:on:plan:neo plan case q.prev)
  ++  do-tend
    |=  [=tend:neo =case:neo]
    ^-  tend:neo
    =/  prev=(pair @ud (map pith:neo case:neo))
      (fall (ram:on:tend:neo tend) [0 *(map pith:neo case:neo)])
    ?>  =(case +(p.prev))
    (put:on:tend:neo tend case q.prev)
  --
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
  ?.  &((lte p.hav wan) (gte q.hav wan))
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
    |=  [=pail:neo cas=(unit case:neo) =oath:neo]
    ^-  (quip loot:neo loam:dirt:neo)
    =/  =poem:neo  [[(fall cas +(case)) oath] `pail]
    (make poem)
  ++  make
    |=  =poem:neo
    ^-  (quip loot:neo loam:dirt:neo)
    =?  fil.loam  ?=(~ fil.loam)
      `*soil:neo
    ?>  ?=(^ fil.loam)
    ~|  have/p.p.poem
    ~|  want/+(case)
    :: ?>  (gte p.p.poem +(case))
    =/  old=(unit (pair @ poem:neo))
      (ram:on:soil:neo u.fil.loam)
    =/  =mode:neo
      ?:  =(q.poem ~)
        %del
      ?~  old
        %add
      ?:  =(q.q.u.old ~)
        %add
      %dif
    ?:  &(=(%dif mode) =(q.q:(need old) q.poem))
      ~&  %dupe-skipping
      `loam
    ~|  overwrite-soil/p.p.poem
    ?>  !(has:on:soil:neo u.fil.loam p.p.poem)
    :-  [p.p.poem mode]^~
    loam(fil `(put:on:soil:neo u.fil.loam [p.p .]:poem))
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
    =^  gifts=(list loot:neo)  lom
      ?-  -.q.card
        %grow  (~(grow plow lom) +.q.card)
        %cull  ~(cull plow lom)
      ==
    :_  (~(rep of:neo loam) p.card lom)
    (turn gifts |=(loot:neo `gift:dirt:neo`[p.card +<]))
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
  ++  parent
    =|  here=pith:neo
    =|  res=(unit pith:neo)
    |=  =pith:neo
    ^+  res
    =/  lom  loam
    ?~  pith
      res
    =?  res  ?=(^ fil.lom)
      ?~  val=(ram:on:soil:neo u.fil.lom)
        res
      ?:  =(~ q.val.u.val)
        res
      `here
    =/  nex  (dif:pith:neo here pith)
    ?>  ?=(^ nex)
    $(here (snoc here i.nex), pith t.pith)
  --
--
::  layer 2
|%
++  till
  |_  [=loam:dirt:neo =farm:neo]
  ++  self
    |=  [=pith:neo case=@ud rift=?]
    ^+  farm
    =/  tuf  (~(gut of:neo farm) pith *turf:neo)
    =/  lom  (~(dip of:neo loam) pith)
    =.  tuf  (jolt lom tuf pith rift)
    (~(put of:neo farm) pith tuf)
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
    =/  rift  |(=(mode.i.gis %add) =(mode.i.gis %del))
    $(farm (eternal pith.i.gis case.i.gis rift), gis t.gis)
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
  ++  peek-x
    |=  =pith:neo
    ^-  (unit (unit saga:neo))
    =/  res  (peek %x pith)
    ?:  ?=($@(~ [~ ~]) res)
      res
    `(~(get of:neo u.u.res) ~)
  ++  tell
    =|  gifts=(list gift:dirt:neo)
    |=  =epic:neo
    ^+  [gifts loam farm]
    =/  pic  ~(tap of:neo epic)
    |-
    ?~  pic
      :+  gifts  loam
      (take gifts)
    =/  [=pith:neo =saga:neo]  i.pic
    =/  =card:dirt:neo  [pith %grow q.saga `p.exe.p.p.saga *oath:neo]
    =^  gis=(list gift:dirt:neo)  loam  
      (~(call plow loam) card)
    =.  gifts  (welp gifts gis)
    $(pic t.pic)
  ++  now-once
    |=  [=care:neo =pith:neo]
    ^-  (unit once:neo)
    =/  val  (~(get of:neo farm) pith)
    ?~  val
      ~
    :-  ~
    ?+    care  !!
        %x
      ?~  ove=(ram:on:land:neo land.u.val)
        x/1
      x/key.u.ove
    ::
        %y
      ?~  ove=(ram:on:tend:neo by-kids.plot.u.val)
        y/1
      y/key.u.ove
    ::
        %z
      ?~  ove=(ram:on:tend:neo by-desc.plot.u.val)
        z/1
      z/key.u.ove
    ==

  ++  get-leaf
    |=  [=care:neo =pith:neo]
    ?~  nce=(now-once care pith)
      *leaf:neo
    =/  then  (dall (look care u.nce(p (dec p.u.nce)) pith) *(axal:neo saga:neo))
    =/  now   (dall (look care u.nce pith) *(axal:neo saga:neo))
    =/  new   (~(dif by ~(tar of:neo now)) ~(tar of:neo then))
    =/  del   (~(dif by ~(tar of:neo now)) ~(tar of:neo then))
    =/  int   (~(int by ~(tar of:neo now)) ~(tar of:neo then))
    %-  gas-leaf
    %-  zing
    ^-  (list (list [pith:neo mode:neo]))
    :~  (turn ~(tap by new) |=([pit=pith:neo =saga:neo] [pit %add]))
        (turn ~(tap by del) |=([pit=pith:neo =saga:neo] [pit %del]))
        %+  murn  ~(tap by int)
        |=  [pit=pith:neo =saga:neo]
        ^-  (unit [pith:neo mode:neo])
        ?~  old=(~(get of:neo then) pit)
          ~
        ?:  =(p.exe.p.p.u.old p.exe.p.p.saga)
          ~
        `[pit %dif]
    == 
  ++  peek
    |=  [=care:neo =pith:neo]
    ^-  (unit (unit (axal:neo saga:neo)))
    ?~  nce=(now-once care pith)
      ~
    (look care u.nce pith)
  ++  look-x
    |=  [=case:neo =pith:neo]
    ^-  (unit (unit saga:neo))
    =/  =once:neo  x/case
    =/  res  (look %x once pith)
    ?:  ?=($@(~ [~ ~]) res)
      res
    `(~(get of:neo u.u.res) ~)
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
      [/ u.u.val]^~
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
      `[kid u.u.child]
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
      `[kid u.u.child]
    --
::  !! :: ``[q.u.u.pom ever q.p.u.u.pom]
  --
--

