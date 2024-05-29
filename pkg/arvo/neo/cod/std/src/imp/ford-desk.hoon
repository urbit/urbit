/@  ford-desk
^-  kook:neo
|%
++  state  pro/%ford-desk
++  poke   (sy %gift ~)
++  kids   
  :+  ~  %z
  %-  ~(gas by *lads:neo)
  =/  mk  |=(=term `pish:neo`[&/term &])
  :~  [(mk %src) pro/%hoon ~]
      [(mk %out) pro/%vase ~]
      [(mk %pre) pro/%vase ~]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  =<
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  +*  run  ~(. +> [bowl ~ !<(ford-desk state-vase)])
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =+  !<(=gift:neo vax)
    =+  !<(sta=ford-desk state-vase)
    =|  cards=(list card:neo)
    =/  gis  ~(tap of:neo gift)
    |-
    ?~  gis
      [cards ford-desk/!>(sta)]
    =/  [=pith:neo =loot:neo]  i.gis
    ?:  =(mode.loot %dif)
      $(gis t.gis)
    ?.  ?=([%src *] pith)
      $(gis t.gis)
    =/  =prop:neo  (pith-to-prop t.pith)
    ?-    mode.loot
        %dif  $(gis t.gis)
        %del  $(cards (welp cards (handle-del:run pith)), gis t.gis)
        %add
      =^  caz=(list card:neo)  sta
        (handle-add:run prop)
      $(cards (welp cards caz), gis t.gis)
    ==
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `(need pal)
  --
  |_  [=bowl:neo cards=(list card:neo) sta=ford-desk]
  ++  abet  [(flop cards) sta]
  ++  emit  |=(=card:neo run(cards [card cards]))
  ++  run  .
  ++  handle-del
    |=  =pith:neo
    ^-  (list card:neo)
    :~  [(welp here.bowl out/pith) %cull ~]
        [(welp here.bowl pre/pith) %cull ~]
    ==
  ++  handle-add
    |=  =prop:neo
    ^-  (quip card:neo _sta)
    =<  abet
    (build-file prop)
  ::
  ++  build-file
    |=  =prop:neo 
    =/  pax  (prop-pith prop)
    =+  !<(src=@t q.pail:(~(got of:neo kids.bowl) pax))
    =/  =file:ford:neo
      ~|  parsing/pax
      (scan (trip src) (rein:ford:neo [our.bowl (tail (welp here.bowl pax))]))
    =.  run  (build-pros (turn pro.file tail))
    :: =.  run  (build-libs (turn lib.file tail))
    ::M?>  built-imports
    =^  pre=pith  run  
      (make-prelude prop file)
    =/  =conf:neo
      (~(gas by *conf:neo) [%sut (welp here.bowl pre)] ~)
    =/  pit  (prop-pith prop)
    (ford-slap out/pit pre src/pit)
  ++  build-pros
    |=  pos=(list stud:neo)
    ^+  run
    ?~  pos
      run
    ?:  ?@(i.pos & =([ship desk]:i.pos [ship desk]:sta))
      $(pos t.pos)
    =.  run  (build-pro ?>(?=(^ i.pos) mark.i.pos))
    $(pos t.pos)
  ++  build-pro
    |=  =mark
    ?:  (~(has of:neo kids.bowl) #/out/pro/[mark])
      run
    ?~  fil=(~(get of:neo kids.bowl) #/src/pro/[mark])
      ~&  missing-dep/mark
      run
    (build-file pro/mark)
  ::
  ++  do-make
    |=  [=pith:neo lib=term sta=(unit pail:neo) =conf:neo]
    (emit (welp here.bowl pith) %make lib sta conf)
  ::
  ++  ford-slap
    |=  [wer=pith sut=pith src=pith]
    %^  do-make  wer  %ford-slap
    `(~(gas by *conf:neo) sut/(ours sut) hoon/(ours src) ~)
  ::
  ++  slop
    |=  [wer=pith a=pith b=pith]
    ~|  %ford-slop
    %^  do-make  wer  %ford-slop
    `(~(gas by *conf:neo) a/(ours a) b/(ours b) ~)
  ++  face
    |=  [wer=pith face=pith sut=pith]
    ~|  %ford-face
    %^  do-make  wer  %ford-face
    `(~(gas by *conf:neo) face/(ours face) sut/(ours sut) ~)
  ++  same
    |=  [wer=pith from=pith]
    ~|  ford-same/[wer from]
    %^  do-make  wer  %ford-same
    `(~(gas by *conf:neo) src/(ours from) ~)
  ++  ours
    |=  p=pith:neo  `pith:neo`[p/our.bowl p]
  ++  make-deps
    =|  idx=@ud
    |=  [pat=pith deps=(list [face=term =pith])]
    ^+  run
    ?~  deps
      ~|  pat
      %+  same  pat
      ?:  =(0 idx)
        #/out/reef
      (snoc pat ud/(dec idx))
    =/  wer=pith  (snoc pat ud/idx)
    =/  fac=pith  (snoc wer %face)
    =/  fav=pith  (snoc fac %term)
    =.  run
      (do-make fav %term `term/!>(face.i.deps) ~)
    =.  run
      (face fac fav pith.i.deps)
    =/  prev=pith
      ?:  =(idx 0)
        #/out/reef
      (snoc pat ud/(dec idx))
    =.  run
      (slop wer fac prev)
    $(deps t.deps, idx +(idx))
  ++  file-to-deps
    |=  =file:ford:neo
    ^-  (list [term pith])
    %+  welp
      (turn pro.file |=(p=pro:ford:neo [face.p %out (~(pith press:neo pro/stud.p) %out)]))
    ~ :: (turn lib.file |=(l=lib:ford:neo [face.l %out (prop-pith prouloc.l)]))
  ++  make-prelude
    |=  [=prop:neo =file:ford:neo]
    ^-  [pith _run]
    =/  pre-path=pith  
      pre/(prop-pith prop)
    [pre-path (make-deps pre-path (file-to-deps file))]
  ::
  ++  prop-pith
    |=  =prop:neo
    ^-  pith:neo
    /[p.prop]/[q.prop]
  ::
  ++  pith-to-prop
    |=  =road:neo
    ?>  ?=([=tack:neo =mark ~] road)
    [tack mark]:road
  ++  exists
    |=  =prop:neo
    ^-  ?
    (~(has of:neo kids.bowl) src/(prop-pith prop))
  ::
  ++  built
    |=  =prop:neo
    ^-  ?
    (~(has of:neo kids.bowl) src/(prop-pith prop))
  --

--
