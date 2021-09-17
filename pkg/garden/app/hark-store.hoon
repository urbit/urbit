::  hark-store: notifications and unread counts [landscape]
::
::  hark-store can store unread counts differently, depending on the
::  resource.
::  - last seen. This way, hark-store simply stores an index into
::  graph-store, which represents the last "seen" item, useful for
::  high-volume applications which are intrinsically time-ordered. i.e.
::  chats, comments
::  - each. Hark-store will store an index for each item that is unread.
::  Usefull for non-linear, low-volume applications, i.e. blogs,
::  collections
::  
/-  store=hark-store
/+  verb, dbug, default-agent, re=hark-unreads, agentio
::
::
~%  %hark-store-top  ..part  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-2
      state-3
      state-4
      state-5
      state-6
      state-7
      state-8
  ==
::
+$  base-state
  $:  places=(map place:store stats:store)
      seen=timebox:store
      unseen=timebox:store
      =archive:store
      half-open=(map bin:store @da)
  ==
::
+$  state-2
  [%2 *]
::
+$  state-3
  [%3 *]
::
+$  state-4
  [%4 *]
::
+$  state-5
  [%5 *]
::
+$  state-6
  [%6 *]
::
+$  state-7
  [%7 *]
::
+$  state-8
  [%8 base-state]
::
::
+$  cached-state
  $:  by-place=(jug place:store [=lid:store =path])
      ~
  ==
+$  inflated-state
  [state-8 cached-state]
::
++  orm  ((ordered-map @da timebox:store) gth)
--
::
=|  inflated-state
=*  state  -
::
=<
%+  verb  &
%-  agent:dbug
^-  agent:gall
~%  %hark-store-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    ha    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
    io    ~(. agentio bowl)
    pass  pass:io
::
++  on-init
  =.  current-timebox  now.bowl
  `this
::
++  on-save  !>(-.state)
++  on-load
  |=  =old=vase
  =/  old
   !<(versioned-state old-vase)
  =|  cards=(list card)
  |^  ^-  (quip card _this)
  ?:  ?=(%8 -.old)
    =.  -.state  old
    =.  +.state  inflate
    :_(this (flop cards))
  ::
  :_  this
  (poke-our:pass %hark-graph-hook hark-graph-migrate+old-vase)^~
  ::
  ++  index-timebox
    |=  [=lid:store =timebox:store out=_by-place]
    ^+  by-place
    %+  roll  ~(tap by timebox)
    |=  [[=bin:store =notification:store] out=_out]
    (~(put ju out) place.bin [lid path.bin])
  ::
  ++  inflate
    =.  by-place   (index-timebox seen+~ seen by-place)
    =.  by-place   (index-timebox unseen+~ unseen by-place)
    =.  by-place
      %+  roll  (tap:orm archive)
      |=  [[=time =timebox:store] out=_by-place]
      (index-timebox archive/time timebox out)
    +.state
  --
::
++  on-watch  
  |=  =path
  ^-  (quip card _this)
  ?>  (team:title [src our]:bowl)
  |^
  ?+    path   (on-watch:def path)
    [%notes ~]  `this
    ::
      [%updates ~]
    :_  this
    [%give %fact ~ hark-update+!>(initial-updates)]~
    ::
  ==
  ::
  ++  initial-updates
    ^-  update:store
    :-  %more
    ^-  (list update:store)
    :~  [%timebox unseen+~ ~(val by unseen)]
        [%timebox seen+~ ~(val by seen)]
        [%all-stats places]
    ==
  --
::
++  on-peek   
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
    ::
      [%x %recent %inbox @ @ ~]
    =/  date=@da
      (slav %ud i.t.t.t.path)
    =/  length=@ud
      (slav %ud i.t.t.t.t.path)
    :^  ~  ~  %hark-update
    !>  ^-  update:store
    :-  %more
    %+  turn  (tab:orm archive `date length)
    |=  [time=@da =timebox:store]
    ^-  update:store
    [%timebox archive+time ~(val by timebox)]
  ==
::
++  on-poke
  ~/  %hark-store-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark           (on-poke:def mark vase)
        %hark-action   (hark-action !<(action:store vase))
        %noun          (poke-noun !<(* vase))
    ==
  [cards this]
  ::
  ++  poke-noun
    |=  val=*
    ?+  val  ~|(%bad-noun-poke !!)
      %print  ~&(+.state [~ state])
      %clear  [~ state(. *inflated-state)]
    ==
  ::
  ++  poke-us
    |=  =action:store
    ^-  card
    [%pass / %agent [our dap]:bowl %poke hark-action+!>(action)]
  ::
  ++  hark-action
    |=  =action:store
    ^-  (quip card _state)
    abet:(abed:poke-engine:ha action)
  --
::
++  on-agent  on-agent:def
::
++  on-leave  on-leave:def
++  on-arvo  
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?.  ?=([%autoseen ~] wire)
    (on-arvo:def wire sign-arvo)
  `this
::
++  on-fail   on-fail:def
--
|_  =bowl:gall
+*  met  ~(. metadata bowl)
    io   ~(. agentio bowl)
    pass  pass:io
++  poke-engine
  |_  [out=(list update:store) cards=(list card)]
  ++  poke-core  .
  ::
  ++  abed 
    |=  in=action:store
    ^+   poke-core
    ?-  -.in
    ::
      %add-note      (add-note +.in)
      %del-place     (del-place +.in)
      %archive       (do-archive +.in)
    ::
      %unread-count  (unread-count +.in)
      %read-count    (read-count +.in)
    ::
      %read-each    (read-each +.in)
      %unread-each  (unread-each +.in)
    ::
      %read-note    (read-note +.in)
    ::
      %saw-place   (saw-place +.in)
    ::
      %opened       opened
      %archive-all  archive-all
    ::
    ==
  ::
  ++  abet
    ^-  (quip card _state)
    :_  state
    %+  snoc  (flop cards)
    [%give %fact ~[/updates] %hark-update !>([%more (flop out)])]
  :: 
  ++  give  |=(=update:store poke-core(out [update out]))
  ++  emit  |=(=card poke-core(cards [card cards]))
  ::
  ::
  ::  +|  %note
  ::  
  ::  notification tracking
  ++  put-notifs
    |=  [time=@da =timebox:store]
    poke-core(archive (put:orm archive time timebox))
  ::
  ++  put-lid
    |=  [=lid:store =bin:store =notification:store]
    ^+  poke-core
    =.  by-place  (~(put ju by-place) place.bin [lid path.bin]) 
    ?-  -.lid
        %seen
      poke-core(seen (~(put by seen) bin notification))
    ::
        %unseen
      poke-core(unseen (~(put by unseen) bin notification))
    ::
        %archive
      poke-core(archive (~(put re archive) time.lid bin notification))
    ==
  ::
  ++  del-lid
    |=  [=lid:store =bin:store]
    =.  by-place  (~(del ju by-place) place.bin [lid path.bin])
    ?-  -.lid
      %seen     poke-core(seen (~(del by seen) bin))
      %unseen   poke-core(unseen (~(del by unseen) bin))
      %archive  poke-core(archive (~(del re archive) time.lid bin))
    ==
  ::
  ++  add-note
    |=  [=bin:store =body:store]
    ^+  poke-core
    =.  poke-core
      (emit (fact:io hark-update+!>([%add-note bin body]) /notes ~))
    =.  by-place
      (~(put ju by-place) place.bin unseen+~ path.bin)
    =/  existing-notif
      (~(gut by unseen) bin *notification:store)
    =/  new=notification:store
      [now.bowl bin [body body.existing-notif]]
    =.  unseen
      (~(put by unseen) bin new)
    (give %added new)
  ::
  ++  del-place
    |=  =place:store
    =.  poke-core  (give %del-place place)
    =/  notes=(list [=lid:store =path])
      ~(tap in (~(get ju by-place) place))
    |-  ^+  poke-core
    ?~  notes  poke-core
    =,  i.notes
    =.  poke-core
      (del-lid lid path place)
    $(notes t.notes)
  ::
  ++  do-archive
    |=  [=lid:store =bin:store]
    ^+  poke-core  
    ~|  %already-archived
    ?<  ?=(%time -.lid)
    ~|  %non-existent
    =/  =notification:store  (need (get-lid lid bin))
    =.  poke-core  (del-lid lid bin)
    =.  poke-core  (put-lid archive+now.bowl bin notification)
    =?  poke-core  ?=(%unseen -.lid)
      ?~  n=(get-lid seen+~ bin)  poke-core
      =.  archive  
        %^  ~(job re archive)  now.bowl  bin 
        |=  og=(unit notification:store)
        (merge-notification og u.n)
      poke-core
    (give %archived now.bowl lid notification)
  ::
  ++  read-note
    |=  =bin:store
    =/  =notification:store
      (~(got by unseen) bin)
    =.  unseen
      (~(del by unseen) bin)
    =/  =time
      (fall timebox:(gut-place place.bin) now.bowl)
    =.  date.notification  time
    =.  archive  (~(put re archive) time bin notification)
    (give %note-read time bin)
  ::
  ::
  ::  +|  %each
  ::  
  ::  each unread tracking
  ::
  ++  unread-each
    |=  [=place:store =path]
    =.  poke-core  (saw-place place ~)
    =.  poke-core  (give %unread-each place path)
    %+  jub-place  place
    |=(=stats:store stats(each (~(put in each.stats) path)))
  ::
  ++  read-index-each
    |=  [=place:store =path]
    %-  read-bins
    %+  skim
      ~(tap in ~(key by unseen))
    |=  =bin:store
    ?.  =(place place.bin)  %.n
    =/  not=notification:store
      (~(got by unseen) bin)
    (lien body.not |=(=body:store =(binned.body path)))
  ::
  ++  read-each
    |=  [=place:store =path]
    =.  poke-core  (read-index-each place path)
    =.  poke-core  (give %read-each place path)
    %+  jub-place  place
    |=  =stats:store
    %_  stats
      timebox  `now.bowl
      each     (~(del in each.stats) path)
    ==
  ::
  ++  gut-place
    |=  =place:store
    ?:  (~(has by places) place)  (~(got by places) place)
    =|  def=stats:store
    def(timebox ~, last now.bowl)
  ::
  ++  jub-place
    |=  $:  =place:store
            f=$-(stats:store stats:store)
        ==
    ^+  poke-core
    =/  =stats:store
      (gut-place place)
    poke-core(places (~(put by places) place (f stats)))
  ::
  ++  unread-count
    |=  [=place:store inc=? count=@ud]
    =.  poke-core
      (give %unread-count place inc count)
    =.  poke-core  (saw-place place ~)
    =/  f
      ?:  inc  (cury add count)
      (curr sub count)
    %+  jub-place  place
    |=  =stats:store
    stats(count (f count.stats))
  ::
  ++  half-archive
    |=  =place:store
    =/  bins=(list [=lid:store =path])
      ~(tap in (~(get ju by-place) place))
    |-  
    ?~  bins  poke-core
    =/  =bin:store
      [path.i.bins place]
    =*  lid  lid.i.bins
    ?:  ?=(%archive -.lid)
      $(bins t.bins)
    =/  seen-place    (~(get by seen) bin)
    =/  n=(unit notification:store)  (get-lid lid bin)
    ?~  n  $(bins t.bins)
    =*  note  u.n
    =/  =time  (~(gut by half-open) bin now.bowl)
    =?  half-open  !(~(has by half-open) bin)
      (~(put by half-open) bin now.bowl)
    =.  archive
      %^  ~(job re archive)  time  bin
      |=(n=(unit notification:store) (merge-notification n note))
    =.  by-place  (~(put ju by-place) place [archive/now.bowl path.bin])
    =.  poke-core  (give %archived now.bowl unseen+~ (~(got re archive) time bin))
    =.  poke-core  (give %archived now.bowl seen+~ (~(got re archive) time bin))
    $(bins t.bins)
  ::
  ++  read-count
    |=  =place:store
    =.  poke-core  (give %read-count place)
    =.  poke-core  (half-archive place)
    %+  jub-place  place
    |=  =stats:store
    stats(count 0, timebox `now.bowl)
  :: 
  ++  read-bins
    |=  bins=(list bin:store)
    |- 
    ?~  bins  poke-core
    =/  core
      (read-note i.bins)
    $(poke-core core, bins t.bins)
  ::
  ++  saw-place
    |=  [=place:store time=(unit time)]
    =.  poke-core  (give %saw-place place time)
    %+  jub-place  place
    |=(=stats:store stats(last (fall time now.bowl)))
  ::
  ++  archive-seen
    =/  seen=(list [=bin:store =notification:store])  ~(tap by seen)
    poke-core
  ::
  ++  opened
    =.  seen
      %-  ~(gas by *timebox:store)
      %+  murn  ~(tap in (~(uni in ~(key by seen)) ~(key by unseen)))
      |=  =bin:store
      =/  se  (~(get by seen) bin)
      =/  un  (~(get by unseen) bin)  
      ?~  un  
        ?~(se ~ `[bin u.se])
      `[bin (merge-notification se u.un)]
    =.  unseen  ~
    =.  poke-core  (turn-places |=(=stats:store stats(timebox ~)))
    (give %opened ~)
    
  ::
  ++  archive-all
    (give:opened %archive-all ~)
  ::
  ++  turn-places
    |=  f=$-(stats:store stats:store)
    =/  places  ~(tap in ~(key by places))
    |-  ^+  poke-core
    ?~  places  poke-core
    =/  core=_poke-core  (jub-place i.places f)
    $(poke-core core, places t.places)
  --
::
++  get-lid
  |=  [=lid:store =bin:store]
  =/  =timebox:store  ?:(?=(%unseen -.lid) unseen seen)
  (~(get by timebox) bin)
::
++  merge-notification
  |=  [existing=(unit notification:store) new=notification:store]
  ^-  notification:store
  ?~  existing  new
  [(max date.u.existing date.new) bin.new (welp body.new body.u.existing)]
::
::  +key-orm: +key:by for ordered maps
++   key-orm
  |=  =archive:store
  ^-  (list @da)
  (turn (tap:orm archive) |=([@da *] +<-))
::
::  +gut-orm: +gut:by for ordered maps
::    TODO: move to zuse.hoon
++  gut-orm
  |=  [=archive:store time=@da]
  ^-  timebox:store
  (fall (get:orm archive time) ~)
::
::
++  scry
  |*  [=mold p=path]
  ?>  ?=(^ p)
  ?>  ?=(^ t.p)
  .^(mold i.p (scot %p our.bowl) i.t.p (scot %da now.bowl) t.t.p)
::
++  give
  |=  [paths=(list path) update=update:store]
  ^-  (list card)
  [%give %fact paths [%hark-update !>(update)]]~
::
++  tap-nonempty
  |=  =archive:store
  ^-  (list [@da timebox:store])
  %+  skim  (tap:orm archive)
  |=([@da =timebox:store] !=(~(wyt by timebox) 0))
--
