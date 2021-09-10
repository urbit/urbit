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
      =unreads:store
      =reads:store
      current-timebox=@da
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
  $:  by-place=(jug place:store [time=(unit @da) =path])
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
%+  verb  |
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
  ++  index-timebox
    |=  [time=(unit @da) =timebox:store out=_by-place]
    ^+  by-place
    %+  roll  ~(tap by timebox)
    |=  [[=bin:store =notification:store] out=_out]
    (~(put ju out) place.bin [time path.bin])
  ::
  ++  index-reads
    ^+  by-place
    %+  roll  (tap:orm reads)
    |=  [[=time =timebox:store] out=_by-place]
    (index-timebox `time timebox out)
  ::
  ++  inflate
    =.  by-place  index-reads
    =.  by-place   (index-timebox ~ unreads by-place)
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
    :~  [%timebox ~ ~(tap by unreads)]
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
    %+  turn  (tab:orm reads `date length)
    |=  [time=@da =timebox:store]
    ^-  update:store
    [%timebox `time ~(tap by timebox)]
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
      %clear  `state(unreads ~, reads ~)
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
      %seen-index   (seen-index +.in)
    ::
      %seen         seen
      %archive-all  archive-all
      %read-all     read-all
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
    poke-core(reads (put:orm reads time timebox))
  ::
  ++  add-note
    |=  [=bin:store =body:store]
    ^+  poke-core
    =.  poke-core
      (emit (fact:io hark-update+!>([%add-note bin body]) /notes ~))
    =.  by-place
      (~(put ju by-place) place.bin ~ path.bin)
    =/  existing-notif
      (~(gut by unreads) bin *notification:store)
    =/  new=notification:store
      [now.bowl bin (snoc body.existing-notif body)]
    =.  unreads
      (~(put by unreads) bin new)
    (give %added new)
  ::
  ++  del-place
    |=  =place:store
    =/  notes=(list [time=(unit @da) =path])
      ~(tap in (~(get ju by-place) place))
    |-  ^+  poke-core
    ?~  notes  poke-core
    =/  core=_poke-core
      (do-archive time.i.notes [path.i.notes place])
    $(poke-core core, notes t.notes)
  ::
  ++  do-archive
    |=  [time=(unit @da) =bin:store]
    ^+  poke-core  
    =.  poke-core  (give %archive time bin)
    |^
    ?~(time archive-unread (archive-read u.time))
    ::
    ++  archive-unread
      =.  by-place  (~(del ju by-place) place.bin ~ path.bin)
      poke-core(unreads (~(del by unreads) bin))
    ::
    ++  archive-read
      |=  time=@da
      %_  poke-core
        by-place  (~(del ju by-place) place.bin `time path.bin)
        reads     (~(del re reads) time bin)
      ==
    --
  ::
  ++  read-note
    |=  =bin:store
    =/  =notification:store
      (~(got by unreads) bin)
    =.  unreads
      (~(del by unreads) bin)
    =/  =time
      (fall timebox:(gut-place place.bin) now.bowl)
    =.  date.notification  time
    =.  reads  (~(put re reads) time bin notification)
    (give %note-read time bin)
  ::
  ::
  ::  +|  %each
  ::  
  ::  each unread tracking
  ::
  ++  unread-each
    |=  [=place:store =path]
    =.  poke-core  (seen-index place ~)
    =.  poke-core  (give %unread-each place path)
    %+  jub-place  place
    |=(=stats:store stats(each (~(put in each.stats) path)))
  ::
  ++  read-index-each
    |=  [=place:store =path]
    %-  read-bins
    %+  skim
      ~(tap in ~(key by unreads))
    |=  =bin:store
    ?.  =(place place.bin)  %.n
    =/  not=notification:store
      (~(got by unreads) bin)
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
    =.  poke-core  (seen-index place ~)
    =/  f
      ?:  inc  (cury add count)
      (curr sub count)
    %+  jub-place  place
    |=  =stats:store
    stats(count (f count.stats))
  ::
  ++  read-count
    |=  =place:store
    =.  poke-core  (give %read-count place)
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
  ++  seen-index
    |=  [=place:store time=(unit time)]
    =.  poke-core  (give %seen-index place time)
    %+  jub-place  place
    |=(=stats:store stats(last (fall time now.bowl)))
  ::
  ++  seen
    =.  poke-core
      (read-bins ~(tap in ~(key by unreads)))
    =.  poke-core  (turn-places |=(=stats:store stats(timebox ~)))
    poke-core(current-timebox now.bowl)
  ::
  ++  archive-all
    =.  poke-core  (give:seen %archive-all ~)
    poke-core(unreads ~, reads ~)
  ::
  ++  read-all
    =.  poke-core  (give:seen %read-all ~)
    =/  to-read=(list bin:store)  ~(tap in ~(key by unreads))
    |-  
    ?~  to-read  poke-core
    =/  core=_poke-core  (read-note i.to-read)
    $(to-read t.to-read, poke-core core)
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
++  merge-notification
  |=  [existing=(unit notification:store) new=notification:store]
  ^-  notification:store
  ?~  existing  new
  [(max date.u.existing date.new) bin.new (welp body.u.existing body.new)]
::
::  +key-orm: +key:by for ordered maps
++   key-orm
  |=  =reads:store
  ^-  (list @da)
  (turn (tap:orm reads) |=([@da *] +<-))
::
::  +gut-orm: +gut:by for ordered maps
::    TODO: move to zuse.hoon
++  gut-orm
  |=  [=reads:store time=@da]
  ^-  timebox:store
  (fall (get:orm reads time) ~)
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
  |=  =reads:store
  ^-  (list [@da timebox:store])
  %+  skim  (tap:orm reads)
  |=([@da =timebox:store] !=(~(wyt by timebox) 0))
--
