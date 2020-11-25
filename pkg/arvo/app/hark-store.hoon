::  hark-store: notifications [landscape]
::
/-  store=hark-store, post, group-store, metadata-store
/+  resource, metadata, default-agent, dbug, graph-store
::
~%  %hark-store-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0
  $:  %0
      =notifications:store
      archive=notifications:store
      last-seen=@da
      dnd=_|
  ==
+$  inflated-state
  $:  state-0
      cache
  ==
::  $cache: useful to have precalculated, but can be derived from state
::  albeit expensively
+$  cache
  $:  unread-count=@ud
      by-index=(jug index:store @da)
      ~
  ==
::
++  orm  ((ordered-map @da timebox:store) gth)
--
::
=|  inflated-state
=*  state  -
::
=<
%-  agent:dbug
^-  agent:gall
~%  %hark-store-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    ha    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
    met   ~(. metadata bowl)
::
++  on-init
  :_  this
  ~[autoseen-timer]
::
++  on-save  !>(-.state)
++  on-load
  |=  =old=vase
  ^-  (quip card _this)
  =/  old
   !<(state-0 old-vase)
  =.  notifications.old
    (gas:orm *notifications:store (tap:orm notifications.old))
  =.  archive.old
    (gas:orm *notifications:store (tap:orm archive.old))
  `this(-.state old, +.state (inflate-cache old))
::
++  on-watch  
  |=  =path
  ^-  (quip card _this)
  |^
  ?+    path   (on-watch:def path)
    ::
      [%updates ~]
    :_  this
    [%give %fact ~ hark-update+!>(initial-updates)]~
  ==
  ::
  ++  initial-updates
    ^-  update:store
    :-  %more
    ^-  (list update:store)
    :-  unreads
    :+  [%set-dnd dnd]
      [%count unread-count]
    %+  weld
      %+  turn
         %+  scag  3
        (tap-nonempty:ha archive)
      (timebox-update &)
    %+  turn
      %+  scag  3
      (tap-nonempty:ha notifications)
    (timebox-update |)
  ::
  ++  unreads
    ^-  update:store
    :-  %unreads
    ^-  (list [index:store @ud])
    %+  turn
      ~(tap by by-index)
    |=([=index:store =(set @da)] [index ~(wyt in set)])
  ::
  ++  timebox-update
    |=  archived=?
    |=  [time=@da =timebox:store]
    ^-  update:store
    [%timebox time archived ~(tap by timebox)]
  --
::
++  on-peek   
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
    ::
      [%x %recent ?(%archive %inbox) @ @ ~]
    =/  is-archive
      =(%archive i.t.t.path)
    =/  offset=@ud
      (slav %ud i.t.t.t.path)
    =/  length=@ud
      (slav %ud i.t.t.t.t.path)
    :^  ~  ~  %hark-update
    !>  ^-  update:store
    :-  %more
    %+  turn
      %+  scag  length
      %+  slag  offset
      %-  tap-nonempty:ha 
      ?:(is-archive archive notifications)
    |=  [time=@da =timebox:store]
    ^-  update:store
    :^  %timebox  time  is-archive
    ~(tap by timebox)
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
    ==
  [cards this]
  ::
  ++  hark-action
    |=  =action:store
    ^-  (quip card _state)
    |^
    ?-  -.action
      %add      (add +.action)
      %archive  (do-archive +.action)
      %seen     seen
      %read     (read +.action)
      %read-index  (read-index +.action)
      %unread   (unread +.action)
      %set-dnd  (set-dnd +.action)
    ==
    ++  add  
      |=  [=index:store =notification:store]
      ^-  (quip card _state)
      =/  =timebox:store
        (gut-orm:ha notifications last-seen)
      =/  existing-notif
        (~(get by timebox) index)
      =/  new=notification:store
        ?~  existing-notif
          notification
        (merge-notification:ha u.existing-notif notification)
      =/  new-read=?
        ?~  existing-notif
          %.y
        read.u.existing-notif
      =.  read.new  %.n
      =/  new-timebox=timebox:store
        (~(put by timebox) index new)
      :-  (give:ha [/updates]~ %added last-seen index new)
      %_  state
        +  ?.(new-read +.state (upd-unreads:ha index last-seen %.n))
        notifications  (put:orm notifications last-seen new-timebox)
      ==
    ++  read-index
      |=  =index:store
      ^-  (quip card _state)
      =/  times=(list @da)
        ~(tap in (~(gut by by-index) index ~))
      =|  cards=(list card)
      |- 
      ?~  times
        [cards state]
      =*  time  i.times
      =^  crds  state
        (read time index)
      $(cards (weld cards crds), times t.times)
    ::
    ++  do-archive
      |=  [time=@da =index:store]
      ^-  (quip card _state)
      =/  =timebox:store
        (gut-orm:ha notifications time)
      =/  =notification:store
        (~(got by timebox) index)
      =/  new-timebox=timebox:store
        (~(del by timebox) index)
      :-  (give:ha [/updates]~ %archive time index)
      %_  state
        +  ?.(read.notification (upd-unreads:ha index time %.y) +.state)
        ::
          notifications
        (put:orm notifications time new-timebox)
        ::
          archive
        %^  jub-orm:ha  archive  time
        |=  archive-box=timebox:store
        ^-  timebox:store
        (~(put by archive-box) index notification(read %.y))
      ==
    ::
    ++  read
      |=  [time=@da =index:store]
      ^-  (quip card _state)
      :-  (give:ha [/updates]~ %read time index)
      %_  state
        +  (upd-unreads:ha index time %.y)
        unread-count   (dec unread-count)
        notifications  (change-read-status:ha time index %.y)
      ==
    ::
    ++  unread
      |=  [time=@da =index:store]
      ^-  (quip card _state)
      :-  (give:ha [/updates]~ %unread time index)
      %_  state
        +  (upd-unreads:ha index time %.n)
        unread-count   +(unread-count)
        notifications  (change-read-status:ha time index %.n)
      ==
    ::
    ++  seen
      ^-  (quip card _state)
      :_  state(last-seen now.bowl)
      :~  cancel-autoseen:ha
          autoseen-timer:ha
      ==
    ::
    ++  set-dnd
      |=  d=?
      ^-  (quip card _state)
      :_  state(dnd d)
      (give:ha [/updates]~ %set-dnd d)
    --
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
  ?>  ?=([%b %wake *] sign-arvo)
  :_  this(last-seen now.bowl)
  ~[autoseen-timer:ha]
::
++  on-fail   on-fail:def
--
|_  =bowl:gall
+*  met  ~(. metadata bowl)
::
++  tap-nonempty
  |=  =notifications:store
  ^-  (list [@da timebox:store])
  %+  skip  (tap:orm notifications)
  |=([@da =timebox:store] =(0 ~(wyt by timebox)))
::
++  merge-notification
  |=  [existing=notification:store new=notification:store]
  ^-  notification:store
  ?-    -.contents.existing
    ::
      %chat
    ?>  ?=(%chat -.contents.new)
    existing(list.contents (weld list.contents.existing list.contents.new))
    ::
      %graph
    ?>  ?=(%graph -.contents.new)
    existing(list.contents (weld list.contents.existing list.contents.new))
    ::
       %group
    ?>  ?=(%group -.contents.new)
    existing(list.contents (weld list.contents.existing list.contents.new))
  ==
::
++  change-read-status
  |=  [time=@da =index:store read=?]
  ^+  notifications
  %^  jub-orm  notifications  time
  |=  =timebox:store
  %+  ~(jab by timebox)  index
  |=  =notification:store
  ?>  !=(read read.notification)
  notification(read read)
::  +key-orm: +key:by for ordered maps
++   key-orm
  |=  =notifications:store
  ^-  (list @da)
  (turn (tap:orm notifications) |=([key=@da =timebox:store] key))
::  +jub-orm: combo +jab/+gut for ordered maps
::    TODO: move to zuse.hoon
++  jub-orm
  |=  [=notifications:store time=@da fun=$-(timebox:store timebox:store)]
  ^-  notifications:store
  =/  =timebox:store
    (fun (gut-orm notifications time))
  (put:orm notifications time timebox)
::  +gut-orm: +gut:by for ordered maps
::    TODO: move to zuse.hoon
++  gut-orm
  |=  [=notifications:store time=@da]
  ^-  timebox:store
  (fall (get:orm notifications time) ~)
::
++  autoseen-interval  ~h3
++  cancel-autoseen
  ^-  card
  [%pass /autoseen %arvo %b %rest (add last-seen autoseen-interval)]
::
++  autoseen-timer
  ^-  card
  [%pass /autoseen %arvo %b %wait (add now.bowl autoseen-interval)]
::
++  give
  |=  [paths=(list path) update=update:store]
  ^-  (list card)
  [%give %fact paths [%hark-update !>(update)]]~
::
++  upd-unreads
  |=  [=index:store time=@da read=?]
  ^+  +.state
  %_    +.state
    ::
      by-index 
    %.  [index time]
    ?:  read
      ~(del ju by-index)
    ~(put ju by-index)
  ==
::
++  inflate-cache
  |=  state-0
  ^+  +.state
  =/  nots=(list [p=@da =timebox:store])
    (tap:orm notifications)
  |-  =*  outer  $
  ?~  nots
    +.state
  =/  unreads  ~(tap by timebox.i.nots)
  |-  =*  inner  $
  ?~  unreads  
    outer(nots t.nots)
  =*  notification  q.i.unreads
  =*  index         p.i.unreads
  ?:  read.notification
    inner(unreads t.unreads)
  =.  +.state
    (upd-unreads index p.i.nots %.n)
  inner(unreads t.unreads)
--
