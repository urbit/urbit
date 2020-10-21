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
  ==
::
++  orm  ((ordered-map @da timebox:store) lth)
--
::
=|  state-0
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
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  `this(state !<(state-0 old))
::
++  on-watch  on-watch:def
++  on-peek   on-peek:def
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
      %unread   (unread +.action)
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
      =.  timebox
        (~(put by timebox) index new)
      =.  notifications
        (put:orm notifications last-seen timebox)
      :_(state (give:ha [/updates]~ %add index notification))
    ::
    ++  do-archive
      |=  [time=@da =index:store]
      ^-  (quip card _state)
      =/  =timebox:store
        (gut-orm:ha notifications time)
      =/  =notification:store
        (~(got by timebox) index)
      =.  timebox
        (~(del by timebox) index)
      :-  (give:ha [/updates]~ %archive time index)
      %_  state
        ::
          notifications
        (put:orm notifications time timebox)
        ::
          archive
        %^  jub-orm:ha  archive  time
        |=  =timebox:store
        ^-  timebox:store
        (~(put by timebox) index notification)
      ==
    ::
    ++  read
      |=  [time=@da =index:store]
      ^-  (quip card _state)
      :-  (give:ha [/updates]~ %read time index)
      state(notifications (change-read-status:ha time index %.y))
    ::
    ++  unread
      |=  [time=@da =index:store]
      ^-  (quip card _state)
      :-  (give:ha [/updates]~ %unread time index)
      state(notifications (change-read-status:ha time index %.n))
    ::
    ++  seen
      ^-  (quip card _state)
      :_  state(last-seen now.bowl)
      :~  cancel-autoseen:ha
          autoseen-timer:ha
      ==
    ::
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
++  merge-notification
  |=  [existing=notification:store new=notification:store]
  ^-  notification:store
  ?-    -.contents.existing
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
  notification(read read)
::  +jub-orm: combo +jab/+gut for ordered maps
++  jub-orm
  |=  [=notifications:store time=@da fun=$-(timebox:store timebox:store)]
  ^-  notifications:store
  =/  =timebox:store
    (fun (gut-orm notifications time))
  (put:orm notifications time timebox)
::
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
  [%give %fact paths [%hark-update !>([%0 update])]]~
--
