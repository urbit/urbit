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
/-  post, group-store, metadata-store
/+  resource, metadata, default-agent, dbug, graph-store, graphl=graph, verb, store=hark-store
::
::
~%  %hark-store-top  ..part  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state:state-zero:store
      state-1
  ==
+$  unread-stats
  [indices=(set index:graph-store) last=@da]
::
+$  state-1
  $:  %1
      unreads-each=(jug index:store index:graph-store)
      unreads-count=(map index:store @ud)
      last-seen=(map index:store @da)
      =notifications:store
      archive=notifications:store
      current-timebox=@da
      dnd=_|
  ==
+$  inflated-state
  $:  state-1
      cache
  ==
::  $cache: useful to have precalculated, but can be derived from state
::  albeit expensively
+$  cache
  $:  by-index=(jug index:store @da)
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
%+  verb  |
%-  agent:dbug
^-  agent:gall
~%  %hark-store-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    ha    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
    met   ~(. metadata bowl)
    gra   ~(. graphl bowl)
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
   !<(versioned-state old-vase)
  =|  cards=(list card)
  |^  
  ?-  -.old
      %1
    [cards this(+.state (inflate-cache:ha old), -.state old)]
    ::
      %0

    %_   $
      ::
        old
      %*  .  *state-1
        notifications    (convert-notifications-1 notifications.old)
        archive          (convert-notifications-1 archive.old)
        current-timebox  current-timebox.old
        dnd              dnd.old
      ==
    ==
  ==
  ++  convert-notifications-1
    |=  old=notifications:state-zero:store
    %+  gas:orm  *notifications:store
    ^-  (list [@da timebox:store])
    %+  murn  
      (tap:orm:state-zero:store old)
    |=  [time=@da =timebox:state-zero:store]
    ^-  (unit [@da timebox:store])
    =/  new-timebox=timebox:store
      (convert-timebox-1 timebox)
    ?:  =(0 ~(wyt by new-timebox))
      ~ 
    `[time new-timebox]
  ::
  ++  convert-timebox-1
    |=  =timebox:state-zero:store
    ^-  timebox:store
    %-  ~(gas by *timebox:store)
    ^-  (list [index:store notification:store])
    %+  murn
      ~(tap by timebox)
    |=  [=index:state-zero:store =notification:state-zero:store]
    ^-  (unit [index:store notification:store])
    =/  new-index=(unit index:store)
      (convert-index-1 index)
    =/  new-notification=(unit notification:store)
      (convert-notification-1 notification)
    ?~  new-index  ~
    ?~  new-notification  ~
    `[u.new-index u.new-notification]

  ::
  ++  convert-index-1
    |=  =index:state-zero:store 
    ^-  (unit index:store)
    ?+  -.index  `index
      %chat  ~
      ::
        %graph  
      =,  index
      `[%graph group graph module description ~]
    ==
  ::
  ++  convert-notification-1
    |=  =notification:state-zero:store
    ^-  (unit notification:store)
    ?:  ?=(%chat -.contents.notification)
      ~
    `notification
  --
::
++  on-watch  
  |=  =path
  ^-  (quip card _this)
  ?>  (team:title [src our]:bowl)
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
    :+  give-unreads
      [%set-dnd dnd]
    %+  weld
      %+  turn
        (tap-nonempty:ha archive)
      (timebox-update &)
    %+  turn
      (tap-nonempty:ha notifications)
    (timebox-update |)
  ::
  ++  give-since-unreads
    ^-  (list [index:store index-stats:store])
    %+  turn
      ~(tap by unreads-count)
    |=  [=index:store count=@ud]
    ?>  ?=(%graph -.index)
    :*  index
        ~(wyt in (~(gut by by-index) index ~))
        [%count count]
        (~(gut by last-seen) index *time)
    ==
  ++  give-each-unreads
    ^-  (list [index:store index-stats:store])
    %+  turn
      ~(tap by unreads-each)
    |=  [=index:store indices=(set index:graph-store)]
    :*  index
        ~(wyt in (~(gut by by-index) index ~))
        [%each indices]
        (~(gut by last-seen) index *time)
    ==
  ::
  ++  give-unreads
    ^-  update:store
    :-  %unreads
    (weld give-each-unreads give-since-unreads)
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
        %noun   ~&  +.state  [~ state]
    ==
  [cards this]
  ::
  ++  hark-action
    |=  =action:store
    ^-  (quip card _state)
    |^
    ?-  -.action  
      %add-note      (add-note +.action)
      %archive       (do-archive +.action)
    ::
      %read-each     (read-each +.action)
      %unread-each   (unread-each +.action)
    ::
      %read-count    (read-count +.action)
      %unread-count  (unread-count +.action)
    ::
      %read-note     (read-note +.action)
      %unread-note   (unread-note +.action)
    ::
      %read-all      read-all
    ::
      %set-dnd       (set-dnd +.action)
      %seen        seen
    ==
    ::
    ++  add-note
      |=  [=index:store =notification:store]
      ^-  (quip card _state)
      =/  =timebox:store
        (gut-orm:ha notifications current-timebox)
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
      :-  (give:ha [/updates]~ %added current-timebox index new)
      %_  state
        +  ?.(new-read +.state (upd-unreads:ha index current-timebox %.n))
        notifications  (put:orm notifications current-timebox new-timebox)
      ==
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
    ++  unread-each
      |=  [=index:store unread=index:graph-store time=@da]
      :-  (give:ha ~[/updates] %unread-each index unread time)
      %_  state
          unreads-each
        %+  jub  index
        |=  indices=(set index:graph-store)
        (~(put in indices) unread)
        ::
          last-seen
        (~(put by last-seen) index time)
      ==
    ::
    ++  jub
      |=  [=index:store f=$-((set index:graph-store) (set index:graph-store))]
      ^-  (jug index:store index:graph-store)
      =/  val=(set index:graph-store)
        (~(gut by unreads-each) index ~)
      (~(put by unreads-each) index (f val))
    ::
    ++  read-each
      |=  [=index:store ref=index:graph-store]
      =/  to-dismiss=(list @da)
        %+  skim
          ~(tap in (~(get ju by-index) index))
        |=  time=@da
        =/  =timebox:store
          (gut-orm notifications time)
        =/  not=(unit notification:store)
          (~(get by timebox) index)
        ?~  not  %.n
        ?>  ?=(%graph -.contents.u.not)
        (lien list.contents.u.not |=(p=post:post =(index.p ref)))
      =|  cards=(list card)
      |- 
      ?^  to-dismiss
        =^  crds  state
          (read-note i.to-dismiss index)
        $(cards (weld cards crds), to-dismiss t.to-dismiss)
      :-  (weld cards (give:ha ~[/updates] %read-each index ref))
      %_    state
        ::
          unreads-each
        %+  jub  index
        |=  indices=(set index:graph-store)
        (~(del in indices) ref)
      ==
    ::
    ++  read-note
      |=  [time=@da =index:store]
      ^-  (quip card _state)
      :-  (give:ha [/updates]~ %read-note time index)
      %_  state
        +  (upd-unreads:ha index time %.y)
        notifications  (change-read-status:ha time index %.y)
      ==
    ::
    ++  unread-note
      |=  [time=@da =index:store]
      ^-  (quip card _state)
      :-  (give:ha [/updates]~ %unread-note time index)
      %_  state
        +  (upd-unreads:ha index time %.n)
        notifications  (change-read-status:ha time index %.n)
      ==
    ::
    ++  read-count
      |=  =index:store
      ^-  (quip card _state)
      =^  cards  state
        (read-index index)
      :-  %+  weld  cards
          (give:ha [/updates]~ %read-count index)
      %_  state
        unreads-count  (~(put by unreads-count) index 0)
      ==
    ::
    ++  read-boxes
      |=  [boxes=(set @da) =index:store]
      ^+  state
      =/  boxes=(list @da)
        ~(tap in boxes)
      |- 
      ?~  boxes  state
      =*  box  i.boxes
      =^  cards  state
        (read-note box index)
      $(boxes t.boxes)
    ::
    ++  read-index
      |=  =index:store
      ^-  (quip card _state)
      =/  boxes=(set @da)
        (~(get ju by-index) index)
      :-  (give:ha ~[/updates] %read-index index)
      (read-boxes boxes index)
    ::
    ++  read-all
      ^-  (quip card _state)
      `state
    ::
    ++  unread-count
      |=  [=index:store time=@da]
      ^-  (quip card _state)
      :-  (give:ha [/updates]~ %unread-count index time)
      =/  curr=@ud
        (~(gut by unreads-count) index 0)
      %_  state
        last-seen      (~(put by last-seen) index time)
        unreads-count  (~(put by unreads-count) index +(curr))
      ==
    ::
    ++  seen
      ^-  (quip card _state)
      :_  state(current-timebox now.bowl)
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
  ?>  ?=([%behn %wake *] sign-arvo)
  :_  this(current-timebox now.bowl)
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
    existing(read %.n, list.contents (weld list.contents.existing list.contents.new))
    ::
       %group
    ?>  ?=(%group -.contents.new)
    existing(read %.n, list.contents (weld list.contents.existing list.contents.new))
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
  [%pass /autoseen %arvo %b %rest (add current-timebox autoseen-interval)]
::
++  autoseen-timer
  ^-  card
  [%pass /autoseen %arvo %b %wait (add now.bowl autoseen-interval)]
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
++  group-for-index
  |=  =index:store
  ^-  (unit resource)
  ?.  ?=(%graph -.index)
    ~
  `group.index
::
++  give-dirtied-unreads
  |=  [=index:store =update:store]
  ^-  (list card)
  =/  group
    (group-for-index index)
  ?~  group  ~
  (give ~[group+(en-path:resource u.group)] update) 
::
++  tap-nonempty
  |=  =notifications:store
  ^-  (list [@da timebox:store])
  %+  skim  (tap:orm notifications)
  |=([@da =timebox:store] !=(~(wyt by timebox) 0))
::
++  inflate-cache
  |=  state-1
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
