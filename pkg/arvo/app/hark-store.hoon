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
        %noun          ~&  +.state  [~ state]
    ==
  [cards this]
  ::
  ++  hark-action
    |=  =action:store
    ^-  (quip card _state)
    abet:translate:(abed:poke-engine:ha action)
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
++  poke-engine
  |_  [in=action:store out=(list update:store) cards=(list card)]
  ++  poke-core  .
  ::
  ++  abed 
    |=  =action:store  poke-core(in action)
  ::
  ++  abet
    ^-  (quip card _state)
    :_  state
    %+  snoc  (flop cards)
    [%give %fact ~[/updates] %hark-update !>([%more (flop out)])]
  :: 
  ++  give
    |=  =update:store  poke-core(out [update out])
  ::
  ++  emit
    |=  =card  poke-core(cards [card cards])
  ::
  ++  translate
    ^+  poke-core
    ?+  -.in  poke-core
    ::
      %add-note      (add-note +.in)
      %archive       (do-archive +.in)
    ::
      %unread-count  (unread-count +.in)
      %read-count    (read-count +.in)
    ::
      %read-each    (read-each +.in)
      %unread-each  (unread-each +.in)
    ::
      %read-note    (read-note +.in)
      %unread-note  (unread-note +.in)
    ::
      %seen-index   (seen-index +.in)
      %set-dnd      (set-dnd +.in)
      %seen         seen
    ==
  ::
  ::  +|  %note
  ::  
  ::  notification tracking
  ++  upd-cache
    |=  [read=? time=@da =index:store]
    poke-core(+.state (^upd-cache read time index))
  ::
  ++  put-notifs
    |=  [time=@da =timebox:store]
    poke-core(notifications (put:orm notifications time timebox))
  ::
  ++  add-note
    |=  [=index:store =notification:store]
    ^+  poke-core
    =/  =timebox:store
      (gut-orm notifications current-timebox)
    =/  existing-notif
      (~(get by timebox) index)
    =/  new=notification:store
      (merge-notification existing-notif notification)
    =/  new-read=?
      ?~  existing-notif  %.y
      read.u.existing-notif
    =/  new-timebox=timebox:store
      (~(put by timebox) index new)
    =.  poke-core  (put-notifs current-timebox new-timebox)
    =?  poke-core  new-read
      (upd-cache %.n current-timebox index)
    (give %added current-timebox index new)
  ::
  ++  do-archive
    |=  [time=@da =index:store]
    ^+  poke-core  
    =/  =timebox:store
      (gut-orm notifications time)
    =/  =notification:store
      (~(got by timebox) index)
    =/  new-timebox=timebox:store
      (~(del by timebox) index)
    =?  poke-core  !read.notification
      (upd-cache %.y time index)
    =.  poke-core
      (put-notifs time new-timebox)
    =.  archive
      %^  jub-orm  archive  time
      |=  archive-box=timebox:store
      (~(put by archive-box) index notification(read %.y))
    (give %archive time index)
  ::
  ++  change-read-status
    |=  [time=@da =index:store read=?]
    =.  poke-core  (upd-cache read time index)
    %_  poke-core
        notifications
      %^  jub-orm  notifications  time
      |=  =timebox:store
      %+  ~(jab by timebox)  index
      |=  n=notification:store
      ?>(!=(read read.n) n(read read))
    ==
  ::
  ++  read-note
    |=  [time=@da =index:store]
    %.  [%read-note time index]
    give:(change-read-status time index %.y)
  ::
  ++  unread-note
    |=  [time=@da =index:store]
    %.  [%unread-note time index]
    give:(change-read-status time index %.n)
  ::
  ::  +|  %each
  ::  
  ::  each unread tracking
  ::
  ++  unread-each
    |=  [=index:store unread=index:graph-store time=@da]
    =.  poke-core  (seen-index time index)
    %+  jub-unreads-each:(give %unread-each index unread time)
      index
    |=  indices=(set index:graph-store)
    (~(put ^in indices) unread)
  ::
  ++  read-index-each
    |=  [=index:store ref=index:graph-store]
    %+  read-boxes  index 
    %+  skim
      ~(tap ^in (~(get ju by-index) index))
    |=  time=@da
    =/  =timebox:store
      (gut-orm notifications time)
    =/  not=notification:store
      (~(gut by timebox) index [now.bowl %.n %graph ~])
    ?>  ?=(%graph -.contents.not)
    (lien list.contents.not |=(p=post:post =(index.p ref)))
  ::
  ++  read-each
    |=  [=index:store ref=index:graph-store]
    =.  poke-core  (read-index-each index ref)
    %+  jub-unreads-each:(give %read-each index ref)
      index
    |=  indices=(set index:graph-store)
    (~(del ^in indices) ref)
  ::
  ++  jub-unreads-each
    |=  $:  =index:store
            f=$-((set index:graph-store) (set index:graph-store))
        ==
    poke-core(unreads-each (jub index f))
  ::
  ++  unread-count
    |=  [=index:store time=@da]
    =/  new-count
      +((~(gut by unreads-count) index 0))
    =.  unreads-count
      (~(put by unreads-count) index new-count)
    (seen-index:(give %unread-count index time) time index)
  ::
  ++  read-count
    |=  =index:store
    =.  unreads-count  (~(put by unreads-count) index 0)
    (give:(read-index index) %read-count index)
  :: 
  ++  read-index
    |=  =index:store
    (read-boxes index ~(tap ^in (~(get ju by-index) index)))
  ::
  ++  read-boxes
    |=  [=index:store boxes=(list @da)]
    ?~  boxes  poke-core
    =/  core=_poke-core
      (read-note i.boxes index)
    $(poke-core core, boxes t.boxes)
  ::
  ++  seen-index
    |=  [time=@da =index:store]
    =/  new-time=@da
      (max time (~(gut by last-seen) index 0))
    =.  last-seen
      (~(put by last-seen) index new-time)
    (give %seen-index new-time index)
  ::
  ++  seen
    =>  (emit cancel-autoseen)
    =>  (emit autoseen-timer)
    poke-core(current-timebox now.bowl)
  ::
  ++  set-dnd
    |=  d=?
    (give:poke-core(dnd d) %set-dnd d)
  --
::
++  merge-notification
  |=  [existing=(unit notification:store) new=notification:store]
  ^-  notification:store
  ?~  existing  new
  ?-    -.contents.u.existing
    ::
      %graph
    ?>  ?=(%graph -.contents.new)
    u.existing(read %.n, list.contents (weld list.contents.u.existing list.contents.new))
    ::
       %group
    ?>  ?=(%group -.contents.new)
    u.existing(read %.n, list.contents (weld list.contents.u.existing list.contents.new))
  ==
::
::  +key-orm: +key:by for ordered maps
++   key-orm
  |=  =notifications:store
  ^-  (list @da)
  (turn (tap:orm notifications) |=([@da *] +<-))
::  +jub-orm: combo +jab/+gut for ordered maps
::    TODO: move to zuse.hoon
++  jub-orm
  |=  [=notifications:store time=@da fun=$-(timebox:store timebox:store)]
  ^-  notifications:store
  =/  =timebox:store
    (fun (gut-orm notifications time))
  (put:orm notifications time timebox)
++  jub
  |=  [=index:store f=$-((set index:graph-store) (set index:graph-store))]
  ^-  (jug index:store index:graph-store)
  =/  val=(set index:graph-store)
    (~(gut by unreads-each) index ~)
  (~(put by unreads-each) index (f val))
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
++  tap-nonempty
  |=  =notifications:store
  ^-  (list [@da timebox:store])
  %+  skim  (tap:orm notifications)
  |=([@da =timebox:store] !=(~(wyt by timebox) 0))
::
++  upd-cache
  |=  [read=? time=@da =index:store]
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
  |=  state-1
  ^+  +.state
  =/  nots=(list [p=@da =timebox:store])
    (tap:orm notifications)
  |-  =*  outer  $
  ?~  nots  +.state
  =/  unreads  ~(tap by timebox.i.nots)
  |-  =*  inner  $
  ?~  unreads  
    outer(nots t.nots)
  =*  notification  q.i.unreads
  =*  index         p.i.unreads
  ?:  read.notification
    inner(unreads t.unreads)
  =.  +.state
    (upd-cache %.n p.i.nots index)
  inner(unreads t.unreads)
--
