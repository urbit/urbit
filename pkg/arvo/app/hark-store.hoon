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
/-  post, group-store, metadata-store, store=hark-store
/+  resource, metadata, default-agent, dbug, graph-store, graphl=graph, verb, store=hark-store
::
::
~%  %hark-store-top  ..part  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state:state-zero:store
      state:state-one:store
      state-2
      state-3
      state-4
      state-5
  ==
+$  unread-stats
  [indices=(set index:graph-store) last=@da]
::
+$  base-state 
  $:  unreads-each=(jug stats-index:store index:graph-store)
      unreads-count=(map stats-index:store @ud)
      last-seen=(map stats-index:store @da)
      =notifications:store
      archive=notifications:store
      current-timebox=@da
      dnd=_|
  ==
::
+$  state-2
  [%2 state-two:store]
::
+$  state-3
  [%3 state-two:store]
::
+$  state-4
  [%4 base-state]
::
+$  state-5
  [%5 base-state]
::
+$  inflated-state
  $:  state-5
      cache
  ==
::  $cache: useful to have precalculated, but can be derived from state
::  albeit expensively
+$  cache
  $:  by-index=(jug stats-index:store [time=@da =index:store])
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
      %5  
    :-  (flop cards)
    this(-.state old, +.state (inflate-cache:ha old))
    ::
      %4
    %_  $
      -.old  %5
      ::
        last-seen.old
      %-  ~(run by last-seen.old)
      |=(old=@da (min old now.bowl))
    ==
    ::
      %3
    %_  $
      -.old  %4
      notifications.old  (convert-notifications-3 notifications.old)
      archive.old  (convert-notifications-3 archive.old)
    ==
    ::
      %2
    %_  $
      -.old  %3
      ::
        cards
      :_  cards
      [%pass / %agent [our dap]:bowl %poke noun+!>(%fix-dangling)]
    ==
    ::
      %1
    %_  $
      ::
        old
      %*  .  *state-2
        unreads-each     ((convert-unread ,(set index:graph-store)) uni-by unreads-each.old)
        unreads-count    ((convert-unread ,@ud) add unreads-count.old)
        last-seen        ((convert-unread ,@da) max last-seen.old)
        notifications    notifications.old
        archive          archive.old
        current-timebox  current-timebox.old
        dnd              dnd.old
      ==
    ==
    ::
      %0
    %_   $
      ::
        old
      %*  .  *state:state-one:store
        notifications    (convert-notifications-1 notifications.old)
        archive          (convert-notifications-1 archive.old)
        current-timebox  current-timebox.old
        dnd              dnd.old
      ==
    ==
  ==
  ::
  ++  convert-notifications-3
    |=  old=notifications:state-two:store
    %+  gas:orm  *notifications:store
    ^-  (list [@da timebox:store])
    %+  murn  
      (tap:orm:state-two:store old)
    |=  [time=@da =timebox:state-two:store]
    ^-  (unit [@da timebox:store])
    =/  new-timebox=timebox:store
      (convert-timebox-3 timebox)
    ?:  =(0 ~(wyt by new-timebox))
      ~ 
    `[time new-timebox]
  ::
  ++  convert-timebox-3
    |=  =timebox:state-two:store
    ^-  timebox:store
    %-  ~(gas by *timebox:store)
    ^-  (list [index:store notification:store])
    %+  murn
      ~(tap by timebox)
    |=  [=index:store =notification:state-two:store]
    ^-  (unit [index:store notification:store])
    =/  new-notification=(unit notification:store)
      (convert-notification-3 notification)
    ?~  new-notification  ~
    `[index u.new-notification]
  ::
  ++  convert-notification-3
    |=  =notification:state-two:store
    ^-  (unit notification:store)
    ?:  ?=(%graph -.contents.notification)
      `notification
    =/  con=(list group-contents:store)
      (convert-group-contents-3 list.contents.notification)
    ?:  =(~ con)  ~
    =,  notification
    `[date read %group con]
  ::
  ++  convert-group-contents-3
    |=  con=(list group-contents:state-two:store)
    ^-  (list group-contents:store)
    %+  murn  con
    |=  =group-contents:state-two:store
    ^-  (unit group-contents:store)
    ?.  ?=(?(%add-members %remove-members) -.group-contents)  ~
    `group-contents
  ::
  ++  uni-by
    |=  [a=(set index:graph-store) b=(set index:graph-store)]
    =/  merged
      (~(uni in a) b)
    %-  ~(gas in *(set index:graph-store))
    %+  skip  ~(tap in merged)
    |=(=index:graph-store &(=((lent index) 3) !=(-:(flop index) 1)))
  ::
  ++  convert-unread
    |*  value=mold
    |=  [combine=$-([value value] value) unreads=(map index:store value)]
    ^-  (map stats-index:store value)
    %+  roll
      ~(tap in unreads)
    |=  [[=index:store val=value] out=(map stats-index:store value)]
    =/  old=value
      (~(gut by unreads) index (combine))
    =/  =stats-index:store
      (to-stats-index:store index)
    (~(put by out) stats-index (combine old val))
  ::
  ++  convert-notifications-1
    |=  old=notifications:state-zero:store
    %+  gas:orm:state-two:store  *notifications:state-two:store
    ^-  (list [@da timebox:state-two:store])
    %+  murn  
      (tap:orm:state-zero:store old)
    |=  [time=@da =timebox:state-zero:store]
    ^-  (unit [@da timebox:state-two:store])
    =/  new-timebox=timebox:state-two:store
      (convert-timebox-1 timebox)
    ?:  =(0 ~(wyt by new-timebox))
      ~ 
    `[time new-timebox]
  ::
  ++  convert-timebox-1
    |=  =timebox:state-zero:store
    ^-  timebox:state-two:store
    %-  ~(gas by *timebox:state-two:store)
    ^-  (list [index:store notification:state-two:store])
    %+  murn
      ~(tap by timebox)
    |=  [=index:state-zero:store =notification:state-zero:store]
    ^-  (unit [index:store notification:state-two:store])
    =/  new-index=(unit index:store)
      (convert-index-1 index)
    =/  new-notification=(unit notification:state-two:store)
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
    ^-  (unit notification:state-two:store)
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
    :-  give-unreads
    [%set-dnd dnd]~
  ::
  ++  give-since-unreads
    ^-  (list [stats-index:store stats:store])
    %+  turn
      ~(tap by unreads-count)
    |=  [=stats-index:store count=@ud]
    :*  stats-index
        ~(wyt in (~(gut by by-index) stats-index ~))
        [%count count]
        (~(gut by last-seen) stats-index *time)
    ==
  ::
  ++  give-each-unreads
    ^-  (list [stats-index:store stats:store])
    %+  turn
      ~(tap by unreads-each)
    |=  [=stats-index:store indices=(set index:graph-store)]
    :*  stats-index
        ~(wyt in (~(gut by by-index) stats-index ~))
        [%each indices]
        (~(gut by last-seen) stats-index *time)
    ==
  ::
  ++  give-group-unreads
    ^-  (list [stats-index:store stats:store])
    %+  murn  ~(tap by by-index)
    |=  [=stats-index:store nots=(set [time index:store])]
    ?.  ?=(%group -.stats-index)
      ~
    :-  ~
    :*  stats-index
        ~(wyt in nots)
        [%count 0]
        *time
    ==
  ::
  ++  give-unreads
    ^-  update:store
    :-  %unreads
    ;:  weld 
      give-each-unreads 
      give-since-unreads
      give-group-unreads
    ==
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
        %noun          (poke-noun !<(* vase))
    ==
  [cards this]
  ::
  ++  poke-noun
    |=  val=*
    ?+  val  ~|(%bad-noun-poke !!)
      %fix-dangling  fix-dangling
      %print  ~&(+.state [~ state])
    ==
  ::
  ++  fix-dangling
    =/  graphs  get-keys:gra
    :_  state
    %+  roll
      ~(tap by unreads-each)
    |=  $:  [=stats-index:store indices=(set index:graph-store)]
            out=(list card)
        ==
    ?.  ?=(%graph -.stats-index)  out
    ?.  (~(has in graphs) graph.stats-index)
      :_(out (poke-us %remove-graph graph.stats-index))
    %+  welp  out
    %+  turn
      %+  skip
        ~(tap in indices) 
      |=  =index:graph-store
      (check-node-existence:gra graph.stats-index index)
    |=(=index:graph-store (poke-us %read-each stats-index index))
  ::
  ++  poke-us
    |=  =action:store
    ^-  card
    [%pass / %agent [our dap]:bowl %poke hark-action+!>(action)]
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
    ?-  -.in
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
      %remove-graph  (remove-graph +.in)
      %set-dnd      (set-dnd +.in)
      %seen         seen
      %read-all     read-all
    ::
    ==
  ::
  ::  +|  %note
  ::  
  ::  notification tracking
  ++  upd-cache
    |=  [read=? time=@da =index:store]
    poke-core(+.state (^upd-cache read time index))
  ::
  ++  rebuild-cache
    poke-core(+.state (inflate-cache -.state))
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
  ::  if we detect cache inconsistencies, wipe and rebuild
  ++  change-read-status
    |=  [time=@da =index:store read=?]
    ^+  poke-core
    =.  poke-core  (upd-cache read time index)
    =/  tib=(unit timebox:store)
      (get:orm notifications time)
    ?~  tib  poke-core
    =/  not=(unit notification:store)
      (~(get by u.tib) index)
    ?~  not  poke-core
    =?    poke-core
        ::  cache is inconsistent iff we didn't directly
        ::  call this through %read-note or %unread-note
        &(=(read read.u.not) !?=(?(%read-note %unread-note) -.in))
      ~&  >>  "Inconsistent hark cache, rebuilding"
      rebuild-cache
    ?<  &(=(read read.u.not) ?=(?(%read-note %unread-note) -.in))
    =.  u.tib
      (~(put by u.tib) index u.not(read read))
    =.  notifications
      (put:orm notifications time u.tib)
    poke-core
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
    |=  [=stats-index:store unread=index:graph-store time=@da]
    =.  poke-core  (seen-index time stats-index)
    %+  jub-unreads-each:(give %unread-each stats-index unread time)
      stats-index
    |=  indices=(set index:graph-store)
    (~(put ^in indices) unread)
  ::
  ++  read-index-each
    |=  [=stats-index:store ref=index:graph-store]
    %-  read-indices
    %+  skim
      ~(tap ^in (~(get ju by-index) stats-index))
    |=  [time=@da =index:store]
    =/  =timebox:store
      (gut-orm notifications time)
    =/  not=notification:store
      (~(got by timebox) index)
    ?.  ?=(%graph -.index)  %.n
    ?.  ?=(%graph -.contents.not)  %.n
    (lien list.contents.not |=(p=post:post =(index.p ref)))
  ::
  ++  read-each
    |=  [=stats-index:store ref=index:graph-store]
    =.  poke-core  (read-index-each stats-index ref)
    %+  jub-unreads-each:(give %read-each stats-index ref)
      stats-index
    |=  indices=(set index:graph-store)
    (~(del ^in indices) ref)
  ::
  ++  jub-unreads-each
    |=  $:  =stats-index:store
            f=$-((set index:graph-store) (set index:graph-store))
        ==
    poke-core(unreads-each (jub stats-index f))
  ::
  ++  unread-count
    |=  [=stats-index:store time=@da]
    =/  new-count
      +((~(gut by unreads-count) stats-index 0))
    =.  unreads-count
      (~(put by unreads-count) stats-index new-count)
    (seen-index:(give %unread-count stats-index time) time stats-index)
  ::
  ++  read-count
    |=  =stats-index:store
    =.  unreads-count  (~(put by unreads-count) stats-index 0)
    =/  times=(list [@da index:store])
      ~(tap ^in (~(get ju by-index) stats-index))
    (give:(read-indices times) %read-count stats-index)
  :: 
  ++  read-indices
    |=  times=(list [time=@da =index:store])
    |- 
    ?~  times  poke-core
    =/  core
      (read-note i.times)
    $(poke-core core, times t.times)
  ::
  ++  seen-index
    |=  [time=@da =stats-index:store]
    =/  new-time=@da
      (max time (~(gut by last-seen) stats-index 0))
    =.  last-seen
      (~(put by last-seen) stats-index new-time)
    (give %seen-index new-time stats-index)
  ::
  ++  remove-graph
    |=  rid=resource
    |^  
    =/  indices  get-stats-indices
    =.  poke-core
      (give %remove-graph rid)
    =.  poke-core
      (remove-notifications indices)
    =.  unreads-count
      ((dif-map-by-key ,@ud) unreads-count indices)
    =.  unreads-each
      %+  (dif-map-by-key ,(set index:graph-store))
      unreads-each  indices
    =.  last-seen
      ((dif-map-by-key ,@da) last-seen indices)
    =.  by-index
      ((dif-map-by-key ,(set [@da =index:store])) by-index indices)
    poke-core
    ::
    ++  get-stats-indices
      %-  ~(gas ^in *(set stats-index:store))
      %+  skim
        ;:  weld
          ~(tap ^in ~(key by unreads-count))
          ~(tap ^in ~(key by last-seen))
          ~(tap ^in ~(key by unreads-each))
          ~(tap ^in ~(key by by-index))
        ==
      |=  =stats-index:store
      ?.  ?=(%graph -.stats-index)  %.n
      =(graph.stats-index rid)
    ::
    ++  dif-map-by-key
      |*  value=mold
      |=  [=(map stats-index:store value) =(set stats-index:store)]
      =/  to-remove   ~(tap ^in set)
      |-  
      ?~  to-remove  map
      =.  map
        (~(del by map) i.to-remove)
      $(to-remove t.to-remove)
    ::
    ++  remove-notifications
      |=  =(set stats-index:store)
      ^+  poke-core
      =/  indices
        ~(tap ^in set)
      |- 
      ?~  indices  poke-core
      =/  times=(list [time=@da =index:store])
        ~(tap ^in (~(get ju by-index) i.indices))
      =.  poke-core
        (read-indices times)
      $(indices t.indices)
    --
  ::
  ++  seen
    =>  (emit cancel-autoseen)
    =>  (emit autoseen-timer)
    poke-core(current-timebox now.bowl)
  ::
  ++  read-all
    =:  unreads-count  (~(run by unreads-count) _0)
        unreads-each    (~(run by unreads-each) _~)      
        notifications  (~(run by notifications) _~)
      ==
    (give:seen:rebuild-cache %read-all ~)
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
  |=  [=stats-index:store f=$-((set index:graph-store) (set index:graph-store))]
  ^-  (jug stats-index:store index:graph-store)
  =/  val=(set index:graph-store)
    (~(gut by unreads-each) stats-index ~)
  (~(put by unreads-each) stats-index (f val))
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
    %.  [(to-stats-index:store index) time index]
    ?:  read
      ~(del ju by-index)
    ~(put ju by-index)
  ==
::
++  inflate-cache
  |=  state-5
  ^+  +.state
  =.  +.state
    *cache
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
