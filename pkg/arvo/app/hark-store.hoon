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
      state-6
      state-7
  ==
+$  unread-stats
  [indices=(set index:graph-store) last=@da]
::
+$  base-state
  $:  unreads-each=(jug stats-index:store index:graph-store)
      unreads-count=(map stats-index:store @ud)
      timeboxes=(map stats-index:store @da)
      unread-notes=timebox:store
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
  [%4 state-three:store]
::
+$  state-5
  [%5 state-three:store]
::
+$  state-6
  [%6 state-four:store]
::
+$  state-7
  [%7 base-state]
::
::
++  orm  ((ordered-map @da timebox:store) gth)
--
::
=|  state-7
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
++  on-save  !>(state)
++  on-load
  |=  =old=vase
  ^-  (quip card _this)
  =/  old
   !<(versioned-state old-vase)
  =|  cards=(list card)
  |^ 
  ^-  (quip card _this)
  ?-  -.old
      %7  
    :-  (flop cards)
    this(state old)
  ::
      %6
    %_  $
      -.old  %7
    ::
        +.old
      %*  .  *base-state
        notifications      (notifications:to-five:upgrade:store notifications.old)
        archive            ~
        unreads-each       unreads-each.old
        unreads-count      unreads-count.old
        last-seen          last-seen.old
        current-timebox    current-timebox
        dnd                dnd.old
      ==
    ==
  ::
      %5  
    %_  $
      -.old  %6
      notifications.old  (notifications:to-four:upgrade:store notifications.old)
      archive.old        *notifications:state-four:store
    ==
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
      notifications.old  (notifications:to-three:upgrade:store notifications.old)
      archive.old        *notifications:state-three:store
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
    ^-  (list [index:state-two:store notification:state-two:store])
    %+  murn
      ~(tap by timebox)
    |=  [=index:state-zero:store =notification:state-zero:store]
    ^-  (unit [index:state-two:store notification:state-two:store])
    =/  new-index=(unit index:state-two:store)
      (convert-index-1 index)
    =/  new-notification=(unit notification:state-two:store)
      (convert-notification-1 notification)
    ?~  new-index  ~
    ?~  new-notification  ~
    `[u.new-index u.new-notification]
  ::
  ++  convert-index-1
    |=  =index:state-zero:store 
    ^-  (unit index:state-two:store)
    ?+  -.index  `index
      %chat  ~
      ::
        %graph  
      =,  index
      `[%graph graph *resource module description ~]
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
    :~  give-unreads
        [%set-dnd dnd]
        give-notifications
    ==
  ::
  ++  give-notifications
    ^-  update:store
    [%timebox ~ ~(tap by unread-notes)]
  ::
  ++  give-since-unreads
    ^-  (list [stats-index:store stats:store])
    %+  turn
      ~(tap by unreads-count)
    |=  [=stats-index:store count=@ud]
    :*  stats-index
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
        [%each indices]
        (~(gut by last-seen) stats-index *time)
    ==
  ::
  ++  give-unreads
    ^-  update:store
    :-  %unreads
    ;:  weld 
      give-each-unreads 
      give-since-unreads
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
  `this
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
  ++  put-notifs
    |=  [time=@da =timebox:store]
    poke-core(notifications (put:orm notifications time timebox))
  ::
  ++  add-note
    |=  [=index:store =notification:store]
    ^+  poke-core
    =/  existing-notif
      (~(get by unread-notes) index)
    =/  new=notification:store
      (merge-notification existing-notif notification)
    =.  unread-notes
      (~(put by unread-notes) index new)
    =/  timebox=@da
      (~(gut by timeboxes) (to-stats-index:store index) current-timebox)
    (give %added index new)
  ::
  ++  do-archive
    |=  [time=(unit @da) =index:store]
    ^+  poke-core  
    |^
    ?~(time archive-unread (archive-read u.time))
    ::
    ++  archive-unread
      =.  unread-notes
        (~(del by unread-notes) index)
      (give %archive ~ index)
    ::
    ++  archive-read
      |=  time=@da
      =/  =timebox:store
        (gut-orm notifications time)
      =/  =notification:store
        (~(got by timebox) index)
      =/  new-timebox=timebox:store
        (~(del by timebox) index)
      =.  poke-core
        (put-notifs time new-timebox)
      (give %archive `time index)
    --
  ::
  ++  read-note
    |=  =index:store
    =/  =notification:store
      (~(got by unread-notes) index)
    =.  unread-notes
      (~(del by unread-notes) index)
    =/  =time
      (~(gut by timeboxes) (to-stats-index:store index) current-timebox)
    =/  =timebox:store
      (gut-orm notifications time)
    =/  existing-notif
      (~(get by timebox) index)
    =/  new=notification:store
      (merge-notification existing-notif notification)
    =.  timebox
      (~(put by timebox) index new)
    =.  notifications
      (put:orm notifications time timebox)
    (give %note-read time index)
  ::
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
      ~(tap ^in ~(key by unread-notes))
    |=  =index:store
    ?.  (stats-index-is-index:store stats-index index)  %.n
    =/  not=notification:store
      (~(got by unread-notes) index)
    ?.  ?=(%graph -.index)  %.n
    ?.  ?=(%graph -.contents.not)  %.n
    (lien list.contents.not |=(p=post:post =(index.p ref)))
  ::
  ++  read-each
    |=  [=stats-index:store ref=index:graph-store]
    =.  timeboxes  (~(put by timeboxes) stats-index now.bowl)
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
    =/  times=(list index:store)
      (unread-for-stats-index stats-index)
    =?  timeboxes  !(~(has by timeboxes) stats-index)  (~(put by timeboxes) stats-index now.bowl)
    (give:(read-indices times) %read-count stats-index)
  :: 
  ++  read-indices
    |=  times=(list =index:store)
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
    poke-core
    ::
    ++  get-stats-indices
      %-  ~(gas ^in *(set stats-index:store))
      %+  skim
        ;:  weld
          ~(tap ^in ~(key by unreads-count))
          ~(tap ^in ~(key by last-seen))
          ~(tap ^in ~(key by unreads-each))
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
      =/  times=(list =index:store)
        (unread-for-stats-index i.indices)
      =.  poke-core
        (read-indices times)
      $(indices t.indices)
    --
  ::
  ++  seen
    =.  poke-core
      (read-indices ~(tap ^in ~(key by unread-notes)))
    poke-core(current-timebox now.bowl, timeboxes ~)
  ::
  ++  read-all
    =:  unreads-count  (~(run by unreads-count) _0)
        unreads-each    (~(run by unreads-each) _~)      
        notifications  (~(run by notifications) _~)
      ==
    (give:seen %read-all ~)
  ::
  ++  set-dnd
    |=  d=?
    (give:poke-core(dnd d) %set-dnd d)
  --
::
++  unread-for-stats-index
  |=  =stats-index:store
  %+  skim  ~(tap in ~(key by unread-notes))
  (cury stats-index-is-index:store stats-index)
::
++  merge-notification
  |=  [existing=(unit notification:store) new=notification:store]
  ^-  notification:store
  ?~  existing  new
  ?-    -.contents.u.existing
    ::
      %graph
    ?>  ?=(%graph -.contents.new)
    u.existing(list.contents (weld list.contents.u.existing list.contents.new))
    ::
       %group
    ?>  ?=(%group -.contents.new)
    u.existing(list.contents (weld list.contents.u.existing list.contents.new))
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
--
