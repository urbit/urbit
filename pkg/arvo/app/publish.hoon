::  publish [landscape]
::
::  stores notebooks in clay, subscribes and allow subscriptions to notebooks
::
/-  *publish
/-  *group
/-  group-hook
/-  *permission-hook
/-  *permission-group-hook
/-  *permission-store
/-  inv=invite-store
/-  *metadata-store
/-  *metadata-hook
/-  contact-view
/-  pull-hook
/-  push-hook
/+  *server
/+  *publish
/+  cram
/+  default-agent
/+  dbug
/+  verb
/+  grpl=group
/+  group-store
/+  graph-store
/+  resource
::
~%  %publish  ..is  ~
|%
+$  card  card:agent:gall
::
+$  collection-zero  [* pos=(map @tas *) *]
::
+$  state-zero
  $:  pubs=(map @tas collection-zero)
      *
  ==
::
+$  state-two
  $:  our-paths=(list path)
      books=(map @tas notebook-2)
      subs=(map [@p @tas] notebook-2)
      tile-num=@ud
  ==
::
+$  state-three
  $:  our-paths=(list path)
      books=(map [@p @tas] notebook)
      tile-num=@ud
      $=  limbo
      $:  notes=(map [@p @tas @tas] note)
          comments=(map [@p @tas @tas @da] comment)
      ==
  ==
::
+$  state-four
  [state-three migrate=migration-state]  
::
::  $migration-state: resources that are unavailable because their host
::  has not processed the ota, and number of times we've tried to reach
::  the host
+$  migration-state
  (map resource @ud)
::
+$  versioned-state
  $%  [%1 state-two]
      [%2 state-two]
      [%3 state-three]
      [%4 state-three]
      [%5 state-three]
      [%6 state-three]
      [%7 state-four]
  ==
::
+$  metadata-delta
  $%  $:  %add
          group-path=path
          app-path=path
          title=@t
          desc=@t
          author=@p
          created=@da
      ==
      [%remove author=@p book=@tas]
  ==
--
::
=|  [%7 state-four]
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this  .
      def   ~(. (default-agent this %|) bol)
      main  ~(. +> bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    `this
  ::
  ++  on-save  !>(state)
  ::
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    =/  old-state=(each versioned-state tang)
      (mule |.(!<(versioned-state old)))
    =|  cards=(list card)
    |^
    ?:  ?=(%| -.old-state)
      =/  zero  !<(state-zero old)
      =/  rav  [%next %t [%da now.bol] /app/publish/notebooks]
      =/  init-cards=(list card)
        :~  [%pass /read/paths %arvo %c %warp our.bol q.byk.bol `rav]
            :*  %pass  /permissions  %agent  [our.bol %permission-store]  %watch
                /updates
            ==
            (invite-poke:main [%create %publish])
            :*  %pass  /invites  %agent  [our.bol %invite-store]  %watch
                /invitatory/publish
            ==
            [%pass /bind %arvo %e %disconnect [~ /'~publish']]
            [%pass /view-bind %arvo %e %connect [~ /'publish-view'] %publish]
            :*  %pass  /srv  %agent  [our.bol %file-server]
                %poke  %file-server-action
                !>([%serve-dir /'~publish' /app/landscape %.n %.y])
            ==
        ==
      =+  ^-  [kick-cards=(list card) old-subs=(jug @tas @p)]  kick-subs
      =/  inv-scry-pax
        /(scot %p our.bol)/invite-store/(scot %da now.bol)/invitatory/publish/noun
      =/  invi=(unit invitatory:inv)  .^((unit invitatory:inv) %gx inv-scry-pax)
      =|  new-state=state-two
      =?  tile-num.new-state  ?=(^ invi)
        ~(wyt by u.invi)
      %=  $
          old-state  [%& %2 new-state]
      ::
          cards
        ;:  weld
          kick-cards
          init-cards
          (move-files old-subs)
        ==
      ==
    ?-  -.p.old-state
        %1
      %=  $
          -.p.old-state  %2
      ::
          cards
        %-  zing
        %+  turn  ~(tap by books.p.old-state)
        |=  [name=@tas book=notebook-2]
        ^-  (list card)
        =/  group-host=(unit @p)
          ?>  ?=(^ writers.book)
          (slaw %p i.writers.book)
        ?~  group-host  ~
        ?:  =(u.group-host our.bol)  ~
        :~  %-  perm-group-hook-poke:main
            [%associate writers.book [[writers.book %white] ~ ~]]
          ::
            (perm-hook-poke:main [%add-owned writers.book writers.book])
        ==
      ==
    ::
        %2
      %=  $
          p.old-state
        =/  new-books=(map [@p @tas] notebook)
          %-  %~  uni  by
            %-  ~(run by subs.p.old-state)
            |=  old-notebook=notebook-2
            ^-  notebook-3
            (convert-notebook-2-3 old-notebook)
          ^-  (map [@p @tas] notebook)
          %-  ~(rep by books.p.old-state)
          |=  [[key=@tas val=notebook-2] out=(map [@p @tas] notebook)]
          ^-  (map [@p @tas] notebook)
          %+  ~(put by out)
            [our.bol key]
          (convert-notebook-2-3 val)
        [%3 our-paths.p.old-state new-books tile-num.p.old-state [~ ~]]
      ==
    ::
        %3
      %=  $
          -.p.old-state  %4
      ::
          cards
        %+  welp  cards
        :~  [%pass /bind %arvo %e %disconnect [~ /'~publish']]
            [%pass /view-bind %arvo %e %connect [~ /'publish-view'] %publish]
            :*  %pass  /srving  %agent  [our.bol %file-server]
                %poke  %file-server-action
                !>([%serve-dir /'~publish' /app/landscape %.n %.y])
        ==  ==
      ==
    ::
        %4
      %=  $
          p.old-state
        =/  new-books=(map [@p @tas] notebook)
          %-  ~(run by books.p.old-state)
          |=  old-notebook=notebook-3
          ^-  notebook-3
          (convert-notebook-3-4 old-notebook)
        [%5 our-paths.p.old-state new-books tile-num.p.old-state [~ ~]]
      ::
          cards
        %+  welp  cards
        :~  [%pass /groups %agent [our.bol %group-store] %watch /groups]
        ==
      ==
    ::
        %5
      %=  $
          -.p.old-state  %6
          cards
        %+  weld  cards
        %+  roll  ~(tap by books.p.old-state)
        |=  [[[who=@p book=@tas] nb=notebook] out=(list card)]
        ^-  (list card)
        ?.  =(who our.bol)
          out
        =/  rid  (de-path:resource writers.nb)
        =/  grp=(unit group)  (scry-group:grup:main rid)
        ?~  grp  out
        ?:  hidden.u.grp
          out
        =/  =tag  [%publish (cat 3 'writers-' book)]
        :_  out
        (group-proxy-poke entity.rid %add-tag rid tag members.u.grp)
      ==
    ::
        %6
      =/  [ours=(set [rid=resource nb=notebook]) theirs=(set resource)]
        %+  roll  ~(tap by books.p.old-state)
        |=  [[[who=@p book=@tas] nb=notebook] [ours=(set [resource notebook]) theirs=(set resource)]]
        ^-  [(set [resource notebook]) (set resource)]
        =/  =resource
          [who book]
        ?.  =(who our.bol)
          ours^(~(put in theirs) resource)
        :_  theirs
        (~(put in ours) [resource nb])
      ::
      %_    $
          p.old-state  
        :+  %7  +.p.old-state 
        %-  ~(gas by *(map resource @ud))
        (turn ~(tap in theirs) (late 0))
        ::
          cards
        ;:  weld
          cards
        ::  move our books to graph-store
          ^-  (list card)
          %-  zing
          %+  turn  ~(tap in ours)
          |=  [rid=resource nb=notebook]
          ^-  (list card)
          =/  =graph:graph-store
            (notebook-to-graph nb)
          :~ 
            %-  poke-graph-store
            :*  %0  date-created.nb  %add-graph
                rid 
                graph
                `%graph-validator-publish
                %.y
            ==
            (poke-graph-push %add rid)
          ==
        ::  for their books, subscribe to graph-pull-hook, to see if host has migrated
          ^-  (list card)
          (turn ~(tap in theirs) check-host-migrate:main)
        ::  leave all subscriptions
          ^-  (list card)
          %+  turn  ~(tap in ~(key by wex.bol))
          |=  [=wire =ship app=term]
          ^-  card
          [%pass wire %agent [ship app] %leave ~]
        ==
      ==
    ::
        %7
      [cards this(state p.old-state)]
    ==
    ++  blank-note-node
      |=  =note
      %*  .   *node:graph-store
        author.post  author.note
        time-sent.post  date-created.note
      ==
    ::
    ++  notebook-to-graph
      |=  =notebook
      ^-  graph:graph-store
      %+  gas:orm:graph-store  *graph:graph-store
      %+  turn  ~(tap by notes.notebook)
      |=  [@ta =note]
      ^-  [atom node:graph-store]
      :-  date-created.note
      %*  .   (blank-note-node note)
        index.post   ~[date-created.note]
        ::
          children
        :-  %graph
        (note-to-revision-container notebook note)
      ==
    ::
    ++  note-to-revision-container
      |=  [=notebook =note]
      ^-  graph:graph-store
      %+  gas:orm:graph-store  *graph:graph-store
      :~
        :-  %1
        %*  .   (blank-note-node note)
          index.post   ~[date-created.note %1]
          children     graph+(note-to-revisions note)
        ==
      ::
        :-  %2
        %*  .   (blank-note-node note)
          index.post   ~[date-created.note %2]
          children  (comments-to-internal-graph note)
        ==
      ==
    ::
    ++  note-to-revisions
      |=  =note
      ^-  graph:graph-store
      %^  put:orm:graph-store
        *graph:graph-store  %1
      =/  body=@t
        =/  file
          (trip file.note)
        =/  idx
          (find ";>" file)
        ?~  idx
          file.note
        %-  crip
        (slag (add 2 u.idx) (trip file.note))
      %*  .  (blank-note-node note)
        index.post   ~[date-created.note %1 %1]
        contents.post  ~[text+title.note text+body]
      ==
    ::
    ++  comments-to-internal-graph
      |=  =note
      ^-  internal-graph:graph-store
      ?:  =(~ comments.note)
        [%empty ~]
      :-  %graph
      %+  gas:orm:graph-store  *graph:graph-store
      %+  turn  ~(tap by comments.note)
      |=  [when=@da =comment]
      ^-  [atom node:graph-store]
      :-  when
      %*  .  *node:graph-store
        author.post  author.comment
        index.post  ~[date-created.note %2 when] 
        time-sent.post  when
        contents.post  [%text content.comment]~
      ==
    ::
    ++  poke-our
      |=  [app=term =cage]
      [%pass / %agent [our.bol app] %poke cage]
    ::
    ++  poke-graph-pull
      |=  =action:pull-hook
      (poke-our %graph-pull-hook pull-hook-action+!>(action))
    ::
    ++  poke-graph-store
      |=  =update:graph-store
      (poke-our %graph-store graph-update+!>(update))
    ::
    ++  poke-graph-push
      |=  =action:push-hook
      (poke-our %graph-push-hook push-hook-action+!>(action))
    ++  convert-notebook-3-4
      |=  prev=notebook-3
      ^-  notebook-3
      %=    prev
          writers
        ?>  ?=(^ writers.prev)
        :-  %ship
        ?:  =('~' i.writers.prev)
          t.writers.prev
        writers.prev
      ::
          subscribers
        ?>  ?=(^ subscribers.prev)
        :-  %ship
        %+  scag  2
        ?:  =('~' i.subscribers.prev)
          t.subscribers.prev
        subscribers.prev

      ==
    ::
    ++  convert-comment-2-3
      |=  prev=comment-2
      ^-  comment-3
      %=  prev
        content  [content.prev %.n]
      ==
    ::
    ++  convert-note-2-3
      |=  prev=note-2
      ^-  note-3
      %=    prev
          comments
        [(~(run by comments.prev) convert-comment-2-3) %.n]
      ==
    ::
    ++  convert-notebook-2-3
      |=  prev=notebook-2
      ^-  notebook-3
      %=    prev
          notes
        %-  ~(run by notes.prev)
        |=  =note-2
        (convert-note-2-3 note-2)
      ==
    ::
    ++  kick-subs
      ^-  [(list card) (jug @tas @p)]
      =+  ^-  [paths=(list path) subs=(jug @tas @p)]
        %+  roll  ~(tap by sup.bol)
        |=  [[duct [who=@p pax=path]] paths=(list path) subs=(jug @tas @p)]
        ^-  [(list path) (jug @tas @p)]
        ?.  ?=([%collection @ ~] pax)
          [paths subs]
        =/  book-name  i.t.pax
        :-  [pax paths]
        (~(put ju subs) book-name who)
      ?~  paths
        [~ subs]
      [[%give %kick paths ~]~ subs]
    ::
    ++  send-invites
      |=  [book=@tas subscribers=(set @p)]
      ^-  (list card)
      %+  turn  ~(tap in subscribers)
      |=  who=@p
      ^-  card
      =/  uid  (sham %publish who book eny.bol)
      =/  =invite:inv
        :*  our.bol  %publish  [our.bol book]  who
            (crip "invite for notebook {<our.bol>}/{(trip book)}")
        ==
      =/  =action:inv  [%invite %publish uid invite]
      [%pass /invite %agent [who %invite-hook] %poke %invite-action !>(action)]
    ::
    ++  move-files
      |=  old-subs=(jug @tas @p)
      ^-  (list card)
      =+  ^-  [cards=(list card) sob=soba:clay]
        %+  roll  .^((list path) %ct (weld our-beak:main /web/publish))
        |=  [pax=path car=(list card) sob=soba:clay]
        ^-  [(list card) soba:clay]
        ?+    pax
            [car sob]
        ::
            [%web %publish @ %publish-info ~]
          =/  book-name  i.t.t.pax
          =/  old=old-info  .^(old-info %cx (welp our-beak:main pax))
          =/  group-pax  /~/(scot %p our.bol)/[book-name]
          =/  book=notebook-info
            [title.old '' =(%open comments.old) / /]
          =+  ^-  [grp-car=(list card) write-pax=path read-pax=path]
            (make-groups:main book-name [group-pax ~ %.n %.n] title.old '')
          =.  writers.book      write-pax
          =.  subscribers.book  read-pax
          =/  inv-car  (send-invites book-name (~(get ju old-subs) book-name))
          :-  :(weld car grp-car inv-car)
          ^-  soba:clay
          :+  [pax %del ~]
            :-  /app/publish/notebooks/[book-name]/publish-info
            [%ins %publish-info !>(book)]
          sob
        ::
            [%web %publish @ @ %udon ~]
          =/  book  i.t.t.pax
          =/  note  i.t.t.t.pax
          :-  car
          :+  [pax %del ~]
            :-  /app/publish/notebooks/[book]/[note]/udon
            [%ins %udon !>(.^(@t %cx (welp our-beak:main pax)))]
          sob
        ::
            [%web %publish @ @ @ %publish-comment ~]
          =/  book  i.t.t.pax
          =/  note  i.t.t.t.pax
          =/  comm  i.t.t.t.t.pax
          =/  old-com  .^(old-comment %cx (welp our-beak:main pax))
          =/  new=comment-2
            [creator.old-com date-created.old-com content.old-com]
          :-  car

          :+  [pax %del ~]
            :-  /app/publish/notebooks/[book]/[note]/[comm]/publish-comment
            [%ins %publish-comment !>(new)]
          sob
        ==
      [[%pass /move-files %arvo %c %info q.byk.bol %& sob] cards]
    --
  ::
  ++  on-poke  on-poke:def
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-peek  on-peek:def
  ++  on-agent  
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?.  ?=([%graph-migrate *] wire)
      (on-agent:def wire sign)
    =/  rid=resource
      (de-path:resource t.wire)
    ?.  ?=(%watch-ack -.sign)
      ~|  "Expected error, please ignore"
      (on-agent:def wire sign)
    ?~  p.sign
      ::  if watch acked successfully, then host has completed OTA, and
      ::  we are safe to add it to the pull-hook
      :_  this(migrate (~(del by migrate) rid))
      ~[(poke-graph-pull:main %add entity.rid rid)]
    ::  if nacked, then set a exponential backoff and retry
    =/  nack-count=@ud
      +((~(gut by migrate) rid 0))
    ?:  (gte nack-count 24)
      ~&  >>>  "failed to migrate notebook {<rid>} to graph-store"
      [~ this]
    :_  this(migrate (~(put by migrate) rid nack-count))
    ::  (bex 19) is roughly 6 days
    =/  wakeup=@da
      (add now.bol (mul ~s1 (bex (min 19 nack-count))))
    [%pass wire %arvo %b %wait wakeup]~
  ::   
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?.  ?=([%graph-migrate *] wire)
      (on-arvo:def wire sign-arvo)
    =/  rid=resource
      (de-path:resource t.wire)
    ?>  ?=([%b %wake *] sign-arvo)
    ~?  ?=(^ error.sign-arvo)
      "behn errored in backoff timers, continuing anyway" 
    :_  this
    ~[(check-host-migrate:main rid)]
  ::
  ++  on-fail  on-fail:def
  --
::
|_  bol=bowl:gall
++  grup  ~(. grpl bol)
::
++  our-beak  /(scot %p our.bol)/[q.byk.bol]/(scot %da now.bol)
::
++  perm-hook-poke
  |=  act=permission-hook-action
  ^-  card
  :*  %pass
      /
      %agent
      [our.bol %permission-hook]
      %poke
      %permission-hook-action
      !>(act)
  ==
::
++  invite-poke
  |=  act=action:inv
  ^-  card
  [%pass / %agent [our.bol %invite-store] %poke %invite-action !>(act)]
::
++  perm-group-hook-poke
  |=  act=permission-group-hook-action
  ^-  card
  :*  %pass
      /
      %agent
      [our.bol %permission-group-hook]
      %poke
      %permission-group-hook-action
      !>(act)
  ==
::
++  generate-invites
  |=  [book=@tas invitees=(set ship)]
  ^-  (list card)
  %+  turn  ~(tap in invitees)
  |=  who=ship
  =/  uid  (sham %publish who book eny.bol)
  =/  =invite:inv
    :*  our.bol  %publish  [our.bol book]  who
        (crip "invite for notebook {<our.bol>}/{(trip book)}")
    ==
  =/  act=action:inv  [%invite %publish uid invite]
  [%pass / %agent [our.bol %invite-hook] %poke %invite-action !>(act)]
::
++  make-groups
  |=  [book=@tas group=group-info title=@t about=@t]
  ^-  [(list card) write=path read=path]
  ?>  ?=(^ group-path.group)
  =/  scry-path
    ;:  welp
      /(scot %p our.bol)/group-store/(scot %da now.bol)
      [%groups group-path.group]
      /noun
    ==
  =/  rid=resource  (de-path:resource group-path.group)
  =/  grp=(unit ^group)  (scry-group:grup rid)
  ?:  use-preexisting.group
    ?~  grp  !!
    ?.  (is-managed group-path.group)  !!
    =/  =tag  [%publish (cat 3 'writers-' book)]
    :_  [group-path.group group-path.group]
    [(group-proxy-poke entity.rid %add-tag rid tag members.u.grp)]~
  ::
  =/  =policy
    *open:policy
  ?:  make-managed.group
    ?^  grp  [~ group-path.group group-path.group]
    ?.  (is-managed group-path.group)  !!
    =/  whole-grp  (~(put in invitees.group) our.bol)
    :_  [group-path.group group-path.group]
    [(contact-view-create [group-path.group whole-grp policy title about])]~
  ::  make unmanaged group
  =*  group-path  group-path.group
  :_  [group-path group-path]
  ?^  grp  ~
  =/  rid=resource
    (de-path:resource group-path)
  :-  (group-poke %add-group rid policy %.y)
  (generate-invites book (~(del in invitees.group) our.bol))
::
++  is-managed
  |=  =path
  ^-  ?
  ?>  ?=(^ path)
  !=(i.path '~')
::
++  group-poke
  |=  =update:group-store
  ^-  card
  [%pass / %agent [our.bol %group-store] %poke %group-update !>(update)]
::
++  group-proxy-poke
  |=  [=ship =update:group-store]
  ^-  card
  [%pass / %agent [ship %group-push-hook] %poke %group-update !>(update)]
++  contact-view-poke
  |=  act=contact-view-action:contact-view
  ^-  card
  [%pass / %agent [our.bol %contact-view] %poke %contact-view-action !>(act)]
::
++  contact-view-create
  |=  [=path ships=(set ship) =policy title=@t description=@t]
  =/  rid=resource
    (de-path:resource path)
  =/  act=contact-view-action:contact-view
    [%create name.rid policy title description]
  (contact-view-poke act)
::
++  check-host-migrate
  |=  rid=resource
  ^-  card
  =/  res-path
    (en-path:resource rid)
  =-  [%pass graph-migrate+res-path %agent -]
  [[entity.rid %graph-push-hook] %watch resource+res-path]
::

::
++  poke-our
  |=  [app=term =cage]
  [%pass / %agent [our.bol app] %poke cage]
::
++  poke-graph-pull
  |=  =action:pull-hook
  (poke-our %graph-pull-hook pull-hook-action+!>(action))
::
--
