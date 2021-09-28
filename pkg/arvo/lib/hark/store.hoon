/-  sur=hark-store, post
/+  resource, graph-store, group-store
^?
=<  [. sur]
=,  sur
|%
++  upgrade
  |%
  ++  to-three
    =*  two    state-two
    =*  three  state-three
    |%
    ++  index
      |=  =index:two
      ^-  (unit index:three)
      `index
    ++  contents
      |=  =contents:two
      ^-  (unit contents:three)
      ?.  ?=(%group -.contents)
        `contents
      =-  ?:  =(~ -)  ~
          `[%group -]
      %+  murn  list.contents
      |=  =group-contents:two
      ^-  (unit group-contents:three)
      ?:  ?=(?(%add %remove) -.group-contents)
        ~
      `group-contents
    ::
    ++  stats-index
      |=  =stats-index:two
      ^-  (unit stats-index:three)
      `stats-index
    ::
    ++  notifications
      upg-notifications:upg
    ::
    ++  upg
      %.  [index stats-index contents]
      %:  upgrade
        index:two
        stats-index:two
        contents:two
        index:three
        stats-index:three
        contents:three
      ==
    --
  ::
  ++  to-four
    =*  three  state-three
    =*  four   state-four
    |%
    ++  index
      |=  =index:three
      ^-  (unit index:four)
      `index
    ++  contents
      |=  =contents:three
      ^-  (unit contents:four)
      ?.  ?=(%graph -.contents)
        `contents
      `[%graph (turn list.contents post-to-one:upgrade:graph-store)]
    ::
    ++  unreads-each
      upg-unreads-each:upg
    ::
    ++  notifications
      upg-notifications:upg
    ::
    ++  stats-index
      |=  =stats-index:three
      ^-  (unit stats-index:four)
      `stats-index
    ::
    ++  upg
      %.  [index stats-index contents]
      %:  upgrade
        index:three
        stats-index:three
        contents:three
        index:four
        stats-index:four
        contents:four
      ==
    --
  ::
  ++  to-five
    =*  four  state-four
    =*  five  sur
    |%
    ++  mark
      |=  module=@t
      ^-  (unit @t)
      ?+  module  ~
        %chat     `%graph-validator-chat
        %publish  `%graph-validator-publish
        %link     `%graph-validator-link
      ==
    ++  index
      |=  =index:four
      ^-  (unit index:five)
      ?:  ?=(%group -.index)
        `index
      =*  i  index
      `[%graph graph.i (mark module.i) description.i index.i]
    ::
    ++  contents
      |=  =contents:four
      ^-  (unit contents:five)
      `contents
    ::
    ++  unreads-each
      upg-unreads-each:upg
    ::
    ++  stats-index
      |=  =stats-index:four
      ^-  (unit stats-index:five)
      `stats-index
    ::
    ++  upg
      %.  [index stats-index contents]
      %:  upgrade
        index:four
        stats-index:four
        contents:four
        index:five
        stats-index:five
        contents:five
      ==

    ++  notifications
      upg-notifications:upg
    --
  ::
  ++  upgrade
    |*  $:  :: input molds
            in-index=mold
            in-stats-index=mold
            in-contents=mold
            :: output molds
            out-index=mold
            out-stats-index=mold
            out-contents=mold
        ==
    =>  .  =>
    |%
    ::
    ++  in
      |%
      ::
      +$  index  in-index
      +$  stats-index  in-stats-index 
      +$  contents     in-contents
      +$  unreads-each  (jug stats-index index)
      +$  timebox  (map index notification)
      +$  notification
         [date=@da read=? =contents]
      ++  orm
        ((ordered-map time timebox) gth)
      +$  notifications
        ((mop time timebox) gth)
      --
    ++  out
      |%
      ::
      ::
      +$  index  out-index
      +$  stats-index  out-stats-index 
      +$  contents     out-contents
      +$  timebox  (map out-index notification)
      +$  unreads-each  (jug stats-index index)
      +$  notification
         [date=@da read=? contents=out-contents]
      +$  notifications
        ((mop time timebox) gth)
      ++  orm
        ((ordered-map time timebox) gth)
      --
    --
    |=  $:  fun-index=$-(index:in (unit index:out))
            fun-stats-index=$-(stats-index:in (unit stats-index:out))
            fun-contents=$-(contents:in (unit contents:out))
        ==
    |%
    ::
    ++  upg-unreads-each
      |=  =unreads-each:in
      ^-  unreads-each:out
      %-  ~(gas by *unreads-each:out)
      %+  murn  ~(tap by unreads-each)
      |=  [=stats-index:in indices=(set index:in)]
      ^-  (unit [stats-index:out (set index:out)])
      =/  new-stats
        (fun-stats-index stats-index)
      ?~  new-stats  ~
      =/  new-indices
        (upg-indices indices)
      ?:  =(0 ~(wyt ^in new-indices))  ~
      `[u.new-stats new-indices]
    ::
    ++  upg-indices
      |=  indices=(set index:in)
      ^-  (set index:out)
      %-  ~(gas ^in *(set index:out))
      (murn ~(tap ^in indices) fun-index)
    ::
    ++  upg-notifications
      |=  =notifications:in
      ^-  notifications:out
      %+  gas:orm:out  *notifications:out
      ^-  (list [@da timebox:out])
      %+  murn  (tap:orm:in notifications)
      |=  [time=@da =timebox:in]
      ^-  (unit [@da =timebox:out])
      =/  new-timebox=timebox:out
        (upg-timebox timebox)
      ?:  =(0 ~(wyt by timebox))
        ~
       `[time new-timebox]
     ::
     ++  upg-timebox
       |=  =timebox:in
       ^-  timebox:out
       %-  ~(gas by *timebox:out)
       %+  murn  ~(tap by timebox)
       |=  [=index:in =notification:in]
       ^-  (unit [index:out notification:out])
       =/  new-index
         (fun-index index)
       ?~  new-index  ~
       =/  new-notification
         (upg-notification notification)
       ?~  new-notification  ~
       `[u.new-index u.new-notification]
     ::
     ++  upg-notification
       |=  n=notification:in
       ^-  (unit notification:out)
       =/  new-contents
         (fun-contents contents.n)
       ?~  new-contents  ~
       `[date.n read.n u.new-contents]
     --
   --

++  dejs
  =,  dejs:format
  |%
  ++  index
    %-  of
    :~  graph+graph-index
        group+group-index
    ==
  ::
  ++  group-index
    %-  ot
    :~  group+dejs-path:resource
        description+so
    ==
  ::
  ++  graph-index
    %-  ot
    :~  graph+dejs-path:resource
        mark+(mu so)
        description+so
        index+(su ;~(pfix fas (more fas dem)))
    ==
  ::
  ++  stats-index
    %-  of
    :~  graph+graph-stats-index
        group+dejs-path:resource
    ==
  ++  graph-stats-index
    %-  ot
    :~  graph+dejs-path:resource
        index+graph-store-index
    ==
  ::  parse date as @ud
  ::    TODO: move to zuse
  ++  sd
    |=  jon=json 
    ^-  @da
    ?>  ?=(%s -.jon)
    `@da`(rash p.jon dem:ag)
  ::
  ++  notif-ref
    ^-  $-(json [(unit @da) ^index])
    %-  ot
    :~  time+(mu sd)
        index+index
    ==
  ++  graph-store-index
    (su ;~(pfix fas (more fas dem)))
  ::
  ++  add
    |=  jon=json
    [*^index *notification]
  ::
  ++  read-graph-index
    %-  ot
    :~  index+stats-index
        target+graph-store-index
    ==
  ::
  ++  action
    ^-  $-(json ^action)
    %-  of
    :~  seen+ul
        archive+notif-ref
        read-note+index
        add-note+add
        set-dnd+bo
        read-count+stats-index
        read-graph+dejs-path:resource
        read-group+dejs-path:resource
        read-each+read-graph-index
        read-all+ul
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
  ++  update
    |=  upd=^update
    ^-  json
    |^
    %+  frond  -.upd
    ?+  -.upd  a+~
        %added      (added +.upd)
        %timebox  (timebox +.upd)
        %set-dnd  b+dnd.upd
        %count    (numb count.upd)
        %more     (more +.upd)
        %read-each  (read-each +.upd)
        %read-count  (stats-index +.upd)
        %unread-each  (unread-each +.upd)
        %unread-count  (unread-count +.upd)
        %remove-graph  s+(enjs-path:resource +.upd)
        %seen-index  (seen-index +.upd)
        %unreads     (unreads +.upd)
        %read-note   (index +.upd)
        %note-read   (note-read +.upd)
        ::
          %archive
        (notif-ref +.upd)
    ==
    ::
    ++  note-read
      |=  [tim=@da idx=^index]
      %-  pairs
      :~  time+s+(scot %ud tim)
          index+(index idx)
      ==
    ::
    ++  stats-index
      |=  s=^stats-index
      %+  frond  -.s
      |^
      ?-  -.s
        %graph  (graph-stats-index +.s)
        %group  s+(enjs-path:resource +.s)
      ==
      ::
      ++  graph-stats-index
        |=  [graph=resource =index:graph-store]
        %-  pairs
        :~  graph+s+(enjs-path:resource graph)
            index+(index:enjs:graph-store index)
        ==
      --
    ::
    ++  unreads
      |=  l=(list [^stats-index ^stats]) 
      ^-  json 
      :-  %a
      ^-  (list json)
      %+  turn  l
      |=  [idx=^stats-index s=^stats]
      %-  pairs
      :~  stats+(stats s)
          index+(stats-index idx)
      ==
    ::
    ++  unread
      |=  =^unreads
      %+  frond
        -.unreads
      ?-  -.unreads
        %each   a+(turn ~(tap by indices.unreads) index:enjs:graph-store)
        ::
          %count
        (numb num.unreads)
      ==
    ::
    ++  stats
      |=  s=^stats
      ^-  json
      %-  pairs
      :~  unreads+(unread unreads.s)
          last+(time last-seen.s)
      ==
    ++  added
      |=  [idx=^index not=^notification]
      ^-  json
      %-  pairs
      :~  index+(index idx)
          notification+(notification not)
      ==
    ::
    ++  notif-ref
      |=  [tim=(unit @da) idx=^index]
      ^-  json
      %-  pairs
      :~  [%time ?~(tim ~ s+(scot %ud u.tim))]
          index+(index idx)
      ==
    ++  seen-index
      |=  [tim=@da idx=^stats-index]
      ^-  json
      %-  pairs
      :~  time+(time tim)
          index+(stats-index idx)
      ==     
    ::
    ++  more
      |=  upds=(list ^update)
      ^-  json
      a+(turn upds update)
    ::
    ++  index
      |=  =^index
      %+  frond  -.index
      |^
      ?-  -.index
        %graph  (graph-index +.index)
        %group  (group-index +.index)
      ==
      :: 
      ++  graph-index
        |=  $:  graph=resource
                mark=(unit mark)
                description=@t
                idx=index:graph-store
            ==
        ^-  json
        %-  pairs
        :~  graph+s+(enjs-path:resource graph)
            mark+s+(fall mark '')
            description+s+description
            index+(index:enjs:graph-store idx)
        ==
      ::
      ++  group-index
        |=  [group=resource description=@t]
        ^-  json
        %-  pairs
        :~  group+s+(enjs-path:resource group)
            description+s+description
        ==
      --
    ::
    ++  notification
      |=  ^notification
      ^-  json
      %-  pairs
      :~  time+(time date)
          contents+(^contents contents)
      ==
    ::
    ++  contents
      |=  =^contents
      ^-  json
      %+  frond  -.contents
      |^
      ?-  -.contents
        %graph  (graph-contents +.contents)
        %group  (group-contents +.contents)
      ==
      ::
      ++  graph-contents
        |=  =(list post:post)
        ^-  json
        :-  %a
        (turn list post:enjs:graph-store)
      ::
      ++  group-contents
        |=  =(list ^group-contents)
        ^-  json
        :-  %a
        %+  turn  list
        |=  =^group-contents
        (update:enjs:group-store group-contents)
      --
    :: 
    ++  indexed-notification
      |=  [=^index =^notification]
      %-  pairs
      :~  index+(^index index)
          notification+(^notification notification)
      ==
    ::
    ++  timebox
      |=  [tim=(unit @da) l=(list [^index ^notification])]
      ^-  json
      %-  pairs
      :~  time+`json`?~(tim ~ s+(scot %ud u.tim))
          :-  %notifications  
          ^-  json
          :-  %a
          %+  turn  l
          |=  [=^index =^notification]
          ^-  json
          (indexed-notification index notification)
      ==
    ::
    ++  read-each
      |=  [s=^stats-index target=index:graph-store]
      %-  pairs
      :~  index+(stats-index s)
          target+(index:enjs:graph-store target)
      ==
    ::
    ++  unread-each
      |=  [s=^stats-index target=index:graph-store tim=@da]
      %-  pairs
      :~  index+(stats-index s)
          target+(index:enjs:graph-store target)
          last+(time tim)
      ==
    ::
    ++  unread-count
      |=  [s=^stats-index tim=@da]
      %-  pairs
      :~  index+(stats-index s)
          last+(time tim)
      ==
    --
  --
::
++  to-stats-index
  |=  =index
  ^-  stats-index
  ?-  -.index
    %graph  [%graph graph.index index.index]
    %group  [%group group.index]
  ==
++  stats-index-is-index
  |=  [=stats-index =index]
  ?-    -.index
      %graph  
    ?.  ?=(%graph -.stats-index)  %.n
    =([graph index]:index [graph index]:stats-index)
    ::
      %group  
    ?.  ?=(%group -.stats-index)  %.n
    =(group:index group:stats-index)
  ==
--
