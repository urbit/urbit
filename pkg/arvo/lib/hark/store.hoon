/-  sur=hark-store, post
/+  resource, graph-store, group-store
^?
=<  [. sur]
=,  sur
|%
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
    :~  group+dejs-path:resource
        graph+dejs-path:resource
        module+so
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
    ^-  $-(json [@da ^index])
    %-  ot
    :~  time+sd
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
        unread-note+notif-ref
        read-note+notif-ref
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
        %unreads   (unreads +.upd)
        ::
          ?(%archive %read-note %unread-note)
        (notif-ref +.upd)
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
          notifications+a+(turn ~(tap in notifications.s) notif-ref)
          last+(time last-seen.s)
      ==
    ++  added
      |=  [tim=@da idx=^index not=^notification]
      ^-  json
      %-  pairs
      :~  time+s+(scot %ud tim)
          index+(index idx)
          notification+(notification not)
      ==
    ::
    ++  notif-ref
      |=  [tim=@da idx=^index]
      ^-  json
      %-  pairs
      :~  time+s+(scot %ud tim)
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
        |=  $:  group=resource
                graph=resource
                module=@t
                description=@t
                idx=index:graph-store
            ==
        ^-  json
        %-  pairs
        :~  group+s+(enjs-path:resource group)
            graph+s+(enjs-path:resource graph)
            module+s+module
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
          read+b+read
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
      |=  [tim=@da arch=? l=(list [^index ^notification])]
      ^-  json
      %-  pairs
      :~  time+s+(scot %ud tim)
          archive+b+arch
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
