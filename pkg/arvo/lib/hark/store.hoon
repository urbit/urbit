/-  sur=hark-store, post
/+  resource, graph-store, group-store, chat-store
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
        chat+chat-index
    ==
  ::
  ++  chat-index
    %-  ot
    :~  chat+pa
        mention+bo
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
  ::
  ++  add
    |=  jon=json
    [*^index *notification]
  ::
  ++  read-graph-index
    %-  ot
    :~  index+index
        target+(su ;~(pfix fas (more fas dem)))
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
        read-count+index
        read-each+read-graph-index
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
        %read-count  (index +.upd)
        %unread-each  (unread-each +.upd)
        %unread-count  (unread-count +.upd)
        %unreads   (unreads +.upd)
        ::
          ?(%archive %read-note %unread-note)
        (notif-ref +.upd)
    ==
    ::
    ++  unreads
      |=  l=(list [^index ^index-stats]) 
      ^-  json 
      :-  %a
      ^-  (list json)
      %+  turn  l
      |=  [idx=^index stats=^index-stats]
      %-  pairs
      :~  stats+(index-stats stats)
          index+(index idx)
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
    ++  index-stats
      |=  stats=^index-stats
      ^-  json
      %-  pairs
      :~  unreads+(unread unreads.stats)
          notifications+(numb notifications.stats)
          last+(time last-seen.stats)
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
        %chat   (chat-index +.index)
      ==
      ::
      ++  chat-index
        |=  [chat=^path mention=?]
        ^-  json
        %-  pairs
        :~  chat+(path chat)
            mention+b+mention
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
        %chat   (chat-contents +.contents)
      ==
      ::
      ++  chat-contents
        |=  =(list envelope:chat-store)
        ^-  json
        :-  %a
        (turn list envelope:enjs:chat-store)
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
        %+  murn  list
        |=  =^group-contents
        ?.  ?=(?(%add-members %remove-members) -.group-contents)
          ~
        `(update:enjs:group-store group-contents)
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
      |=  [=^index target=index:graph-store]
      %-  pairs
      :~  index+(^index index)
          target+(index:enjs:graph-store target)
      ==
    ::
    ++  unread-each
      |=  [=^index target=index:graph-store tim=@da]
      %-  pairs
      :~  index+(^index index)
          target+(index:enjs:graph-store target)
          last+(time tim)
      ==
    ::
    ++  unread-count
      |=  [=^index tim=@da]
      %-  pairs
      :~  index+(^index index)
          last+(time tim)
      ==
    --
  --
--
