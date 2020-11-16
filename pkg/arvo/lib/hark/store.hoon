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
  ++  action
    ^-  $-(json ^action)
    %-  of
    :~  seen+ul
        archive+notif-ref
        unread+notif-ref
        read+notif-ref
        add+add
        set-dnd+bo
        read-index+index
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
        %unreads  (unreads unreads.upd)
        %more     (more +.upd)
        ::
          ?(%archive %read %unread)
        (notif-ref +.upd)
    ==
    ::
    ++  unreads
      |=  l=(list [^index @ud]) 
      ^-  json 
      :-  %a
      ^-  (list json)
      %+  turn  l
      |=  [idx=^index unread=@ud]
      %-  pairs
      :~  unread+(numb unread)
          index+(index idx)
      ==
    ::
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
        |=  [group=resource graph=resource module=@t description=@t]
        ^-  json
        %-  pairs
        :~  group+s+(enjs-path:resource group)
            graph+s+(enjs-path:resource graph)
            module+s+module
            description+s+description
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
    --
  --
--
