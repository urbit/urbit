/-  sur=hark-hook, post
/+  resource, jsonl=json, graph-store
^?
=<  [. sur]
=,  sur
|%
++  unreads-orm  ((ordered-map index:post unread) compare-indexes:post)
++  enjs
  =,  enjs:format
  =,  enjs:jsonl
  |%
  ::
  ++  app-idx-read
    |=  [app=resource =index:post]
    ^-  json
    %-  pairs
    :~  [%resource s+(enjs-path:resource app)]
        [%index (index:update:enjs:graph-store index)]
    ==
  :: 
  ++  read-type
    |=  =^read-type
    ^-  json
    %+  frond  -.read-type
    ^-  json
    ?-  -.read-type
      %group  s+(enjs-path:resource group-resource.read-type)
      %app    s+(enjs-path:resource app-resource.read-type)
      %app-at-index  (app-idx-read +.read-type)
    ==
  ::
  ++  action
    |=  act=^action
    ^-  json
    %+  frond  -.act
    ?-  -.act
      %read  (read-type +.act)
    ::
        ?(%listen %ignore)
      s+(enjs-path:resource app-resource.act)
    ==
  ::
  ++  keys
    |=  =(map group=resource (set =app=resource))
    ^-  json
    %-  pairs
    %+  turn
      ~(tap by map)
    |=  [group=resource apps=(set app=resource)]
    ^-  [@t json]
    :-  (enjs-path:resource group)
    :-  %a
    %+  turn
      ~(tap in apps)
    |=  app=resource
    s+(enjs-path:resource app)
  ::
  ++  unread
    |=  =^unread
    ^-  json
    %-  pairs
    :~  date+(time date.unread)
        module+s+module.unread
        group+s+(enjs-path:resource group-resource.unread)
        app+s+(enjs-path:resource app-resource.unread)
        body+(body body.unread)
    ==
  ::
  ++  group-body
    |=  =^group-body
    ^-  json
    %+  frond  -.group-body
    :-  %a
    ^-  (list json)
    (turn ~(tap in ships.group-body) ship)
  ::
  ++  new-md-body
    |=  [title=@t description=@t]
    ^-  json
    %-  pairs
    :~  title+s+title
        description+s+description
    ==
  ::
  ++  metadata-body
    |=  =^metadata-body
    ^-  json
    %+  frond  -.metadata-body
    ?-  -.metadata-body
      %new  (new-md-body +.metadata-body)
      %changed-title  s+title.metadata-body
      %changed-color  s+(scot %ux color.metadata-body)
      %changed-description  s+description.metadata-body
      %deleted  s+title.metadata-body
    ==
  ++  publish-body-content
    |=  [author=@p title=@t]
    ^-  (list [@t json])
    :~  author+(ship author)
        title+s+title
    ==
  ::
  ++  publish-body
    |=  [=index:post =^publish-body]
    ^-  json
    %-  pairs
    :_  ~[index+(index:enjs:graph-store index)]
    :-  %contents
    %+  frond  -.publish-body
    ?-  -.publish-body
      %post-edit  (pairs (publish-body-content +.publish-body))
      ::
        ?(%post-new %comment)
      %+  pairs
        snippet+s+snippet.publish-body
      (publish-body-content [author title]:publish-body)
    ==
  ::
  ++  link-body
    |=  [=index:post =^link-body]
    ^-  json
    %-  pairs
    :_  ~[index+(index:enjs:graph-store index)]
    :-  %contents
    ^-  json
    %+  frond  -.link-body
    %-  pairs
    :~  author+(ship author.link-body)
        title+s+title.link-body
        ?:  ?=(%comment -.link-body)
          snippet+s+snippet.link-body
        url+s+url.link-body
    ==
  ::
  ++  body
    |=  =^body
    ^-  json
    %+  frond
      -.body
    ?-  -.body
      %group     (group-body +.body)
      %metadata  (metadata-body +.body)
      %publish   (publish-body +.body)
      %link      (link-body +.body)
    ==
  ::
  ++  unread-mop
    |=  [group=resource app-rid=resource =^unread-mop]
    ^-  json
    %-  pairs
    :~  group+s+(enjs-path:resource group)
        resource+s+(enjs-path:resource app-rid)
        ::
        :-  %unreads
        %-  pairs
        %+  turn  (tap:unreads-orm unread-mop)
        |=  [=index:post u=^unread]
        =/  idx
          (index:enjs:graph-store index)
        ?>  ?=(%s -.idx)
        [p.idx (unread u)]
    ==
  ::
  ++  update
    |=  upd=^update
    ^-  json
    ?>  ?=(%0 -.upd)
    ?:  ?=(?(%read %listen %ignore) -.+.upd)
      (action +.upd)
    %+  frond  -.+.upd
    ?-  -.+.upd
      %keys  (keys +.+.upd)
      %add   (unread +.+.upd)
      %unreads  (unread-mop +.+.upd)
    ==
  --
++  dejs
  =,  dejs:format
  =,  dejs:jsonl
  |%
  ::
  ++  read-app-idx
    ^-  $-(json [resource index:post])
    %-  ot
    :~  app+dejs-path:resource
        index+index:dejs:graph-store
    ==
  ::
  ++  read-type
    ^-  $-(json ^read-type)
    %-  of
    :~  group+dejs-path:resource
        app+dejs-path:resource
        app-at-index+read-app-idx
    ==
  ::
  ++  action
    ^-  $-(json ^action)
    %-  of
    :~  listen+dejs-path:resource
        ignore+dejs-path:resource
        read+read-type
    ==
  --
--
