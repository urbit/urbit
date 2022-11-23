:: Migrate scripts
/-  gra=graph-store
/-  met=metadata-store
/-  grp=group-store
/-  i=migrate
/-  *group
|_  =bowl:gall
+$  card  card:agent:gall
++  import-club
  |=  [=^groups =associations:met =network:gra]
  %-  ~(gas by *imports:club:i)
  %+  murn  ~(tap by graphs.network)
  |=  [=flag:i graph=graph:gra mar=(unit mark)]
  ^-  (unit [flag:i import:club:i])
  ?.  =(mar `%graph-validator-chat)
    ~
  ?~  assoc=(~(get by associations) [%graph flag])
    ~&  missing-assoc-club/flag
    ~
  ?~  group=(~(get by groups) group.u.assoc)
    ~&  missing-group/[flag group.u.assoc]
    ~
  ?.  hidden.u.group
    ~
  `[flag members.u.group u.assoc graph]
::
++  import-for-mark
  |=  [her=ship =^groups =associations:met =network:gra]
  |=  =mark
  ^-  imports:graph:i
  %-  ~(gas by *imports:graph:i)
  %+  murn  ~(tap by graphs.network)
  |=  [=flag:i graph=graph:gra mar=(unit ^mark)]
  ?.  =(p.flag her)
    ~
  ?.  =(mar `mark)  ::  XX: correct detection?
    ~
  ?~  assoc=(~(get by associations) [%graph flag])
    ~&  missing-assoc/flag^mark
    ~
  ?~  group=(~(get by groups) group.u.assoc)
    ~&  missing-group/[flag group.u.assoc]
    ~
  ?:  hidden.u.group
    ~
  =/  writers=(set ship)  
    (~(get ju tags.u.group) %graph flag %writers)
  ?~  log=(~(get by update-logs.network) flag)
    ~&  missing-log/flag  :: XX: doesn't need to fail, but suspect case
    ~
  `[flag writers u.assoc u.log graph]

++  scry
  |=  [=dude:gall =path]
  %-  welp 
  :_  path
  /gx/(scot %p our.bowl)/[dude]/(scot %da now.bowl)
++  groups
  ~+  .^([@ =^groups *] (scry %group-store /export/noun))
++  network
  ~+  .^([@ =network:gra] (scry %graph-store /export/noun))
++  associations
  ~+  .^(=associations:met (scry %metadata-store /associations/noun))
++  peers
  |=  =network:gra
  =-  (~(del in -) our.bowl)
  %-  ~(gas in *(set ship))
  (turn ~(tap in ~(key by graphs.network)) head)
++  poke-our
  |=  [=dude:gall =cage]
  [%pass /gladio %agent [our.bowl dude] %poke cage]
++  migrate-start
  ^-  (quip card (set ship))
  =+  network
  =+  groups
  =+  associations
  =/  ships   (peers network)
  ~&  ships/ships
  =/  dms  (~(get by graphs:network) [our.bowl %dm-inbox])
  =/  import  (import-for-mark our.bowl groups associations network)
  =/  clubs  (import-club groups associations network)
  =/  chats=imports:graph:i
    (import %graph-validator-chat)
  =/  diarys=imports:graph:i
    (import %graph-validator-publish)
  =/  links=imports:graph:i
    (import %graph-validator-link)
  =/  =imports:groups:i
    %-  ~(gas by *imports:groups:i)
    %+  murn  ~(tap by groups)
    |=  [=flag:i =group]
    ^-  (unit [_flag import:groups:i])
    ?:  hidden.group
      ~
    ?~  assoc=(~(get by associations) [%groups flag])
      ~&  missing-group-assoc/flag
      ~
    =/  chans=(map flag:i association:met)
      %-  ~(gas by *(map flag:i association:met))
      %+  murn  ~(tap by associations)
      |=  [res=md-resource:met ass=association:met]
      ^-  (unit [flag:i association:met])
      ?.  =(group.ass flag)  ~
      `[resource.res ass]
    =/  roles=(set flag:i)
      %-  ~(gas in *(set flag:i))
      %+  murn  ~(tap by chans)
      |=  [=flag:i =association:met]
      ^-  (unit flag:i)
      ?^  link=(~(get by links) flag)
        ?:  =(writers.u.link ~)  ~
        `flag
      ?^  diary=(~(get by diarys) flag)
        ?:  =(writers.u.diary ~)  ~
        `flag
      ?^  chat=(~(get by chats) flag)
        ?:  =(writers.u.chat ~)  ~
        `flag
      ~
    `[flag u.assoc chans roles group]
  =/  dms  (~(get by graphs:network) [our.bowl %dm-inbox])
  :_  (peers network)
  %-  welp
  :_  (migrate-ship our.bowl)
  :*  (poke-our %groups group-import+!>(imports))
      (poke-our %chat club-imports+!>(clubs))
      ?~  dms  ~
      (poke-our %chat dm-imports+!>(p.u.dms))^~
  ==
::
++  migrate-ship
  |=  her=ship
  ^-  (list card)
  =+  groups
  =+  network
  =+  associations
  =/  import  (import-for-mark her groups associations network)
  =/  chats=imports:graph:i
    (import %graph-validator-chat)
  =/  diarys=imports:graph:i
    (import %graph-validator-publish)
  =/  links=imports:graph:i
    (import %graph-validator-link)
  :~  (poke-our %chat graph-imports+!>(chats))
      (poke-our %diary graph-imports+!>(diarys))
      (poke-our %heap graph-imports+!>(links))
  ==
--
