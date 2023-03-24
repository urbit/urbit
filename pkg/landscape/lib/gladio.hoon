:: Migrate scripts
/-  gra=graph-store
/-  met=metadata-store
/-  grp=group-store
/-  i=migrate
/-  *group
/+  res=resource
|_  =bowl:gall
+$  card  card:agent:gall
::  if false, indicates that OTA should be done in one go, in order to
::  allow for testing on partial testnets
++  split-ota   &
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
++  import-flags
  |=  [our=ship =^groups =associations:met =network:gra]
  |=  =mark
  ^-  (set flag:i)
  %-  ~(gas in *(set flag:i))
  %+  skim
    ~(tap in ~(key by ((import-for-mark ~ groups associations network) mark)))
  |=  =flag:i
  !=(our p.flag)
::
++  import-for-mark
  |=  [her=(unit ship) =^groups =associations:met =network:gra]
  |=  =mark
  ^-  imports:graph:i
  %-  ~(gas by *imports:graph:i)
  %+  murn  ~(tap by graphs.network)
  |=  [=flag:i graph=graph:gra mar=(unit ^mark)]
  ?.  |(=(`p.flag her) =(her ~))
    ~
  ?.  =(mar `mark)  ::  XX: correct detection?
    ~
  ?~  assoc=(~(get by associations) [%graph flag])
    ~&  missing-assoc/flag^mark
    ~
  ?~  group=(~(get by groups) group.u.assoc)
    ~&  missing-group/[flag group.u.assoc]
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
++  groups-raw
  .^(* (scry %group-store /export/noun))
++  network
  ~+  .^([@ =network:gra] (scry %graph-store /export/noun))
++  network-raw
  .^(* (scry %graph-store /export/noun))
++  associations
  ~+  .^([@ =associations:met ~] (scry %metadata-store /export/noun))
++  my-channels-associations
  =/  assoc
    .^(associations:met %gx [(scot %p our.bowl) %metadata-store (scot %da now.bowl) %associations %noun ~])
  %-  ~(gas by *associations:met)
  %+  skim
      ~(tap by assoc)
    |=  [m=md-resource:met [g=resource:res metdat=metadatum:met]]
    ?+  config.metdat  %.n
        [%graph @]
      ?&  =(| hidden.metdat)
          =(name.g name.resource.m)
          =(entity.g our.bowl)
          !=(%chat module.config.metdat)
      ==
    ==
++  associations-raw
  .^(* (scry %metadata-store /export/noun))
++  export
  %-  jam
  ^-  *
  :~  [%group-store groups-raw]
      [%metadata-store associations-raw]
  ==
++  peers
  |=  =network:gra
  =-  (~(del in -) our.bowl)
  %-  ~(gas in *(set ship))
  (turn ~(tap in ~(key by graphs.network)) head)
++  poke-our
  |=  [=dude:gall =cage]
  [%pass /gladio %agent [our.bowl dude] %poke cage]
++  migrate-start
  |=  wait=(set ship)
  ^-  (quip card (set ship))
  =+  network
  =+  associations
  =+  groups
  =/  ships   (peers network)
  =/  dms  (~(get by graphs:network) [our.bowl %dm-inbox])
  =/  import  (import-for-mark `our.bowl groups associations network)
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
  =/  flag-importer  (import-flags our.bowl groups associations network)
  =+  :*  chat-flags=(flag-importer %graph-validator-chat)
          heap-flags=(flag-importer %graph-validator-link)
          diary-flags=(flag-importer %graph-validator-publish)
      ==
  =/  setup=(list card)
    %+  welp  (migrate-ship our.bowl)
    :*  (poke-our %groups group-import+!>(imports))
        (poke-our %chat import-flags+!>(chat-flags))
        (poke-our %heap import-flags+!>(heap-flags))
        (poke-our %diary import-flags+!>(diary-flags))
        (poke-our %chat club-imports+!>(clubs))
        ?~  dms  ~
        (poke-our %chat dm-imports+!>(p.u.dms))^~
    ==
  ?.  split-ota
    :_  ~
    (welp setup (zing (turn ~(tap in (~(del in ships) our.bowl)) migrate-ship)))
  [setup (~(uni in ships) wait)]
++  migrate-my-channels
  |=  wait=(set ship)
  ^-  (quip card (set ship))
  =+  network
  =+  groups
  =+  associations
  =/  =flag:i  [our.bowl %my-channels]
  =/  ships  (peers network)
  =/  import  (import-for-mark `our.bowl groups my-channels-associations network)
  =/  chats=imports:graph:i
    (import %graph-validator-chat)
  =/  diarys=imports:graph:i
    (import %graph-validator-publish)
  =/  links=imports:graph:i
    (import %graph-validator-link)
  =|  assoc=association:met
  =/  hoops=imports:groups:i
    %-  ~(gas by *imports:groups:i)
    %+  murn  ~(tap by groups)
    |=  [=flag:i =group]
    ^-  (unit [_flag import:groups:i])
    =/  nass=(unit association:met)
      ?^  (~(get by associations) [%groups flag])  ~
      `assoc(hidden.metadatum |, title.metadatum 'My Channels', creator.metadatum p.flag, group [our.bowl %my-channels])
    ?~  nass  ~
    =/  chans=(map flag:i association:met)
      %-  ~(gas by *(map flag:i association:met))
      %+  murn  ~(tap by associations)
      |=  [res=md-resource:met ass=association:met]
      ^-  (unit [flag:i association:met])
      ?.  =(group.ass flag)  ~
      ?.  =(entity.group.ass our.bowl)  ~
      `[resource.res ass]
    =/  roles=(set flag:i)
      %-  ~(gas in *(set flag:i))
      %+  murn  ~(tap by chans)
      |=  [=flag:i =association:met]
      ^-  (unit flag:i)
      ?.  =(group.association flag)  ~
      ?.  =(entity.group.association our.bowl)  ~
      ?^  link=(~(get by links) flag)
        ?:  =(writers.u.link ~)  ~
        `flag
      ?^  diary=(~(get by links) flag)
        ?:  =(writers.u.diary ~)  ~
        `flag
      ?^  chat=(~(get by chats) flag)
        ?:  =(writers.u.chat ~)  ~
        `flag
      ~
    ?~  chans  ~
    `[flag u.nass chans roles group]
  =/  bigport=import:groups:i
    %+  roll  ~(val by hoops)
    |=  [port=import:groups:i newport=import:groups:i]
    ^-  import:groups:i
    ::  they should have the same association and group already
    :*  association.port
        (~(uni by chans.port) chans.newport)
        (~(uni by roles.port) roles.newport)
        group.port
    ==
  ?~  chans.bigport
    [~ ~]
  =/  mychan-import=imports:groups:i
    (~(put by *imports:groups:i) [our.bowl %my-channels] bigport)
  :_  ~
  :~  (poke-our %groups group-import+!>(mychan-import))
      (poke-our %chat graph-imports+!>(chats))
      (poke-our %heap graph-imports+!>(links))
      (poke-our %diary graph-imports+!>(diarys))
  ==
++  migrate-ship
  |=  her=ship
  ^-  (list card)
  =+  groups
  =+  network
  =+  associations
  =/  import  (import-for-mark `her groups associations network)
  =/  chats=imports:graph:i
    (import %graph-validator-chat)
  =/  diarys=imports:graph:i
    (import %graph-validator-publish)
  =/  links=imports:graph:i
    (import %graph-validator-link)
  =/  graph-flags
    %.  ~(key by links)
    =-  ~(uni in -)
    (~(uni in ~(key by chats)) ~(key by diarys))
  %+  welp
    %+  turn  ~(tap in graph-flags)
    |=  =flag:i
    ^-  card
    (poke-our %graph-store migrated+!>(flag))
  :~  (poke-our %chat graph-imports+!>(chats))
      (poke-our %diary graph-imports+!>(diarys))
      (poke-our %heap graph-imports+!>(links))
  ==
--
