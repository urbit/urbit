:: Migrate scripts
/-  spider
/-  gra=graph-store
/-  met=metadata-store
/-  grp=group-store
/-  i=migrate
/-  *group
/+  strandio
=,  strand=strand:spider
|%
++  import-for-mark
  |=  [=groups =associations:met =network:gra]
  |=  =mark
  ^-  imports:graph:i
  %-  ~(gas by *imports:graph:i)
  %+  murn  ~(tap by graphs.network)
  |=  [=flag:i graph=graph:gra mar=(unit ^mark)]
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
::
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  [%2 =groups]  bind:m
  (scry:strandio ,[%2 =groups] /gx/group-store/export/noun)
;<  [%6 =network:gra]  bind:m
  (scry:strandio ,[%6 =network:gra] /gx/graph-store/export/noun)
;<  =associations:met  bind:m
  (scry:strandio ,associations:met /gx/metadata-store/associations/noun)
=/  import  (import-for-mark groups associations network)
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
;<  ~  bind:m  (poke-our:strandio %groups group-import+!>(imports))
;<  ~  bind:m  (poke-our:strandio %chat graph-imports+!>(chats))
;<  ~  bind:m  (poke-our:strandio %diary graph-imports+!>(diarys))
;<  ~  bind:m  (poke-our:strandio %heap graph-imports+!>(links))
;<  ~  bind:m  
  ?~  dms=(~(get by graphs.network) [our.bowl %dm-inbox])
    (pure:(strand ,~) ~)
  (poke-our:strandio %chat %dm-imports !>(p.u.dms))
(pure:m *vase)
