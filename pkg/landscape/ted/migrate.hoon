:: Migrate scripts
/-  spider
/-  gra=graph-store
/-  met=metadata-store
/-  grp=group-store
/-  i=migrate
/-  *group
/+  strandio
=,  strand=strand:spider
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
=/  =imports:chat:i
  %-  ~(gas by *imports:chat:i)
  %+  murn  ~(tap by graphs.network)
  |=  [=flag:chat:i graph=graph:gra mar=(unit mark)]
  ?.  =(mar `%graph-validator-chat)  ::  XX: correct detection?
    ~
  ?~  assoc=(~(get by associations) [%graph flag])
    ~&  missing-assoc/flag
    ~
  ?~  group=(~(get by groups) group.u.assoc)
    ~&  missing-group/[flag group.u.assoc]
    ~
  =/  writers=(set ship)  (~(get ju tags.u.group) %graph flag %writers)
  ?~  log=(~(get by update-logs.network) flag)
    ~&  missing-log/flag  :: XX: doesn't need to fail, but suspect case
    ~
  `[flag writers u.assoc u.log graph]
;<  ~  bind:m  (poke-our:strandio %chat %graph-imports !>(imports))
(pure:m *vase)
