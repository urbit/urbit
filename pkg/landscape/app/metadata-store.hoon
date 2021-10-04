::  metadata-store [landscape]:
::
::  data store for application metadata and mappings
::  between groups and resources within applications
::
::  paths are expected to be an existing group path
::  resources are expected to correspond to existing app paths
::
::  note: when scrying for metadata, to make the arguments safe in paths,
::  encode path and path using (scot %t (spat path))
::
::  +watch paths:
::  /all                                  associations + updates
::  /updates                              just updates
::  /app-name/%app-name                   specific app's associations + updates
::
::  +peek paths:
::  /associations                                  all associations
::  /group-indices                                 all group indices
::  /app-indices                                   all app indices
::  /resource-indices                              all resource indices
::  /metadata/%path/%app-name/%path      specific metadatum
::  /app-name/%app-name                            associations for app
::  /group/%path                             associations for group
::
/-  store=metadata-store, pull-hook
/+  default-agent, verb, dbug, resource, *migrate, lib=metadata-store
|%
+$  card  card:agent:gall
+$  base-state-0
  $:  associations=associations-0
      group-indices=(jug path md-resource:store)
      app-indices=(jug app-name:store [path path])
      resource-indices=(jug md-resource:store path)
  ==
::
+$  associations-0  (map [path md-resource:store] metadata-0)
::
+$  metadata-0
  $:  title=@t
      description=@t
      color=@ux
      date-created=@da
      creator=@p
  ==
::
+$  metadata-1
  $:  title=@t
      description=@t
      color=@ux
      date-created=@da
      creator=@p
      module=term
  ==
::
+$  md-resource-1   [=app-name:store =path]
::
+$  associations-1  (map [path md-resource-1] metadata-1)
::
+$  base-state-1
  $:  associations=associations-1
      group-indices=(jug path md-resource-1)
      app-indices=(jug app-name:store [path path])
      resource-indices=(jug md-resource-1 path)
  ==
::
+$  metadatum-2
  $:  title=cord
      description=cord
      =color:store
      date-created=time
      creator=ship
      module=term
      picture=url:store
      preview=?
      vip=vip-metadata:store
  ==
::
+$  association-2   [group=resource =metadatum-2]
+$  associations-2  (map md-resource:store association-2)
::
+$  cached-indices
  $:  group-indices=(jug resource md-resource:store)
      app-indices=(jug app-name:store [group=resource =resource])
      resource-indices=(map md-resource:store resource)
  ==
::
+$  base-state-2
  $:  associations=associations-2
      ~
  ==
::
+$  base-state-3
  $:  =associations:store
      ~
  ==
::
+$  state-0    [%0 base-state-0]
+$  state-1    [%1 base-state-0]
+$  state-2    [%2 base-state-0]
+$  state-3    [%3 base-state-1]
+$  state-4    [%4 base-state-1]
+$  state-5    [%5 base-state-1]
+$  state-6    [%6 base-state-1]
+$  state-7    [%7 base-state-2]
+$  state-8    [%8 base-state-3]
+$  state-9    [%9 base-state-3]
+$  state-10   [%10 base-state-3]
+$  state-11   [%11 base-state-3]
+$  state-12   [%12 base-state-3]
+$  versioned-state
  $%  state-0
      state-1
      state-2
      state-3
      state-4
      state-5
      state-6
      state-7
      state-8
      state-9
      state-10
      state-11
      state-12
  ==
::
+$  inflated-state
  $:  state-12
      cached-indices
  ==
--
::
=|  inflated-state
=*  state  -
%+  verb  |
%-  agent:dbug
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      mc    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init  on-init:def
  ++  on-save  !>(-.state)
  ++  on-load
    |=  =vase
    ^-  (quip card _this)
    =^  cards  state
      (on-load:mc vase)
    [cards this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title [our src]:bowl)
    |^
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
          ?(%metadata-action %metadata-update-2)
        (poke-metadata-update !<(update:store vase))
      ::
          %import
        (poke-import q.vase)
      ::
        %noun  ~&  +.state  `state
      ==
    [cards this]
    ::
    ++  poke-metadata-update
      |=  upd=update:store
      ^-  (quip card _state)
      |^
      ?+  -.upd  ~|(%bad-poke !!)
          %add     (handle-add +.upd)
          %remove  (handle-remove +.upd)
          %edit    (handle-edit +.upd)
          %initial-group  (handle-initial-group +.upd)
      ==
      ::
      ++  handle-add
        |=  [group=resource =md-resource:store =metadatum:store]
        ^-  (quip card _state)
        :-  %-  send-diff:mc
            [%add group md-resource metadatum]
        %=  state
            associations
          (~(put by associations) md-resource [group metadatum])
        ::
            app-indices
          %+  ~(put ju app-indices)
            app-name.md-resource
          [group resource.md-resource]
        ::
            resource-indices
          (~(put by resource-indices) md-resource group)
        ::
            group-indices
          (~(put ju group-indices) group md-resource)
        ==
      ::
      ++  handle-edit
        |=  [group=resource =md-resource:store =edit-field:store]
        ^-  (quip card _state)
        =/  [new-group=resource =metadatum:store]
          ~|  %no-assoc-for-edit
          (~(got by associations) md-resource)
        ~|  %cant-reassign-groups
        ?>  =(new-group group)
        =.  metadatum
          ?-  -.edit-field
            %title        metadatum(title title.edit-field)
            %description  metadatum(description description.edit-field)
            %color        metadatum(color color.edit-field)
            %picture      metadatum(picture url.edit-field)
            %hidden       metadatum(hidden hidden.edit-field)
            %preview      metadatum(preview preview.edit-field)
            %vip          metadatum(vip vip.edit-field)
          ==
        :-  (send-diff:mc %add group md-resource metadatum)
        %_  state
          associations  (~(put by associations) md-resource group metadatum)
        ==
      ::
      ++  handle-remove
        |=  [group=resource =md-resource:store]
        ^-  (quip card _state)
        :-  (send-diff:mc [%remove group md-resource])
        %=  state
            associations
          (~(del by associations) md-resource)
        ::
            app-indices
          %+  ~(del ju app-indices)
            app-name.md-resource
          [group resource.md-resource]
        ::
            resource-indices
          (~(del by resource-indices) md-resource)
        ::
            group-indices
          (~(del ju group-indices) group md-resource)
        ==
      ::
      ++  handle-initial-group
        |=  [group=resource =associations:store]
        =/  assocs=(list [=md-resource:store grp=resource =metadatum:store])
          ~(tap by associations)
        =/  old-resources=(set md-resource:store)  (~(get ju group-indices) group)
        =/  cur-resources=(set md-resource:store)  ~(key by associations)
        =.  state   (remove-gone group (~(dif in old-resources) cur-resources))
        |-
        ?~  assocs
          :_  state
          (send-diff:mc %initial-group group associations)
        =,  assocs
        ?>  =(group grp.i)
        =^  cards  state
          (handle-add group [md-resource metadatum]:i)
        $(assocs t)
      ::
      ++  remove-gone
        |=  [group=resource gone=(set md-resource:store)]
        ^+  state
        %+  roll  ~(tap in gone)
        |=  [=md-resource:store out=_state]
        %_  out
          associations      (~(del by associations.out) md-resource)
          group-indices     (~(del ju group-indices.out) group md-resource)
          resource-indices  (~(del by resource-indices.out) md-resource)
          ::
            app-indices
          (~(del ju app-indices.out) app-name.md-resource group resource.md-resource)
        ==
      --
    ::
    ++  poke-import
      |=  arc=*
      ^-  (quip card _state)
      |^
      =^  cards  state
        (on-load:mc !>([%9 (remake-metadata ;;(tree-metadata +.arc))]))
      :_  state
      %+  weld  cards
      %+  turn  ~(tap in ~(key by group-indices))
      |=  rid=resource
      %-  poke-our
      ?:  =(entity.rid our.bowl)
        :-  %metadata-push-hook
        push-hook-action+!>([%add rid])
      :-  %metadata-pull-hook
      pull-hook-action+!>([%add [entity .]:rid])
      ::
      ++  poke-our
        |=  [app=term =cage]
        ^-  card
        [%pass / %agent [our.bowl app] %poke cage]
      ::
      +$  tree-metadata
        $:  associations=(tree [md-resource:store [resource metadatum:store]])
            ~
        ==
      ::
      ++  remake-metadata
        |=  tm=tree-metadata
        ^-  base-state-3
        :*  (remake-map associations.tm)
            ~
        ==
      --
    --
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    |^
    =/  cards=(list card)
      ?+  path  (on-watch:def path)
          [%all ~]
        (give %metadata-update-2 !>([%associations associations]))
      ::
          [%updates ~]
        ~
      ::
          [%app-name @ ~]
        =/  =app-name:store  i.t.path
        =/  app-indices  (metadata-for-app:mc app-name)
        (give %metadata-update-2 !>([%associations app-indices]))
      ==
    [cards this]
    ::
    ++  give
      |=  =cage
      ^-  (list card)
      [%give %fact ~ cage]~
    --
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def path)
        [%y %group-indices ~]
      ``noun+!>(`(jug resource md-resource:store)`group-indices)
    ::
        [%y %app-indices ~]
      ``noun+!>(`(jug app-name:store [group=resource =resource])`app-indices)
    ::
        [%y %resource-indices ~]
      ``noun+!>(`(map md-resource:store resource)`resource-indices)
    ::
        [%x %associations ~]
      ``noun+!>(`associations:store`associations)
    ::
        [%x %app-name @ ~]
      =/  =app-name:store  i.t.t.path
      ``noun+!>(`associations:store`(metadata-for-app:mc app-name))
    ::
        [%x %group *]
      =/  group=resource  (de-path:resource t.t.path)
      ``noun+!>(`associations:store`(metadata-for-group:mc group))
    ::
        [%x %metadata @ @ @ @ ~]
      =/  =md-resource:store
        [i.t.t.path (de-path:resource t.t.t.path)]
      ``noun+!>(`(unit association:store)`(~(get by associations) md-resource))
    ::
        [%x %metadata-json @ @ @ @ ~]
      =/  =md-resource:store
        [i.t.t.path (de-path:resource t.t.t.path)]
      =/  assoc=(unit association:store)  (~(get by associations) md-resource)
      ?~  assoc  ~
      =/  =json
        %-  pairs:enjs:format
        :~  group+s+(enjs-path:resource group.u.assoc)
            metadatum+(metadatum:enjs:lib metadatum.u.assoc)
        ==
      ``json+!>(json)
    ::
        [%x %resource @ *]
      =/  app=term        i.t.t.path
      =/  rid=resource    (de-path:resource t.t.t.path)
      ``noun+!>(`(unit resource)`(~(get by resource-indices) [app rid]))
    ::
        [%x %export ~]
      ``noun+!>(-.state)
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
::
++  on-load
  |=  =vase
  ^-  (quip card _state)
  =/  old  !<(versioned-state vase)
  =|  cards=(list card)
  |^
  =*  loop  $
  ?:  ?=(%12 -.old)
    :-  cards
    %_  state
      associations      associations.old
      resource-indices  (rebuild-resource-indices associations.old)
      group-indices     (rebuild-group-indices associations.old)
      app-indices       (rebuild-app-indices associations.old)
    ==
  ?:  ?=(%11 -.old)
    $(-.old %12, associations.old (reset-group-hidden associations.old))
  ?:  ?=(%10 -.old)
    $(-.old %11, associations.old (hide-dm-assoc associations.old))
  ?:  ?=(%9 -.old)
    =/  groups  
      (fall (~(get by (rebuild-app-indices associations.old)) %groups) ~)
    =/  pokes=(list card)
      %+  murn  ~(tap in ~(key by groups))
      |=  group=resource
      ^-  (unit card)
      =/  =association:store  (~(got by associations.old) [%groups group])
      =*  met  metadatum.association
      ?.  ?=([%group [~ [~ [@ [@ @]]]]] config.met)
        ~
      =*  res  resource.u.u.feed.config.met
      ?:  =(our.bowl entity.res)  ~
      =-  `[%pass /fix-feed %agent [our.bowl %graph-pull-hook] %poke -]
      :-  %pull-hook-action
      !>  ^-  action:pull-hook
      [%add entity.res res]
    %_  $
      cards  (weld cards pokes)
      -.old  %10
    ==
  ?:  ?=(%8 -.old)
    $(-.old %9)
  ?:  ?=(%7 -.old)
    $(old [%8 (associations-2-to-3 associations.old) ~])
  ?:  ?=(%6 -.old)
    =/  old-assoc=associations-1
      (migrate-app-to-graph-store %chat associations.old)
    $(old [%7 (associations-1-to-2 old-assoc) ~])
  ::
  ?:  ?=(%5 -.old)
    =/  associations=associations-1
      (migrate-app-to-graph-store %publish associations.old)
    %_    $
      -.old  %6
      associations.old  associations
    ==
  ::  pre-breach, can safely throw away
  loop(old *state-8)
  ::
  ++  reset-group-hidden
    |=  assoc=associations:store
    ^-  associations:store
    %-  ~(gas by *associations:store)
    %+  turn  ~(tap by assoc)
    |=  [m=md-resource:store [g=resource met=metadatum:store]]
    ^-  [md-resource:store association:store]
    =?  hidden.met  ?=(%groups app-name.m)
      %.n
    [m [g met]]
  ::
  ++  hide-dm-assoc
    |=  assoc=associations:store
    ^-  associations:store
    %-  ~(gas by *associations:store)
    %+  turn  ~(tap by assoc)
    |=  [m=md-resource:store [g=resource met=metadatum:store]]
    ^-  [md-resource:store association:store]
    =?    hidden.met
        ?&  ?=(^ (rush name.resource.m ;~(pfix (jest 'dm--') fed:ag)))  
            ?=(%graph app-name.m)
        ==
      %.y
    [m [g met]]
  ::
  ++  associations-2-to-3
    |=  assoc=associations-2
    ^-  associations:store
    %-  ~(gas by *associations:store)
    %+  turn  ~(tap by assoc)
    |=  [m=md-resource:store [g=resource met=metadatum-2]]
    [m [g (metadatum-2-to-3 met)]]
  ::
  ++  metadatum-2-to-3
    |=  m=metadatum-2
    %*  .  *metadatum:store
      title         title.m
      description   description.m
      color         color.m
      date-created  date-created.m
      creator       creator.m
      preview       preview.m
      hidden        %|
    ::
        config
      ?:  =(module.m %$)
        [%group ~]
      [%graph module.m]
    ==
  ::
  ++  associations-1-to-2
    |=  assoc=associations-1
    ^-  associations-2
    %-  ~(gas by *associations-2)
    %+  murn
      ~(tap by assoc)
    |=  [[group=path m=md-resource-1] met=metadata-1]
    %+  biff  (de-path-soft:resource group)
    |=  g=resource
    %+  bind  (md-resource-1-to-2 m)
    |=  =md-resource:store 
    [md-resource g (metadata-1-to-2 met)]
  ::
  ++  md-resource-1-to-2
    |=  m=md-resource-1
    ^-  (unit md-resource:store)
    %+  bind  (de-path-soft:resource path.m)
    |=  rid=resource
    :_  rid
    ?:  =(%contacts app-name.m)   %groups
    app-name.m
  ::
  ++  metadata-1-to-2
    |=  m=metadata-1
    %*  .  *metadatum-2
      title         title.m
      description   description.m
      color         color.m
      date-created  date-created.m
      creator       creator.m
      module        module.m
      preview       %.n
    ==
  ::
  ++  rebuild-resource-indices
    |=  =associations:store
    %-  ~(gas by *(map md-resource:store resource))
    %+  turn  ~(tap by associations)
    |=  [r=md-resource:store g=resource =metadatum:store]
    [r g] 
  ::
  ++  rebuild-group-indices
    |=  =associations:store
    %-  ~(gas ju *(jug resource md-resource:store))
    %+  turn
      ~(tap by associations)
    |=  [r=md-resource:store g=resource =metadatum:store]
    [g r]
  ::
  ++  rebuild-app-indices
    |=  =associations:store
    %-  ~(gas ju *(jug app-name:store [group=resource resource]))
    %+  turn  ~(tap by associations)
    |=  [r=md-resource:store g=resource =metadatum:store]
    [app-name.r g resource.r]
  ::
  ++  migrate-app-to-graph-store
    |=  [app=@tas associations=associations-1]
    ^-  associations-1
    %-  malt
    %+  turn  ~(tap by associations)
    |=  [[=path md-resource=md-resource-1] m=metadata-1]
    ^-  [[^path md-resource-1] metadata-1]
    ?.  =(app-name.md-resource app)  
      [[path md-resource] m]
    =/  new-path=^path
      ?.  ?=([@ @ ~] path.md-resource)
        path.md-resource
      ship+path.md-resource
    [[path [%graph new-path]] m(module app)]
  --
++  metadata-for-app
  |=  =app-name:store
  ^-  associations:store
  %+  roll  ~(tap in (~(gut by app-indices) app-name ~))
  |=  [[group=resource rid=resource] out=associations:store]
  =/  =md-resource:store
    [app-name rid]
  =/  [resource =metadatum:store]
    (~(got by associations) md-resource)
  (~(put by out) md-resource [group metadatum])
::
++  metadata-for-group
  |=  group=resource
  ^-  associations:store
  =/  resources=(set md-resource:store)
    (~(get ju group-indices) group)
  %+  roll
    ~(tap in resources)
  |=  [=md-resource:store out=associations:store]
  =/  [resource =metadatum:store]
    (~(got by associations) md-resource)
  (~(put by out) md-resource [group metadatum])
::
++  send-diff
  |=  =update:store
  ^-  (list card)
  |^
  %-  zing
  :~  (update-subscribers /all update)
      (update-subscribers /updates update)
  ==
  ::
  ++  update-subscribers
    |=  [pax=path =update:store]
    ^-  (list card)
    [%give %fact ~[pax] %metadata-update-2 !>(update)]~
  --
--
