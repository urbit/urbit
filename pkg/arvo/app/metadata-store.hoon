::  metadata-store [landscape]:
::
::  data store for application metadata and mappings
::  between groups and resources within applications
::
::  group-paths are expected to be an existing group path
::  resources are expected to correspond to existing app paths
::
::  note: when scrying for metadata, to make the arguments safe in paths,
::  encode group-path and app-path using (scot %t (spat group-path))
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
::  /metadata/%group-path/%app-name/%app-path      specific metadatum
::  /app-name/%app-name                            associations for app
::  /group/%group-path                             associations for group
::
/-  *metadata-store, *metadata-hook
/+  *metadata-json, default-agent, verb, dbug, resource, *migrate
|%
+$  card  card:agent:gall
+$  base-state-0
  $:  associations=associations-0
      group-indices=(jug group-path md-resource)
      app-indices=(jug app-name [group-path app-path])
      resource-indices=(jug md-resource group-path)
  ==
::
+$  associations-0  (map [group-path md-resource] metadata-0)
::
+$  metadata-0
  $:  title=@t
      description=@t
      color=@ux
      date-created=@da
      creator=@p
  ==
::
+$  base-state-1
  $:  associations=associations
      group-indices=(jug group-path md-resource)
      app-indices=(jug app-name [group-path app-path])
      resource-indices=(jug md-resource group-path)
  ==
::
+$  state-0   [%0 base-state-0]
+$  state-1   [%1 base-state-0]
+$  state-2   [%2 base-state-0]
+$  state-3   [%3 base-state-1]
+$  state-4   [%4 base-state-1]
+$  state-5   [%5 base-state-1]
+$  state-6   [%6 base-state-1]
+$  versioned-state
  $%  state-0
      state-1
      state-2
      state-3
      state-4
      state-5
      state-6
  ==
--
::
=|  state-6
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
  ++  on-save  !>(state)
  ++  on-load
    |=  =vase
    ^-  (quip card _this)
    =/  old  !<(versioned-state vase)
    =|  cards=(list card)
    |^
    ?:  ?=(%6 -.old)
      [cards this(state old)]
    ?:  ?=(%5 -.old)
      =/  =^associations
        (migrate-app-to-graph-store %publish associations.old)
      %_    $
        -.old  %6
        associations.old  associations
      ::
          resource-indices.old
        (rebuild-resource-indices associations)
        ::
          app-indices.old
        (rebuild-app-indices associations)
        ::
          group-indices.old
        (rebuild-group-indices associations)
      ==

    ?:  ?=(%4 -.old)
      %_    $
        -.old  %5
        ::
          resource-indices.old
        (rebuild-resource-indices associations.old)
        ::
          app-indices.old
        (rebuild-app-indices associations.old)
        ::
          group-indices.old
        (rebuild-group-indices associations.old)
      ==
    ?:  ?=(%3 -.old)
      $(old [%4 +.old])
    ?:  ?=(%2 -.old)
      =/  new-state=state-3
        %*  .  *state-3
            associations
          %-  malt
          %+  murn  ~(tap by associations.old)
          |=  [[=group-path =md-resource] m=metadata-0]
          ^-  (unit [[^group-path ^md-resource] metadata])
          ?:  =(app-name.md-resource %link)  ~
          `[[group-path md-resource] (old-md-to-new m)]
        ==
      $(old new-state)
    ?:  ?=(%1 -.old)
      %_   $
        old  [%2 +.old]
      ::
          cards
        %+  murn  ~(tap in ~(key by group-indices.old))
        |=  =group-path
        ^-  (unit card)
        =/  rid  (de-path-soft:resource group-path)
        ?~  rid  ~
        ?:  =(our.bowl entity.u.rid)
          `(poke-md-hook %add-owned group-path)
        `(poke-md-hook %add-synced entity.u.rid group-path)
      ==
    =/  new-state-1=state-1
      %*  .  *state-1
        associations      (migrate-associations associations.old)
        group-indices     (migrate-group-indices group-indices.old)
        app-indices       (migrate-app-indices app-indices.old)
        resource-indices  (migrate-resource-indices resource-indices.old)
      ==
    $(old new-state-1)
    ::
    ++  rebuild-resource-indices
      |=  =^associations
      %-  ~(gas ju *(jug md-resource group-path))
      %+  turn  ~(tap in ~(key by associations))
      |=  [g=group-path r=md-resource]
      ^-  [md-resource group-path]
      [r g] 
    ::
    ++  rebuild-group-indices
      |=  =^associations 
      %-  ~(gas ju *(jug group-path md-resource))
      ~(tap in ~(key by associations))
    ::
    ++  rebuild-app-indices
      |=  =^associations
      %-  ~(gas ju *(jug app-name [group-path app-path]))
      %+  turn  ~(tap in ~(key by associations))
      |=  [g=group-path r=md-resource]
      ^-  [app-name [group-path app-path]]
      [app-name.r [g app-path.r]]

    ::
    ++  migrate-app-to-graph-store
      |=  [app=@tas =^associations]
      ^+  associations
      %-  malt
      %+  turn  ~(tap by associations)
      |=  [[=group-path =md-resource] m=metadata]
      ^-  [[^group-path ^md-resource] metadata]
      ?.  =(app-name.md-resource app)  
        [[group-path md-resource] m]
      =/  new-app-path=path
        ?.  ?=([@ @ ~] app-path.md-resource)
          app-path.md-resource
        ship+app-path.md-resource
      [[group-path [%graph new-app-path]] m(module app)]
    ::
    ++  poke-md-hook
      |=  act=metadata-hook-action
      ^-  card
      =/  =cage  metadata-hook-action+!>(act)
      [%pass / %agent [our.bowl %metadata-hook] %poke cage]
    ::
    ++  new-group-path
      |=  =group-path
      ship+(new-app-path group-path)
    ::
    ++  new-app-path
      |=  =app-path
      ^-  path
      ?>  ?=(^ app-path)
      ?:(=('~' i.app-path) t.app-path app-path)
    ::
    ++  old-md-to-new
      |=  m=metadata-0
      ^-  metadata
      %*  .  *metadata
          title         title.m
          description   description.m
          color         color.m
          date-created  date-created.m
          creator       creator.m
          module        *term
      ==
    ::
    ++  migrate-md-resource
      |=  md-resource
      ^-  md-resource
      ?:  =(%chat app-name)      [%chat (new-app-path app-path)]
      ?:  =(%contacts app-name)  [%contacts ship+app-path]
      [app-name app-path]
    ::
    ++  migrate-resource-indices
      |=  resource-indices=(jug md-resource group-path)
      ^-  (jug md-resource group-path)
      %-  malt
      %+  turn  ~(tap by resource-indices)
      |=  [=md-resource paths=(set group-path)]
      :-  (migrate-md-resource md-resource)
      (~(run in paths) new-group-path)
    ::
    ++  migrate-app-indices
      |=  app-indices=(jug app-name [group-path app-path])
      %-  malt
      %+  turn  ~(tap by app-indices)
      |=  [app=term indices=(set [=group-path =app-path])]
      :-  app
      %-  ~(run in indices)
      |=  [=group-path =app-path]
      :-  (new-group-path group-path)
      ?:  =(%chat app)      (new-app-path app-path)
      ?:  =(%contacts app)  ship+app-path
      app-path
    ::
    ++  migrate-group-indices
      |=  group-indices=(jug group-path md-resource)
      %-  malt
      %+  turn  ~(tap by group-indices)
      |=  [=group-path resources=(set md-resource)]
      :-  (new-group-path group-path)
      %-  sy
      %+  turn  ~(tap in resources)
      migrate-md-resource
    ::
    ++  migrate-associations
      |=  associations=associations-0
      %-  malt
      %+  turn  ~(tap by associations)
      |=  [[g=group-path r=md-resource] m=metadata-0]
      :_  m
      [(new-group-path g) (migrate-md-resource r)]
    --
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
          %metadata-action
        (poke-metadata-action:mc !<(metadata-action vase))
      ::
          %noun
        =/  val=(each [%cleanup path] tang)
          (mule |.(!<([%cleanup path] vase)))
        ?.  ?=(%& -.val)
          (on-poke:def mark vase)
        =/  group=path  +.p.val
        =/  res=(set md-resource)  (~(get ju group-indices) group)
        =.  group-indices  (~(del by group-indices) group)
        :-  ~
        %+  roll  ~(tap in res)
        |=  [r=md-resource out=_state]
        =:  resource-indices.out  (~(del by resource-indices.out) r)
            associations.out  (~(del by associations.out) group r)
            app-indices.out
          %-  ~(del ju app-indices.out)
          [app-name.r group app-path.r]
        ==
        out
      ::
          %import
        (poke-import:mc q.vase)
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    |^
    =/  cards=(list card)
      ?+  path  (on-watch:def path)
          [%all ~]
        (give %metadata-update !>([%associations associations]))
      ::
          [%updates ~]
        ~
      ::
          [%app-name @ ~]
        =/  =app-name  i.t.path
        =/  app-indices  (metadata-for-app:mc app-name)
        (give %metadata-update !>([%associations app-indices]))
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
        [%y %group-indices ~]     ``noun+!>(group-indices)
        [%y %app-indices ~]       ``noun+!>(app-indices)
        [%y %resource-indices ~]  ``noun+!>(resource-indices)
        [%x %associations ~]      ``noun+!>(associations)
        [%x %app-name @ ~]
      =/  =app-name  i.t.t.path
      ``noun+!>((metadata-for-app:mc app-name))
    ::
        [%x %group *]
      =/  =group-path  t.t.path
      ``noun+!>((metadata-for-group:mc group-path))
    ::
        [%x %metadata @ @ @ ~]
      =/  =group-path   (stab (slav %t i.t.t.path))
      =/  =md-resource  [`term`i.t.t.t.path (stab (slav %t i.t.t.t.t.path))]
      ``noun+!>((~(get by associations) [group-path md-resource]))
    ::
        [%x %resource @ *]
      =/  app=term        i.t.t.path
      =/  app-path=^path  t.t.t.path
      ``noun+!>((~(get by resource-indices) app app-path))
    ::
        [%x %export ~]
      ``noun+!>(state)
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  poke-metadata-action
  |=  act=metadata-action
  ^-  (quip card _state)
  ?>  (team:title our.bowl src.bowl)
  ?-  -.act
      %add     (handle-add group-path.act resource.act metadata.act)
      %remove  (handle-remove group-path.act resource.act)
  ==
::
++  poke-import
  |=  arc=*
  ^-  (quip card _state)
  |^
  =/  sty=state-6
    [%6 (remake-metadata ;;(tree-metadata +.arc))]
  [~ sty]
  ::
  +$  tree-metadata
    $:  associations=(tree [[group-path md-resource] metadata])
        group-indices=(tree [group-path (tree md-resource)])
        app-indices=(tree [app-name (tree [group-path app-path])])
        resource-indices=(tree [md-resource (tree group-path)])
    ==
  ::
  ++  remake-metadata
    |=  tm=tree-metadata
    ^-  base-state-1
    :*  (remake-map associations.tm)
        (remake-jug group-indices.tm)
        (remake-jug app-indices.tm)
        (remake-jug resource-indices.tm)
    ==
  --
::
++  handle-add
  |=  [=group-path =md-resource =metadata]
  ^-  (quip card _state)
  :-  %+  send-diff  app-name.md-resource
      ?:  (~(has by resource-indices) md-resource)
        [%update-metadata group-path md-resource metadata]
      [%add group-path md-resource metadata]
  %=  state
      associations
    (~(put by associations) [group-path md-resource] metadata)
  ::
      group-indices
    (~(put ju group-indices) group-path md-resource)
  ::
      app-indices
    %+  ~(put ju app-indices)
      app-name.md-resource
    [group-path app-path.md-resource]
  ::
      resource-indices
    (~(put ju resource-indices) md-resource group-path)
  ==
::
++  handle-remove
  |=  [=group-path =md-resource]
  ^-  (quip card _state)
  :-  (send-diff app-name.md-resource [%remove group-path md-resource])
  %=  state
      associations
    (~(del by associations) [group-path md-resource])
  ::
      group-indices
    (~(del ju group-indices) group-path md-resource)
  ::
      app-indices
    %+  ~(del ju app-indices)
      app-name.md-resource
    [group-path app-path.md-resource]
  ::
      resource-indices
    (~(del ju resource-indices) md-resource group-path)
  ==
::
++  metadata-for-app
  |=  =app-name
  ^-  ^associations
  %-  ~(gas by *^associations)
  %+  turn  ~(tap in (~(gut by app-indices) app-name ~))
  |=  [=group-path =app-path]
  :-  [group-path [app-name app-path]]
  (~(got by associations) [group-path [app-name app-path]])
::
++  metadata-for-group
  |=  =group-path
  ^-  ^associations
  %-  ~(gas by *^associations)
  %+  turn  ~(tap in (~(gut by group-indices) group-path ~))
  |=  =md-resource
  :-  [group-path md-resource]
  (~(got by associations) [group-path md-resource])
::
++  send-diff
  |=  [=app-name upd=metadata-update]
  ^-  (list card)
  |^
  %-  zing
  :~  (update-subscribers /all upd)
      (update-subscribers /updates upd)
      (update-subscribers [%app-name app-name ~] upd)
  ==
  ::
  ++  update-subscribers
    |=  [pax=path upd=metadata-update]
    ^-  (list card)
    [%give %fact ~[pax] %metadata-update !>(upd)]~
  --
--
