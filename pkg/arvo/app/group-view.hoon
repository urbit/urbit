/-  view-sur=group-view, group-store, *group, metadata=metadata-store
/+  default-agent, agentio, mdl=metadata, resource, dbug, grpl=group, verb
|%
++  card  card:agent:gall
+$  state-zero
  $:  %0
      joining=(map rid=resource =ship)
  ==
++  view  view-sur
--
=|  state-zero
=*  state  -
::
%-  agent:dbug
%+  verb  &
^-  agent:gall
=<
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    gc    ~(. +> bowl)
    io    ~(. agentio bowl)
++  on-init  
  `this
++  on-save
  !>(state)
::
++  on-load
  |=  =vase
  `this
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?.  ?=(?(%group-view-action %noun) mark)
    (on-poke:def mark vase)
  =+  !<(=action:view vase)
  =^  cards  state
    (jn-start:join:gc +.action)
  [cards this]
::
++  on-watch
  |=  =path
  ?+  path  (on-watch:def path)
      [%all ~]  
    :_  this
    (fact:io group-view-update+!>([%initial ~(key by joining)]) ~)^~
  ==
::
++  on-peek
  |=  =path
  [~ ~]
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  =^  cards  state
    ?+    wire  `state
        [%join %ship @ @ *]
      =/  rid
        (de-path:resource t.wire)
      ?.  (~(has by joining) rid)  `state
      (jn-agent:(jn-abed:join:gc rid) t.t.t.t.wire sign)
    ==
  [cards this]
::
++  on-arvo
  |=  [=wire =sign-arvo]
  `this
::
++  on-leave
  |=  =path
  `this
::
++  on-fail  on-fail:def
--
|_  =bowl:gall
++  met  ~(. mdl bowl)
++  grp  ~(. grpl bowl)
++  io   ~(. agentio bowl)
::
::
++  join
  |_  [rid=resource =ship]
  ++  jn-core  .
  ::
  ++  tx-fact
    |=  =progress:view
    =;  =cage
      (fact:io cage /all tx+(en-path:resource rid) ~)
    group-view-update+!>([%progress rid progress]) 
  ::
  ++  watch-md
    (watch-our:(jn-pass-io /md) %metadata-store /updates)
  ::
  ++  watch-groups
    (watch-our:(jn-pass-io /groups) %group-store /groups)
  ::
  ++  jn-pass-io
    |=  pax=path
    ~(. pass:io (welp join+(en-path:resource rid) pax))
  :: 
  ++  jn-abed
    |=  r=resource
    =/  s=^ship
      (~(got by joining) r)
    jn-core(rid r, ship s)
  ::
  ++  jn-start
    |=  [rid=resource =^ship]
    ^-  (quip card _state)
    ?<  (~(has by joining) rid)
    =.  joining
      (~(put by joining) rid ship)
    =.  jn-core
      (jn-abed rid)
    =/  maybe-group
       (peek-group:met %contacts rid)
    ?^  maybe-group
      ~|("already joined group {<rid>}" !!)
    :_  state
    :~  %+  poke:(jn-pass-io /add)
          [ship %group-push-hook]
        group-update+!>([%add-members rid (silt our.bowl ~)])
        ::
        watch-md
        watch-groups
    ==
  ::
  ++  jn-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _state)
    |^  
    ?+    -.wire  ~|("bad %join wire" !!)
        %add  :: join group
      ?>  ?=(%poke-ack -.sign)
      ?^  p.sign
        (cleanup %no-perms)
      :_  state
      :_  ~
      %+  poke-our:(jn-pass-io /pull-groups)  %group-pull-hook 
      pull-hook-action+!>([%add ship rid])
      ::
        %pull-groups
      ?>  ?=(%poke-ack -.sign)
      ?~  p.sign
        ::  do nothing, wait for update from store
        `state
      ::  shouldn't ever fail
      (cleanup %strange)
      ::
        %groups
      ?+  -.sign  !!
        %fact  (groups-fact +.sign)
        %watch-ack  (ack +.sign)
        %kick  groups-kick
      ==
      ::
        %pull-md
      ?>  ?=(%poke-ack -.sign)
      ?~  p.sign  `state
      (cleanup %strange)
      ::
        %md
      ?+  -.sign  !!
        %fact  (md-fact +.sign)
        %watch-ack  (ack +.sign)
        %kick  md-kick
      ==
      ::
        %pull-graphs
      ?>  ?=(%poke-ack -.sign)
      %-  cleanup
      ?^(p.sign %strange %done)
    ==
    ++  groups-fact
      |=  =cage
      ?.  ?=(%group-update p.cage)
        `state
      =+  !<(=update:group-store q.cage)
      ?.  ?=(%initial-group -.update)
        `state
      ?.  =(rid resource.update)
        `state
      :_  state
      :_  ~
      %+  poke-our:(jn-pass-io /pull-md)  %metadata-push-hook
      pull-hook-action+!>([%add [entity .]:rid])
    ::
    ++  md-fact
      |=  [=mark =vase]
      ?.  ?=(%metadata-update mark)  `state
      =+  !<(=update:metadata vase)
      ?.  ?=(%initial-group -.update)  `state
      ?.  =(group.update rid)  `state
      =^  cards  state
        (cleanup %done)
      :_  state
      %+  welp  cards
      ?.  hidden:(need (scry-group:grp rid))  ~
      %+  murn  ~(tap by associations.update)
      |=  [=md-resource:metadata =association:metadata]
      ?.  =(app-name.md-resource %graph)  ~
      =*  rid  resource.md-resource
      :-  ~
      %+  poke-our:(jn-pass-io /pull-graph)  %graph-pull-hook
      pull-hook-action+!>([%add [entity .]:rid])
    ::
    ++  groups-kick
      :_  state
      watch-groups^~
    ::
    ++  md-kick
      :_  state
      watch-md^~
    ::
    ++  ack
      |=  err=(unit tang)
      ?~  err  `state
      (cleanup %strange)
    ::
    ++  cleanup
      |=  =progress:view
      ^-  (quip card _state)
      :_  state(joining (~(del by joining) rid))
      :~  (leave-our:(jn-pass-io /groups) %group-store)
          (leave-our:(jn-pass-io /md) %metadata-store)
          (tx-fact progress)
      ==
    --
  --
--
