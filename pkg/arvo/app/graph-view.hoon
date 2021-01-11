/-  view-sur=graph-view, group-store, *group, *metadata-store
/+  default-agent, agentio, mdl=metadata, resource, dbug
|%
++  card  card:agent:gall
+$  state-zero
  $:  %0
      joining=(map @uv [rid=resource =ship])
  ==
--
=|  state-zero
=*  state  -
::
%-  agent:dbug
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
  ?.  ?=(%graph-view-action mark)
    (on-poke:def mark vase)
  =+  !<(=action:view-sur vase)
  =^  cards  state
    ?+    -.action  `state
        %join
      (jn-start:join:gc +.action)
    ==
  [cards this]
::
++  on-watch
  |=  =path
  `this
::
++  on-peek
  |=  =path
  [~ ~]
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  =^  cards  state
    ?+    wire  `state
        [%join @ *]
      =/  uid=@uv
        (slav %uv i.t.wire)
      (jn-agent:(jn-abed:join:gc uid) t.t.wire sign)
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
++  on-fail
  |=  [=term =tang]
  `this
--
|_  =bowl:gall
++  met  ~(. mdl bowl)
++  io   ~(. agentio bowl)
::
++  tx-fact
  |=  [kind=@tas uid=@uv fact=@tas]
  (fact:io graph-view-update+!>([kind uid fact]) /all /tx/(scot %uv uid) ~)
::
++  join
  |_  [uid=@uv rid=resource =ship]
  ++  jn-core  .
  ++  jn-pass-io
    |=  =path
    ~(. pass:io (weld /join/(scot %uv uid) path))
  :: 
  ++  jn-abed
    |=  uid=@uv 
    =/  [r=resource s=^ship]
      (~(got by joining) uid)
    jn-core(uid uid, rid r, ship s)
  ::
  ++  jn-start
    |=  [rid=resource =^ship]
    ^-  (quip card _state)
    =/  uid=@uv
      (shaf %join eny.bowl)
    =.  jn-core
      jn-core(uid uid, rid rid, ship ship)
    =/  maybe-group
       (group-from-app-resource:met %contacts rid)
    :_  state(joining (~(put by joining) uid [rid ship]))
    :_  ~
    ?^  maybe-group
      %+   poke-our:(jn-pass-io /pull-graph)  %graph-pull-hook
      pull-hook-action+!>([%add rid ship])
    %+  poke:(jn-pass-io /add)
      [ship %group-push-hook]
    group-update+!>([%add-members rid (silt our.bowl ~)])
  ::
  ++  jn-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _state)
    |^  
    ?+    -.wire  ~|("bad %join wire" !!)
        %add  :: join group
      ?>  ?=(%poke-ack -.sign)
      ?^  p.sign
        join-failed
      joined
      ::
        %pull-groups
      ?>  ?=(%poke-ack -.sign)
      ?~  p.sign
        ::  do nothing, wait for update from store
        `state
      ::  shouldn't ever fail
      weird-failure
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
      weird-failure
      ::
        %md
      ?+  -.sign  !!
        %fact  (md-fact +.sign)
        %watch-ack  (ack +.sign)
        %kick  md-kick
      ==
      ::
        %pull-graph
      ?>  ?=(%poke-ack -.sign)
      ?^  p.sign
        weird-failure
      :_  state(joining (~(del by joining) uid))
      (tx-fact %join uid %done)^~
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
      :~
        (leave-our:(jn-pass-io /groups) %group-store)
        ::
        %+  poke-our:(jn-pass-io /pull-md)  %metadata-hook
        metadata-hook-action+!>([%add-synced ship (en-path:resource rid)])
      ==
    ::
    ++  md-fact
      |=  [=mark =vase]
      :_  state
      ?.  ?=(%metadata-update mark)  ~
      =+  !<(upd=metadata-update vase)
      ?.  ?=(%add -.upd)  ~
      ?.  =(group-path.upd (en-path:resource rid))  ~
      :~  
        (leave-our:(jn-pass-io /md) %metadata-store)
        %+  poke-our:(jn-pass-io /pull-graph)  %graph-pull-hook
        pull-hook-action+!>([%add ship rid])
      ==
    ++  watch-md
      (watch-our:(jn-pass-io /md) %metadata-store /updates)
    ::
    ++  watch-groups
      (watch-our:(jn-pass-io /groups) %group-store /groups)
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
      weird-failure
    ::
    ++  join-failed
      ::  failed do not continue
      :_  state(joining (~(del by joining) uid))
      (tx-fact %join uid %no-perms)^~
    ::
    ++  joined
      :_  state
      :~
        watch-groups
        ::
        %+  poke-our:(jn-pass-io /pull-group)  %group-pull-hook
        pull-hook-action+!>([%add ship rid])
      ==
    ::
    ++  weird-failure
      ~&  >>>  "Weird failure joining {<rid>}, please report"
      :_  state(joining (~(del by joining) uid))
      (tx-fact %join uid %strange)^~
    --
  --
--
