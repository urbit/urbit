/-  view-sur=group-view, group-store, *group, metadata=metadata-store
/+  default-agent, agentio, mdl=metadata,
    resource, dbug, grpl=group, conl=contact, verb
|%
++  card  card:agent:gall
::
+$   base-state
  joining=(map rid=resource [=ship =progress:view])
::
+$  state-zero
  [%0 base-state]
::
+$  state-one
  [%1 base-state]
::
+$  versioned-state
  $%  state-zero
      state-one
  ==
::
++  view  view-sur
--
=|  state-one
=*  state  -
::
%-  agent:dbug
%+  verb  |
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
  =+  !<(old=versioned-state vase)
  =|  cards=(list card)
  |-
  ?:  ?=(%1 -.old)
    `this(state old)
  $(-.old %1, cards :_(cards (poke-self:pass:io noun+!>(%cleanup))))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?:  ?=(%noun mark)
    =^  cards  state
      poke-noun:gc
    [cards this]
  ?.  ?=(%group-view-action mark)
    (on-poke:def mark vase)
  =+  !<(=action:view vase)
  ?>  ?=(%join -.action)
  =^  cards  state
    jn-abet:(jn-start:join:gc +.action)
  [cards this]
::
++  on-watch
  |=  =path
  ?+  path  (on-watch:def path)
      [%all ~]  
    :_  this
    :_  ~
    %+  fact:io  
      :-  %group-view-update
      !>  ^-  update:view
      [%initial (~(run by joining) |=([=ship =progress:view] progress))]
    ~
  ==
::
++  on-peek  on-peek:def
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  =^  cards  state
    ?+    wire  `state
        [%join %ship @ @ *]
      =/  rid
        (de-path:resource t.wire)
      ?.  (~(has by joining) rid)  `state
      jn-abet:(jn-agent:(jn-abed:join:gc rid) t.t.t.t.wire sign)
    ==
  [cards this]
::
++  on-arvo  on-arvo:def
++  on-leave  on-leave:def
++  on-fail  on-fail:def
--
|_  =bowl:gall
++  met  ~(. mdl bowl)
++  grp  ~(. grpl bowl)
++  io   ~(. agentio bowl)
++  con  ~(. conl bowl)
::
++  has-joined
  |=  rid=resource
  =-  ?=(^ -)
  ?~  grp=(peek-group:met %groups rid)
    (peek-group:met %graph rid)
  grp
::
++  poke-noun
  ^-  (quip card _state)
  =;  new-joining=(map resource [ship progress:view])
    `state(joining new-joining)
  %+  roll  ~(tap by joining)
  |=  [[rid=resource =ship =progress:view] out=_joining]
  ?.  (has-joined rid)  out
  (~(del by out) rid)
::
++  join
  |_  [rid=resource =ship cards=(list card)]
  ++  jn-core  .
  ++  emit-many
    |=  crds=(list card)
    jn-core(cards (weld (flop crds) cards))
  ::
  ++  emit
    |=  =card
    jn-core(cards [card cards])
  ::
  ++  tx-progress
    |=  =progress:view
    =.  joining
      (~(put by joining) rid [ship progress])
    =;  =cage
      (emit (fact:io cage /all tx+(en-path:resource rid) ~))
    group-view-update+!>([%progress rid progress]) 
  ::
  ++  watch-md
    (emit (watch-our:(jn-pass-io /md) %metadata-store /updates))
  ::
  ++  watch-groups
    (emit (watch-our:(jn-pass-io /groups) %group-store /groups))
  ::
  ++  jn-pass-io
    |=  pax=path
    ~(. pass:io (welp join+(en-path:resource rid) pax))
  :: 
  ++  jn-abed
    |=  r=resource
    =/  [s=^ship =progress:view]
      (~(got by joining) r)
    jn-core(rid r, ship s)
  ::
  ++  jn-abet
    ^-  (quip card _state)
    [(flop cards) state]
  ::
  ++  jn-start
    |=  [rid=resource =^ship]
    ^+  jn-core
    ?<  (~(has by joining) rid)
    =.  joining
      (~(put by joining) rid [ship %start])
    =.  jn-core
      (jn-abed rid)
    ?<  ~|("already joined {<rid>}" (has-joined rid)) 
    =.  jn-core
      %-  emit
      %+  poke:(jn-pass-io /add)
        [ship %group-push-hook]
      group-update+!>([%add-members rid (silt our.bowl ~)])
    =.  jn-core  (tx-progress %start)
    =>  watch-md
    watch-groups
  ::
  ++  jn-agent
    |=  [=wire =sign:agent:gall]
    ^+  jn-core
    |^  
    ?+    -.wire  ~|("bad %join wire" !!)
        %add  :: join group
      ?>  ?=(%poke-ack -.sign)
      ?^  p.sign
        (cleanup %no-perms)
      =>  %-  emit
          %+  poke-our:(jn-pass-io /pull-groups)  %group-pull-hook 
          pull-hook-action+!>([%add ship rid])
      (tx-progress %added)
    ::
        %pull-groups
      ?>  ?=(%poke-ack -.sign)
      (ack +.sign)
    ::
        %groups
      ?+  -.sign  !!
        %fact  (groups-fact +.sign)
        %watch-ack  (ack +.sign)
        %kick  watch-groups
      ==
    ::
        %pull-md
      ?>  ?=(%poke-ack -.sign)
      (ack +.sign)
    ::
        %pull-co
      ?>  ?=(%poke-ack -.sign)
      (ack +.sign)
    ::
        %share-co
      ?>  ?=(%poke-ack -.sign)
      (ack +.sign)
    ::
        %push-co
      ?>  ?=(%poke-ack -.sign)
      (ack +.sign)
    ::
        %md
      ?+  -.sign  !!
        %fact  (md-fact +.sign)
        %watch-ack  (ack +.sign)
        %kick  watch-md
      ==
    ::
        %pull-graphs
      ?>  ?=(%poke-ack -.sign)
      %-  cleanup
      ?^(p.sign %strange %done)
    ==
    ::
    ++  groups-fact
      |=  =cage
      ?.  ?=(%group-update-0 p.cage)  jn-core
      =+  !<(=update:group-store q.cage)
      ?.  ?=(%initial-group -.update)  jn-core
      ?.  =(rid resource.update)  jn-core
      %-  emit-many
      =/  cag=^cage  pull-hook-action+!>([%add [entity .]:rid])
      %-  zing
      :~  [(poke-our:(jn-pass-io /pull-md) %metadata-pull-hook cag)]~
          [(poke-our:(jn-pass-io /pull-co) %contact-pull-hook cag)]~
        ::
          ?.  scry-is-public:con  ~
          :_  ~
          %+  poke:(jn-pass-io /share-co)
            [entity.rid %contact-push-hook]
          [%contact-share !>([%share our.bowl])]
      ==
    ::
    ++  md-fact
      |=  [=mark =vase]
      ?.  ?=(%metadata-update-0 mark)    jn-core
      =+  !<(=update:metadata vase)
      ?.  ?=(%initial-group -.update)  jn-core
      ?.  =(group.update rid)          jn-core
      =.  jn-core  (cleanup %done)
      ?.  hidden:(need (scry-group:grp rid))  jn-core
      %-  emit-many
      %+  murn  ~(tap by associations.update)
      |=  [=md-resource:metadata =association:metadata]
      ^-  (unit card)
      ?.  =(app-name.md-resource %graph)  ~
      =*  rid  resource.md-resource
      :-  ~
      %+  poke-our:(jn-pass-io /pull-graph)  %graph-pull-hook
      pull-hook-action+!>([%add [entity .]:rid])
    ::
    ++  ack
      |=  err=(unit tang)
      ?~  err  jn-core
      %-  (slog u.err)
      (cleanup %strange)
    ::
    ++  cleanup
      |=  =progress:view
      =.  jn-core
        (tx-progress progress)
      =.  joining  (~(del by joining) rid)
      =.  jn-core
        (emit (leave-our:(jn-pass-io /groups) %group-store))
      (emit (leave-our:(jn-pass-io /md) %metadata-store))
    --
  --
--
