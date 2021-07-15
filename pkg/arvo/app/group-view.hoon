/-  view-sur=group-view, group-store, *group, metadata=metadata-store
/+  default-agent, agentio, mdl=metadata,
    resource, dbug, grpl=group, conl=contact, verb
|%
++  card  card:agent:gall
::
+$   base-state-0
  joining=(map rid=resource [=ship =progress:view])
::
+$   base-state-1
  joining=(map rid=resource request:view)
::
+$  state-zero
  [%0 base-state-0]
::
+$  state-one
  [%1 base-state-0]
::
+$  state-two
  [%2 base-state-1]
::
+$  versioned-state
  $%  state-zero
      state-one
      state-two
  ==
::
++  view  view-sur
--
=|  state-two
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
  |^
  ?-  -.old
    %2  [cards this(state old)]
    %1  $(-.old %2, +.old (base-state-to-1 +.old))
    %0  $(-.old %1, cards :_(cards (poke-self:pass:io noun+!>(%cleanup))))
  ==
  ::
  ++  base-state-to-1
    |=  base-state-0
    %-  ~(gas by *(map resource request:view))
    (turn ~(tap by joining) request-to-1)
  ::
  ++  request-to-1
    |=  [rid=resource =ship =progress:view]
    ^-  [resource request:view]
    :-  rid
    %*  .  *request:view
      started   now.bowl
      hidden    %.n
      ship      ship
      progress  progress
    ==
  --
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
  =^  cards  state
    ?+  -.action  !!
      %join  jn-abet:(jn-start:join:gc +.action)
      %hide  (hide:gc +.action)
    ==
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
      !>(`update:view`[%initial joining])
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
++  hide
  |=  rid=resource
  ^-  (quip card _state)
  =/  =request:view  (~(got by joining) rid)
  ?:  ?=(final:view progress.request)
    =.  joining  (~(del by joining) rid)
    :_  state
    (fact:io group-view-update+!>(`update:view`[%initial joining]) /all ~)^~
  :-  (fact:io group-view-update+!>([%hide rid]) /all ~)^~
  state(joining (~(put by joining) rid request(hidden %.y)))
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
  =;  new-joining=(map resource request:view)
    `state(joining new-joining)
  %+  roll  ~(tap by joining)
  |=  [[rid=resource =request:view] out=_joining]
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
      (~(jab by joining) rid |=(req=request:view req(progress progress)))
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
    =/  =request:view
      (~(got by joining) r)
    jn-core(rid r, ship ship.request)
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
      (~(put by joining) rid [%.n now.bowl ship %start])
    =.  jn-core
      (jn-abed rid)
    =.  jn-core
      %-  emit
      %+  fact:io
        group-view-update+!>([%started rid (~(got by joining) rid)])
      ~[/all]
    ?<  ~|("already joined {<rid>}" (has-joined rid)) 
    =.  jn-core
      %-  emit
      %+  poke:(jn-pass-io /add)
        [ship %group-push-hook]
      group-update-0+!>([%add-members rid (silt our.bowl ~)])
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
      =.  jn-core
        (tx-progress %added)
      %-  emit
      %+  poke-our:(jn-pass-io /pull-groups)  %group-pull-hook 
      pull-hook-action+!>([%add ship rid])
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
      ?.  ?=(%metadata-update-2 mark)    jn-core
      =+  !<(=update:metadata vase)
      ?.  ?=(%initial-group -.update)  jn-core
      ?.  =(group.update rid)          jn-core
      =.  jn-core  (cleanup %done)
      ?.  hidden:(need (scry-group:grp rid))
        =/  list-md=(list [=md-resource:metadata =association:metadata])
          %+  skim  ~(tap by associations.update)
          |=  [=md-resource:metadata =association:metadata]
          =(app-name.md-resource %groups)
        ?>  ?=(^ list-md)
        =*  metadatum  metadatum.association.i.list-md
        ?.  ?&  ?=(%group -.config.metadatum)
                ?=(^ feed.config.metadatum)
                ?=(^ u.feed.config.metadatum)
            ==
          jn-core
        =*  feed  resource.u.u.feed.config.metadatum
        %-  emit
        %+  poke-our:(jn-pass-io /pull-feed)  %graph-pull-hook
        pull-hook-action+!>([%add [entity .]:feed])
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
      =.  jn-core
        (emit (leave-our:(jn-pass-io /groups) %group-store))
      (emit (leave-our:(jn-pass-io /md) %metadata-store))
    --
  --
--
