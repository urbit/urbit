/-  view-sur=group-view, group-store, *group, metadata=metadata-store, hark=hark-store
/-  inv=invite-store
/+  default-agent, agentio, mdl=metadata,
    resource, dbug, grpl=group, conl=contact, verb
|%
++  card  card:agent:gall
::
::
::
+$  state-zero
  [%0 *]
::
+$  state-one
  [%1 *]
::
+$  state-two
  [%2 *]
::
+$  state-three
  [%3 joining=(map rid=resource request:view)]
::
+$  versioned-state
  $%  state-zero
      state-one
      state-two
      state-three
  ==
::
++  view  view-sur
--
=|  state-three
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
  ?:  ?=(%3 -.old)
    [cards this(state old)]
  $(old *state-three)
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
      %join   jn-abet:(jn-start:join:gc +.action)
      %abort  jn-abet:(jn-abort:join:gc +.action)
      %done   jn-abet:(jn-done:join:gc +.action)
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
    ?+    wire  (on-agent:def:gc wire sign)
        [%join %ship @ @ *]
      =/  rid
        (de-path:resource t.wire)
      ?.  (~(has by joining) rid)  `state
      jn-abet:(jn-agent:(jn-abed:join:gc rid) t.t.t.t.wire sign)
    ==
  [cards this]
::
++  on-arvo  
  |=  [=wire sign=sign-arvo]
  =^  cards  state
    ?+    wire  (on-arvo:def:gc wire sign)
        [%breach ~]
      ?>  ?=([%jael %public-keys *] sign)
      ?.  ?=(%breach -.public-keys-result.sign)
        `state
      (breach who.public-keys-result.sign)
    ==
  [cards this]
::
++  on-leave  on-leave:def
++  on-fail  on-fail:def
--
|_  =bowl:gall
++  met  ~(. mdl bowl)
++  grp  ~(. grpl bowl)
++  io   ~(. agentio bowl)
++  con  ~(. conl bowl)
++  def   ~(. (default-agent state %|) bowl)
++  hide
  |=  rid=resource
  ^-  (quip card _state)
  =/  =request:view  (~(got by joining) rid)
  ?:  ?=(final:view progress.request)
    =.  joining  (~(del by joining) rid)
    :_  state
    (fact:io group-view-update+!>(`update:view`[%initial joining]) /all ~)^~
  :-  (fact:io group-view-update+!>([%hide rid]) /all ~)^~
  state(joining (~(put by joining) rid request))
::
++  is-tracking
  |=  her=ship
  ^-  ?
  %+  lien  ~(tap in ~(key by joining))
  |=([him=ship name=term] =(her him))
::
++  breach
  |=  who=ship
  ^-  (quip card _state)
  =/  requests=(list [rid=resource =request:view])
    ~(tap by joining)
  =|  cards=(list card)
  |-  ^-  (quip card _state)
  ?~  requests
    [cards state]
  ?.  =(entity.rid.i.requests who)  
    $(requests t.requests)
  =^  crds  state
    jn-abet:jn-breach:(jn-abed:join rid.i.requests)
  [(welp cards crds) state]
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
  ++  pass
    |%
    ++  pull-action  pull-hook-action+!>([%add ship rid])
    ::
    ++  listen-hark
      |=  gr=resource
      %+  poke-our:pass:io  %hark-graph-hook
      hark-graph-hook-action+!>([%listen gr /])
    ::
    ++  watch-md      (watch-our:(jn-pass-io /md) %metadata-store /updates)
    ++  watch-groups  (watch-our:(jn-pass-io /groups) %group-store /groups)
    ++  watch-md-nacks  (watch-our:(jn-pass-io /md-nacks) %metadata-pull-hook /nack)
    ++  watch-grp-nacks  (watch-our:(jn-pass-io /grp-nacks) %group-pull-hook /nack)
    ::
    ++  add-us 
      %+  poke:(jn-pass-io /add)
        [ship %group-push-hook]
      group-update-0+!>([%add-members rid (silt our.bowl ~)])
    ::
    ++  del-us
      %+  poke:pass:io  [ship %group-push-hook]
      group-update-0+!>([%remove-members rid (silt our.bowl ~)])
    ::
    ++  remove-pull-groups
      (poke-our:pass:io %group-pull-hook pull-hook-action+!>([%remove rid]))
    ::
    ++  pull-groups  
      (poke-our:(jn-pass-io /poke) %group-pull-hook pull-action)
    ++  pull-md
      (poke-our:(jn-pass-io /poke) %metadata-pull-hook pull-action)
    ++  pull-co
      (poke-our:(jn-pass-io /poke) %contact-pull-hook pull-action)
    ::
    ++  allow-co
      %+  poke-our:(jn-pass-io /poke)  %contact-store
      contact-update-0+!>([%allow %group rid])
    ::
    ++  share-co
      %+  poke:(jn-pass-io /poke)
        [entity.rid %contact-push-hook]
      [%contact-share !>([%share our.bowl])]
    ::
    ++  pull-gra
      |=  gr=resource
      (poke-our:(jn-pass-io /poke) %graph-pull-hook pull-hook-action+!>([%add entity .]:gr))
    ::
    ++  retry
      (poke-self:pass:io group-view-action+!>([%join rid ship]))
    ++  watch-breach
      (~(arvo pass:io /breach) %j %public-keys (silt ship ~))
    ++  leave-breach
      (~(arvo pass:io /breach) %j %nuke (silt ship ~))
    --
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
    |=  [rid=resource =^ship =app:view share-co=? autojoin=?]
    ^+  jn-core
    ?>  ?=  $@(~ [~ ?(%done %abort)])
            (bind (~(get by joining) rid) |=(request:view progress))
    =/  =request:view
      [now.bowl ship %start app share-co autojoin (get-invites app rid)]
    =.  joining
      (~(put by joining) rid request)
    =.  jn-core
      (jn-abed rid)
    =.  jn-core
      %-  emit
      %+  fact:io
        group-view-update+!>([%started rid (~(got by joining) rid)])
      ~[/all]
    ?<  ~|("already joined {<rid>}" (has-joined rid)) 
    =.  jn-core  (emit add-us:pass)
    =.  jn-core  (tx-progress %start)
    =?  jn-core  !(is-tracking ship)
      (emit watch-breach:pass)
    =>  (emit watch-md:pass)
    =>  (emit watch-groups:pass)
    =>  (emit watch-grp-nacks:pass)
    =>  (emit watch-md-nacks:pass)
    (emit watch-breach:pass)
  ::
  ++  jn-breach
    =/  =request:view  (~(got by joining) rid)
    ?.  ?=(%start progress.request)
      :: no action required, subscriptions are sane across breaches 
      jn-core 
    (emit add-us:pass)
  ::
  ++  jn-abort
    |=  r=resource
    ^+  jn-core
    =.  jn-core  (jn-abed r)
    (cleanup:rollback %abort)
  ::
  ++  jn-done
    |=  r=resource
    =.  joining  (~(del by joining) r)
    jn-core
  ::
  ++  rollback
    |^
    =/  =request:view     (~(got by joining) rid)
    ?+  progress.request  ~|(cannot-rollback/progress.request !!)
      %start                        start
      %added                        added
      %metadata                     metadata
      ?(%no-perms %strange %abort)  error
    ==
    ++  error   jn-core
    ++  start   jn-core
    ++  added   (emit del-us:pass)
    ++  metadata  (emit:added remove-pull-groups:pass)
    --
  ::
  ++  get-invites
    |=  [=app:view rid=resource]
    ^-  (set uid:view)
    =+  .^(invit=(unit invitatory:inv) %gx (scry:io %invite-store /invitatory/[app]/noun))
    ?~  invit  ~
    %-  ~(gas in *(set uid:view))
    %+  murn  ~(tap by u.invit)
    |=  [=uid:view =invite:inv]
    ?.  =(rid resource.invite)  ~
    `uid
  ::
  ++  cleanup
    |=  =progress:view
    =.  jn-core
      (tx-progress progress)
    =.  jn-core
      (emit (leave-our:(jn-pass-io /groups) %group-store))
    =.  jn-core
      (emit (leave-our:(jn-pass-io /md) %metadata-store))
    =.  jn-core
      (emit (leave-our:(jn-pass-io /md-nacks) %metadata-pull-hook))
    =.  jn-core
      (emit (leave-our:(jn-pass-io /grp-nacks) %group-pull-hook))
    =/  =request:view  (~(got by joining) rid)
    =.  jn-core
      %-  emit-many 
      %+  turn  ~(tap in invite.request)
      |=  =uid:view
      %+  poke-our:pass:io  %invite-store
      =-  invite-action+!>(-)
      ^-  action:inv
      [%accept `@tas`app.request uid]
    jn-core
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
      (emit pull-groups:pass)
    ::
        %groups
      ?+  -.sign  !!
        %fact  (groups-fact +.sign)
        %watch-ack  (ack +.sign)
        %kick  (emit watch-groups:pass)
      ==
    ::
        %poke
      ?>  ?=(%poke-ack -.sign)
      (ack +.sign)
    ::
        %md
      ?+  -.sign  !!
        %fact  (md-fact +.sign)
        %watch-ack  (ack +.sign)
        %kick  (emit watch-md:pass)
      ==
    ::
        %pull-graphs
      ?>  ?=(%poke-ack -.sign)
      %-  cleanup
      ?^(p.sign %strange %done)
    ::
        %md-nacks
      ?+  -.sign  !!
        %watch-ack  (ack +.sign)
        %kick  (emit watch-md-nacks:pass)
      ::
          %fact
        ?.  =(%resource p.cage.sign)  jn-core
        =+  !<(nack=resource q.cage.sign)
        ?.  =(nack rid)  jn-core
        (cleanup %strange)
      ==
    ::
        %grp-nacks
      ?+  -.sign  !!
        %watch-ack  (ack +.sign)
        %kick  (emit watch-grp-nacks:pass)
      ::
          %fact
        ?.  =(%resource p.cage.sign)  jn-core
        =+  !<(nack=resource q.cage.sign)
        ?.  =(nack rid)  jn-core
        (cleanup %strange)
      ==

    ==
    ::
    ++  groups-fact
      |=  =cage
      ?.  ?=(%group-update-0 p.cage)  jn-core
      =+  !<(=update:group-store q.cage)
      ?.  ?=(%initial-group -.update)  jn-core
      =/  =request:view  (~(got by joining) rid)
      ?.  =(rid resource.update)  jn-core
      =.  jn-core  (emit pull-md:pass)
      =.  jn-core  (emit pull-co:pass)
      ?.  |(share-co.request scry-is-public:con)
        jn-core
      ?:  scry-is-public:con  (emit share-co:pass)
      =.  jn-core  (emit allow-co:pass)
      (emit share-co:pass)
    ::
    ++  md-fact
      |=  [=mark =vase]
      ?.  ?=(%metadata-update-2 mark)    jn-core
      =+  !<(=update:metadata vase)
      ?.  ?=(%initial-group -.update)  jn-core
      ?.  =(group.update rid)          jn-core
      |^  ^+  jn-core
      =/  =request:view  (~(got by joining) rid)
      =/  feed  feed-rid
      =.  jn-core  (cleanup %done)
      =/  hidden  hidden:(need (scry-group:grp rid))
      =?  jn-core  ?&(!hidden ?=(^ feed))
        %-  emit
        (pull-gra:pass (need feed))
      =?  jn-core  |(hidden autojoin.request)
        %-  emit-many
        (turn graphs pull-gra:pass)
      =?  jn-core  hidden
        %-  emit-many
        (turn graphs listen-hark:pass)
      jn-core
      ::
      ++  feed-rid
        ^-  (unit resource)
        =/  list-md=(list [=md-resource:metadata =association:metadata])
          %+  skim  ~(tap by associations.update)
          |=  [=md-resource:metadata =association:metadata]
          =(app-name.md-resource %groups)
        ?~  list-md  ~
        =*  metadatum  metadatum.association.i.list-md
        ?.  ?&  ?=(%group -.config.metadatum)
                ?=([~ ~ *] feed.config.metadatum)
            ==
          ~
        `resource.u.u.feed.config.metadatum
      ::
      ++  graphs
        ^-  (list resource)
        %+  murn  ~(tap by associations.update)
        |=  [=md-resource:metadata =association:metadata]
        ?.  =(app-name.md-resource %graph)  ~
        `resource.md-resource
      --
    ::
    ++  ack
      |=  err=(unit tang)
      ?~  err  jn-core
      %-  (slog u.err)
      (cleanup %strange)
    ::
    --
  --
--
