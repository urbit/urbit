::  metadata-pull-hook [landscape]:
::
::  allow syncing group data from foreign paths to local paths
::
/-  *group, invite-store, metadata=metadata-store
/+  default-agent, verb, dbug, store=group-store, grpl=group, pull-hook
/+  resource, mdl=metadata
~%  %group-hook-top  ..part  ~
|%
+$  card  card:agent:gall
::
++  config
  ^-  config:pull-hook
  :*  %metadata-store
      update:metadata
      %metadata-update
      %metadata-push-hook
  ==
+$  state-zero
  [%0 previews=(map resource group-preview:metadata)]
::
--
::
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:pull-hook config)
^-  (pull-hook:pull-hook config)
=|  state-zero
=*  state  -
=>  |_  =bowl:gall
    ++  def   ~(. (default-agent state %|) bowl)
    ++  get-preview
      |=  rid=resource
      ^-  card
      =/  =path
        preview+(en-path:resource rid)
      =/  =dock
        [entity.rid %metadata-push-hook]
      =/  =cage
        metadata-hook-update+!>([%req-preview rid])
      [%pass path %agent dock %poke cage]
    ::
    ++  watch-invites
      ^-  card
      [%pass /invites %agent [our.bowl %invite-store] %watch /updates]
    ::
    ++  take-invites
      |=  =sign:agent:gall
      ^-  (quip card _state)
      ?+  -.sign  (on-agent:def /invites sign)
          %fact
        ?>  ?=(%invite-update p.cage.sign)
        =+  !<(=update:invite-store q.cage.sign)
        :_  state
        ?.  ?=(%invite -.update)  ~
        ?.  =(%contacts term.update)  ~
        (get-preview resource.invite.update)^~
      ::
        %kick  [watch-invites^~ state]
      ==
    --
|_  =bowl:gall
+*  this        .
    def         ~(. (default-agent this %|) bowl)
    dep         ~(. (default:pull-hook this config) bowl)
    met         ~(. mdl bowl)
    hc          ~(. +> bowl)
::
++  on-init  on-init:def
++  on-save  !>(state)
++  on-load  
  |=  =vase
  =+  !<(old=state-zero vase)
  :_  this(state old)
  ?:  (~(has by wex.bowl) [/invites our.bowl %invite-store])  ~
  ~[watch-invites:hc]
::
++  on-poke  
  |=  [=mark =vase]
  ?.  ?=(%metadata-hook-update mark)
    (on-poke:def mark vase)
  =+  !<(=hook-update:metadata vase)
  ?.  ?=(%preview -.hook-update)
    (on-poke:def mark vase)
  :_  this(previews (~(put by previews) group.hook-update +.hook-update))
  =/  paths=(list path)
    ~[preview+(en-path:resource group.hook-update)]
  :~  [%give %fact paths mark^vase]
      [%give %kick paths ~]
  ==
::
++  on-agent  
  |=  [=wire =sign:agent:gall]
  =^  cards  state
    ?+  wire  (on-agent:def:hc wire sign)
      [%invites ~]        (take-invites:hc sign)
      ::
        [%preview @ @ @ ~]
      ?.  ?=(%poke-ack -.sign)
        (on-agent:def:hc wire sign)
      :_  state
      ?~  p.sign  ~
      :~  [%give %fact ~[wire] tang+!>(u.p.sign)]
          [%give %kick ~[wire] ~]
      ==
    ==
  [cards this]
::
++  on-watch  
  |=  =path
  ?>  (team:title [our src]:bowl)
  ?.  ?=([%preview @ @ @ ~] path)
    (on-watch:def path)
  =/  rid=resource
    (de-path:resource t.path)
  =/  prev=(unit group-preview:metadata)
    (~(get by previews) rid)
  :_  this
  ?~  prev
    (get-preview rid)^~
  [%give %fact ~ metadata-hook-update+!>([%preview u.prev])]~
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
::
++  on-fail   on-fail:def
++  on-pull-nack
  |=   [=resource =tang]
  ^-  (quip card _this)
  =/  =associations:metadata
    (metadata-for-group:met resource)
  :_  this
  %+  turn  ~(tap by associations)
  |=  [=md-resource:metadata =association:metadata]
  =-  [%pass / %agent [our.bowl %metadata-store] %poke -]
  :-  %metadata-update
  !>  ^-   update:metadata
  [%remove resource md-resource]
::
++  on-pull-kick
  |=  =resource
  ^-  (unit path)
  `/
--
