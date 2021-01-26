::  metadata-pull-hook [landscape]:
::
::  allow syncing group data from foreign paths to local paths
::
/-  *group, invite-store, *metadata-store
/+  default-agent, verb, dbug, store=group-store, grpl=group, pull-hook
/+  resource, mdl=metadata
~%  %group-hook-top  ..part  ~
|%
+$  card  card:agent:gall
::
++  config
  ^-  config:pull-hook
  :*  %metadata-store
      metadata-update
      %metadata-update
      %metadata-push-hook
  ==
+$  state-zero
  [%0 previews=(map resource group-preview)]
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
    ++  watch-preview
      |=  rid=resource
      ^-  card
      =/  =path
        preview+(en-path:resource rid)
      =/  =dock
        [entity.rid %metadata-push-hook]
      [%pass path %agent dock %watch path]
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
        (watch-preview resource.invite.update)^~
      ::
        %kick  [watch-invites^~ state]
      ==
    ::
    ++  take-preview
      |=  [=wire =sign:agent:gall]
      ^-  (quip card _state)
      ?>  ?=([%preview @ *] wire)
      =/  rid=resource
        (de-path:resource t.wire)
      ?+  -.sign  (on-agent:def wire sign)
          %fact  
        ?>  =(%metadata-update p.cage.sign)
        =+  !<(upd=metadata-update q.cage.sign)
        ?>  ?=(%preview -.upd)
        :_  state(previews (~(put by previews) rid +.upd))
        :~  [%give %fact ~[wire] cage.sign]
            [%give %kick ~[wire] ~]
        ==
      ::
          %watch-ack
        :_  state
        ?~  p.sign  ~
        :~  [%give %fact ~[wire] tang+!>(u.p.sign)]
            [%give %kick ~[wire] ~]
        ==
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
  ?:  =(1 1)  `this
  =+  !<(old=state-zero vase)
  :_  this(state old)
  ?:  (~(has by wex.bowl) [/invites our.bowl %invite-store])  ~
  watch-invites^~
::
++  on-poke  on-poke:def
++  on-agent  
  |=  [=wire =sign:agent:gall]
  =^  cards  state
    ?+  wire  (on-agent:def:hc wire sign)
      [%invites ~]        (take-invites:hc sign)
      [%preview @ @ @ ~]  (take-preview:hc wire sign)
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
  =/  prev=(unit group-preview)
    (~(get by previews) rid)
  :_  this
  ?^  prev
    :~  [%give %fact ~ metadata-update+!>([%preview u.prev])]
        [%give %kick ~ ~]
    ==
  =/  =dock
    [entity.rid %metadata-push-hook]
  [%pass path %agent dock %watch path]~
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
::
++  on-fail   on-fail:def
++  on-pull-nack
  |=   [=resource =tang]
  ^-  (quip card _this)
  =/  =associations
    (metadata-for-group:met resource)
  :_  this
  %+  turn  ~(tap by associations)
  |=  [=md-resource =association]
  =-  [%pass / %agent [our.bowl %metadata-store] %poke -]
  :-  %metadata-update
  !>  ^-  metadata-update
  [%remove resource md-resource]
::
++  on-pull-kick
  |=  =resource
  ^-  (unit path)
  `/
--
