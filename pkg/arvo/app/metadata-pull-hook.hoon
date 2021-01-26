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
  [%0 ~]
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
    ++  add-resource
      |=  rid=resource
      ^-  card
      =-  [%pass / %agent [our dap]:bowl %poke -]
      pull-hook-action+!>(`action:pull-hook`[%add [entity .]:rid])
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
        (add-resource resource.invite.update)^~
      ::
        %kick  [watch-invites^~ state]
      ==
    --
|_  =bowl:gall
+*  this        .
    def         ~(. (default-agent this %|) bowl)
    dep         ~(. (default:pull-hook this config) bowl)
    met         ~(. mdl bowl)
    grp         ~(. grpl bowl)
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
  =/  prev=(unit group-preview)  ~
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
  |=  rid=resource
  ^-  (unit path)
  ?.  (is-member:grp our.bowl rid)  ~
  `/
::
++  take-update
  |=  [rid=resource =vase]
  `this
--
