::  metadata-push-hook [landscape]:
::
/-  *group, *invite-store, store=metadata-store
/+  default-agent, verb, dbug, grpl=group, push-hook,
    resource, mdl=metadata, gral=graph, agentio
~%  %group-hook-top  ..part  ~
|%
+$  card  card:agent:gall
::
++  config
  ^-  config:push-hook
  :*  %metadata-store
      /all
      update:store
      %metadata-update
      %metadata-pull-hook
      1  1
  ==
::
+$  agent  (push-hook:push-hook config)
::
+$  state-null  ~
+$  state-zero  [%0 ~]
::
+$  versioned-state
  $%  state-null
      state-zero
  ==
--
::
::
=|  state-zero
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:push-hook config)
^-  agent
|_  =bowl:gall
+*  this        .
    def         ~(. (default-agent this %|) bowl)
    grp       ~(. grpl bowl)
    met       ~(. mdl bowl)
    gra       ~(. gral bowl)
    io        ~(. agentio bowl)
    pass      pass:io
::
++  on-init  on-init:def
++  on-save  !>(~)
++  on-load  on-load:def
::
++  on-poke
  |=  [=mark =vase]
  |^  ^-  (quip card _this)
  ?+  mark  (on-poke:def mark vase)
    %metadata-hook-update  metadata-hook-update
    %noun                  noun
  ==
  ::
  ++  metadata-hook-update
    =+  !<(=hook-update:store vase)
    ?.  ?=(%req-preview -.hook-update)
      (on-poke:def mark vase)
    ?>  =(entity.group.hook-update our.bowl)
    =/  =group-preview:store
      (get-preview:met group.hook-update)
    :_  this
    =-  [%pass / %agent [src.bowl %metadata-pull-hook] %poke -]~
    metadata-hook-update+!>(`hook-update:store`[%preview group-preview])
  ::
  ++  noun
    ?+  q.vase  ~|("unknown noun poke" !!)
    ::
        %clean-dm
      =+  .^(sharing=(set resource) (scry:io %gx dap.bowl /sharing/noun))
      :_  this
      %+  murn  ~(tap in sharing)
      |=  rid=resource
      ^-  (unit card)
      ?@  (rush name.rid ;~(pfix (jest 'dm--') fed:ag))  ~
      `(poke-self:pass push-hook-action+!>([%remove rid]))
    ==
  --
::
++  on-agent  on-agent:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
::
++  transform-proxy-update
  |=  vas=vase
  ^-  (quip card (unit vase))
  =/  =update:store  !<(update:store vas)
  :-  ~
  ?.  ?=(?(%add %remove %edit) -.update)
    ~
  =/  role=(unit (unit role-tag))
    (role-for-ship:grp group.update src.bowl)
  ?~  role  ~
  =/  metadatum=(unit metadatum:store)
    (peek-metadatum:met %groups group.update)
  ?:  ?&  ?=(~ metadatum)
          (is-managed:grp group.update)
      ==
    ~
  ?:  ?&  ?=(^ metadatum)
          !(is-managed:grp group.update)
      ==
    ~
  ?^  u.role
    ?:  ?=(?(%admin %moderator) u.u.role)
      `vas
    ~
  ?.  ?=(%add -.update)  ~
  ?:  ?&  ?=(^ metadatum)
          =(src.bowl entity.resource.resource.update)
          ?=(%member-metadata vip.u.metadatum)
      ==
    `vas
  ~
::
++  resource-for-update  resource-for-update:met
++  take-update
  |=   =vase
  ^-  [(list card) agent]
  `this
::
++  initial-watch
  |=  [=path rid=resource]
  ^-  vase
  =/  group
    (scry-group:grp rid)
  =/  =associations:store
    (metadata-for-group:met rid)
  ?>  ?=(^ group)
  ?>  (~(has in members.u.group) src.bowl)
  !>  ^-  update:store
  [%initial-group rid associations]
::
--
