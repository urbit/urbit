::  metadata-pull-hook [landscape]:
::
::  allow syncing group data from foreign paths to local paths
::
/-  *group, invite-store, metadata=metadata-store, contact=contact-store
/+  default-agent, verb, dbug, store=group-store, grpl=group, pull-hook
/+  resource, mdl=metadata, agn=agentio
~%  %group-hook-top  ..part  ~
|%
+$  card  card:agent:gall
::
+$  group-preview-0
  $:  group=resource
      channels=associations-0
      members=@ud
      channel-count=@ud
      metadatum=metadatum-0
  ==
::
+$  associations-0
  (map md-resource:metadata [group=resource metadatum=metadatum-0])
::
+$  metadatum-0
  $:  title=cord
      description=cord
      =color:metadata
      date-created=time
      creator=ship
      module=term
      picture=url:metadata
      preview=?
      vip=vip-metadata:metadata
  ==
::
++  config
  ^-  config:pull-hook
  :*  %metadata-store
      update:metadata
      %metadata-update
      %metadata-push-hook
      2  2
      %.n
  ==
+$  state-zero
  [%0 previews=(map resource group-preview-0)]
::
+$  state-one
  $:  %1
      pending=(set resource)
      previews=(map resource group-preview-0)
  ==
::
+$  state-two
  $:  %2
      pending=(set resource)
      previews=(map resource group-preview:metadata)
  ==
::
+$  versioned-state
  $%  state-zero
      state-one
      state-two
  ==
--
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:pull-hook config)
^-  (pull-hook:pull-hook config)
=|  state-two
=*  state  -
=>  |_  =bowl:gall
    ++  def   ~(. (default-agent state %|) bowl)
    ++  met   ~(. mdl bowl)
    ++  io    ~(. agn bowl)
    ++  get-preview
      |=  rid=resource
      =/  =path
        preview+(en-path:resource rid)
      =/  =dock
        [entity.rid %metadata-push-hook]
      %+  ~(poke pass:io path)  dock
      metadata-hook-update+!>([%req-preview rid])
    ::
    ++  watch-invites
      (~(watch-our pass:io /invites) %invite-store /updates)
    ::
    ++  take-invites
      |=  =sign:agent:gall
      ^-  (quip card _state)
      ?+  -.sign  (on-agent:def /invites sign)
          %fact
        ?>  ?=(%invite-update p.cage.sign)
        =+  !<(=update:invite-store q.cage.sign)
        ?.  ?=(%invite -.update)  `state
        ?:  (~(has in pending) resource.invite.update)  `state
        :_  state(pending (~(put in pending) resource.invite.update))
        (get-preview resource.invite.update)^~
      ::
        %kick  [watch-invites^~ state]
      ==
    ::
    ++  watch-contacts
      (~(watch-our pass:io /contacts) %contact-store /all)
    ::
    ++  take-contacts
      |=  =sign:agent:gall
      ^-  (quip card _state)
      ?+  -.sign  (on-agent:def /contacts sign)
        %kick  [~[watch-contacts] state]
        ::
          %fact
        ?>  ?=(%contact-update-0 p.cage.sign)
        =+  !<(=update:contact q.cage.sign)
        ?+  -.update  `state
            %add
          =/  missing=(set resource)
            (check-contact contact.update)
          =.  pending
            (~(uni in pending) missing)
          :_  state
          (get-many-previews missing)

        ::
            %edit
          ?.  ?=(%add-group -.edit-field.update)  `state
          =/  missing=(set resource)
            %-  add-missing-previews
            (~(gas in *(set resource)) resource.edit-field.update ~)
          =.  pending
            (~(uni in pending) missing)
          :_  state
          (get-many-previews missing)
        ::
        ::
            %initial
          =/  missing=(set resource)
            %-  add-missing-previews
            %+  roll  ~(tap by rolodex.update)
            |=  [[ship =contact:contact] out=(set resource)]
            (~(uni in out) (check-contact contact))
          =.  pending
            (~(uni in pending) missing)
          :_  state
          (get-many-previews missing)
        ==
      ==
    ::
    ++  get-many-previews
      |=  rids=(set resource)
      (turn ~(tap by rids) get-preview)
    ::
    ++  check-contact
      |=  =contact:contact
      ^-  (set resource)
      (add-missing-previews groups.contact)
    ::
    ++  add-missing-previews
      |=  groups=(set resource)
      ^-  (set resource)
      =/  have=(set resource)
        (~(uni in ~(key by previews)) pending)
      =/  missing=(set resource)
        (~(dif in groups) have)
      %-  ~(gas by *(set resource))
      %+  murn  ~(tap by missing)
      |=  group=resource
      ^-  (unit resource)
      ?^  (peek-metadatum:met %groups group)  ~
      `group
    ::
    ++  watch-store
      (~(watch-our pass:io /store) %metadata-store /all)
    ::
    ++  take-store
      |=  =sign:agent:gall
      ^-  (quip card _state)
      ?+  -.sign  (on-agent:def /store sign)
        %kick  [watch-store^~ state]
        ::
          %fact
        ?>  ?=(%metadata-update-2 p.cage.sign)
        =+  !<(=update:metadata q.cage.sign)
        ?.  ?=(%initial-group -.update)  `state
        `state(previews (~(del by previews) group.update))
      ==
    --
|_  =bowl:gall
+*  this        .
    def         ~(. (default-agent this %|) bowl)
    dep         ~(. (default:pull-hook this config) bowl)
    met         ~(. mdl bowl)
    hc          ~(. +> bowl)
::
++  on-init  
  :_  this
  :~  watch-invites:hc
      watch-contacts:hc
      watch-store:hc
  ==
::
++  on-save  !>(state)
++  on-load  
  |=  =vase
  =+  !<(old=versioned-state vase)
  |^  
  ?-  -.old
    %2  `this(state old)
    ::
      %1
    %_    $
        old
      %*  .  *state-two
        previews  (~(run by previews.old) preview-to-1)
      ==
    ==
    ::
      %0
    %_    $
        old
      %*  .  *state-one
        previews  previews.old
      ==
    ==
  ==
  ::
  ++  metadatum-to-1
    |=  m=metadatum-0
    %*  .  *metadatum:metadata
      title         title.m
      description   description.m
      color         color.m
      date-created  date-created.m
      creator       creator.m
      preview       preview.m
      hidden        %|
    ::
        config
      ?:  =(module.m %$)
        [%group ~]
      [%graph module.m]
    ==
  ::
  ++  preview-to-1
    |=  preview=group-preview-0
    ^-  group-preview:metadata
    %=    preview
      metadatum  (metadatum-to-1 metadatum.preview)
      channels   (associations-to-1 channels.preview)
    ==
  ::
  ++  associations-to-1
    |=  a=associations-0
    ^-  associations:metadata
    %-  ~(run by a)
    |=  [g=resource m=metadatum-0]
    [g (metadatum-to-1 m)]
  --
::
++  on-poke  
  |=  [=mark =vase]
  ?.  ?=(%metadata-hook-update mark)
    (on-poke:def mark vase)
  =+  !<(=hook-update:metadata vase)
  ?.  ?=(%preview -.hook-update)
    (on-poke:def mark vase)
  =:  pending   (~(del in pending) group.hook-update)
        previews
      (~(put by previews) group.hook-update +.hook-update)
    ==
  :_  this
  =/  =path
    preview+(en-path:resource group.hook-update)
  (fact-kick:io path mark^vase)
::
++  on-agent  
  |=  [=wire =sign:agent:gall]
  =^  cards  state
    ?+  wire  (on-agent:def:hc wire sign)
      [%invites ~]        (take-invites:hc sign)
      [%contacts ~]       (take-contacts:hc sign)
      [%store ~]          (take-store:hc sign)
      ::
        [%preview @ @ @ ~]
      ?.  ?=(%poke-ack -.sign)
        (on-agent:def:hc wire sign)
      ?~  p.sign  `state
      =/  rid
        (de-path:resource t.wire)
      :_  state(pending (~(del in pending) rid))
      (fact-kick:io wire tang+!>(u.p.sign))
    ==
  [cards this]
::
++  on-watch  
  |=  =path
  ?>  (team:title [our src]:bowl)
  ?+  path  (on-watch:def path)
  ::
      [%preview @ @ @ ~]
    =/  rid=resource
      (de-path:resource t.path)
    =/  prev=(unit group-preview:metadata)
      ?^  (peek-metadatum:met %groups rid)  
        (some (get-preview:met rid))
      (~(get by previews) rid)
    ?~  prev
      :_  this(pending (~(put in pending) rid))
      (get-preview rid)^~
    :_  this
    (fact-init:io metadata-hook-update+!>([%preview u.prev]))^~
  ==
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
::
++  on-fail   on-fail:def
++  resource-for-update  resource-for-update:met
++  on-pull-nack
  |=   [=resource =tang]
  ^-  (quip card _this)
  =/  =associations:metadata
    (metadata-for-group:met resource)
  :_  this
  %+  turn  ~(tap by associations)
  |=  [=md-resource:metadata =association:metadata]
  %+  poke-our:pass:io:hc  %metadata-store
  :-  %metadata-update-2
  !>  ^-   update:metadata
  [%remove resource md-resource]
::
++  on-pull-kick
  |=  =resource
  ^-  (unit path)
  `/
--
