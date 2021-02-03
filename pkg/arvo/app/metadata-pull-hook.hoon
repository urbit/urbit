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
++  config
  ^-  config:pull-hook
  :*  %metadata-store
      update:metadata
      %metadata-update
      %metadata-push-hook
      %.n
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
        :_  state
        ?.  ?=(%invite -.update)  ~
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
        :_  state
        ?>  ?=(%contact-update p.cage.sign)
        =+  !<(=update:contact q.cage.sign)
        ?+  -.update  ~
            %add
          (check-contact contact.update)
        ::
            %edit
          ?.  ?=(%add-group -.edit-field.update)  ~
          %-  add-missing-previews 
          (~(gas in *(set resource)) resource.edit-field.update ~)
        ::
            %initial
          ^-  (list card)
          %-  zing
          %+  turn  ~(tap by rolodex.update)
          |=([ship =contact:contact] (check-contact contact))
        ==
      ==
    ::
    ++  check-contact
      |=  =contact:contact
      ^-  (list card)
      (add-missing-previews groups.contact)
    ::
    ++  add-missing-previews
      |=  groups=(set resource)
      ^-  (list card)
      =/  missing=(set resource)
        (~(dif in ~(key by previews)) groups)
      %+  murn  ~(tap by missing)
      |=  group=resource
      ^-  (unit card)
      ?^  (peek-metadatum:met %groups group)  ~
      `(get-preview group)
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
        ?>  ?=(%metadata-update p.cage.sign)
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
++  on-init  on-init:def
++  on-save  !>(state)
++  on-load  
  |=  =vase
  =+  !<(old=state-zero vase)
  :_  this(state old)
  %-  zing
  :~  ?:  (~(has by wex.bowl) [/invites our.bowl %invite-store])  ~
      ~[watch-invites:hc]
      ?:  (~(has by wex.bowl) [/contacts our.bowl %contact-store])  ~
      ~[watch-contacts:hc]
      ?:  (~(has by wex.bowl) [/store our.bowl %metadata-store])  ~
      ~[watch-store:hc]
  ==
::
++  on-poke  
  |=  [=mark =vase]
  ?.  ?=(%metadata-hook-update mark)
    (on-poke:def mark vase)
  =+  !<(=hook-update:metadata vase)
  ?.  ?=(%preview -.hook-update)
    (on-poke:def mark vase)
  :_  this(previews (~(put by previews) group.hook-update +.hook-update))
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
      :_  state
      ?~  p.sign  ~
      (fact-kick:io wire tang+!>(u.p.sign))
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
  (fact-init:io metadata-hook-update+!>([%preview u.prev]))^~
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
  %+  poke-our:pass:io  %metadata-store
  :-  %metadata-update
  !>  ^-   update:metadata
  [%remove resource md-resource]
::
++  on-pull-kick
  |=  =resource
  ^-  (unit path)
  `/
--
