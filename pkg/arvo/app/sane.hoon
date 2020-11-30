::  %sane: sanity checker for the landscape suite of applications
::
::    Userspace currently uses certain identifiers as foreign keys, and
::    expects those foreign keys to exist in a number of locations. 
::
::    These foreign key relationships are prone to breaking during OTAs 
::    and there are enough of them that they rarely get tested for 
::    manually. %sane is a gall app that will check the validity of
::    these relationships, and fix them if asked.
::
::    Sane has a companion thread, -sane, which should be run instead
::    of attempting :sane %fix directly from the dojo.
::
::    Pokes:
::      %fix - Find issues and fix them
::      %check - Find issues and print them
::
::    Currently validates:
::      - Entries in {contact,metadata,group} stores are in sync with 
::         their hooks
::      - Each group has its associated metadata and contacts
::      - Each graph is being synced
::      - Each chat is being synced
::
/-  *metadata-store, contacts=contact-store, *group
/+  default-agent, verb, dbug, resource, graph, mdl=metadata, group
~%  %sane-app  ..is  ~
|%
+$  card  card:agent:gall
::
+$  state-zero  [%0 ~]
::
+$  issue  
  $%  [%lib-pull-hook-desync app=term =resource]
      [%lib-push-hook-desync app=term =resource]
      [%md-hook-desync =path]
      [%contact-hook-desync =path]
      [%chat-desync =path]
  ==
::
+$  issues
  (list issue)
::
+$  action  ?(%check %fix)
--
::
=|  state-zero
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
|_  =bowl:gall
+*  this        .
    sane-core  +>
    sc          ~(. sane-core bowl)
    def         ~(. (default-agent this %|) bowl)
::
++  on-init  
  ^-  (quip card _this)
  `this
++  on-save  !>(state)
::
++  on-load
  |=  =vase
  `this
::
++  on-poke  
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?.  =(%noun mark)
    (on-poke:def mark vase)
  =/  act=action  !<(action vase)
  =^  cards  state
    ?-  act  
      %fix  fix-sane:sc
      %check  print-sane:sc
    ==
  [cards this]
::
++  on-agent  on-agent:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
::
++  on-peek   
  |=  =path
  ^-  (unit (unit cage))
  ?.  ?=([%x %bad-path ~] path)
    (on-peek:def path)
  ~
::
++  on-arvo  on-arvo:def
++  on-fail   on-fail:def
--
::
|_  =bowl:gall
::
++  gra  ~(. graph bowl)
::
++  md   ~(. mdl bowl)
::
++  grp  ~(. group bowl)
::
++  foreign-keys
  |_  =issues
  ++  fk-core  .
  ::
  ++  abet
    ^+  issues
    issues
  ::
  ++  abet-fix
    ^-  (list card)
    (turn issues fix-issue)
  ::
  ++  report
    |=  =issue
    fk-core(issues (snoc issues issue))
  ::
  ++  report-many
    |=  many=^issues
    fk-core(issues (weld issues many))
  ::
  ++  check-all
    =>  (lib-hooks-desync %group scry-groups)
    =>  (lib-hooks-desync %graph get-keys:gra)
    groups
  ::
  ++  chat
    ^+  fk-core
    =/  missing=(set path)
      (~(dif in scry-chats) scry-chat-syncs)
    %-  report-many
    %+  turn
      ~(tap in missing)
    |=  =path
    ^-  issue
    [%chat-desync path]
  ::
  ++  groups
    ^+  fk-core
    =/  groups=(list resource)
       ~(tap in scry-groups)
    |-
    ?~  groups
      fk-core
    =*  group  i.groups
    =?  fk-core  !(~(has in scry-md-syncs) group)
      (report %md-hook-desync (en-path:resource group))
    =?  fk-core  &((is-managed:grp group) !(~(has in scry-contact-syncs) group))
      (report %contact-hook-desync (en-path:resource group))
    $(groups t.groups)
  ::
  ++  lib-hooks-desync
    |=  [app=term storing=(set resource)]
    ^+  fk-core
    =/  tracking
      (tracking-pull-hook (pull-hook-name app))
    =/  sharing
      (sharing-push-hook (push-hook-name app))
    =/  resources
      ~(tap in storing)
    |- 
    ?~  resources
      fk-core
    =*  rid  i.resources
    =?  fk-core  &(=(our.bowl entity.rid) !(~(has in sharing) rid))
      (report %lib-push-hook-desync (push-hook-name app) rid)
    =?  fk-core  &(!=(our.bowl entity.rid) !(~(has in tracking) rid))
      (report %lib-pull-hook-desync (pull-hook-name app) rid)
    $(resources t.resources)
  --
::
++  pull-hook-name
  |=  app=term
  :(join-cord app '-' %pull-hook)
::
++  push-hook-name
  |=  app=term
  :(join-cord app '-' %push-hook)
::
++  fix-sane
  ^-  (quip card _state)
  =/  cards=(list card)
    =>  foreign-keys
    =>  check-all
    abet-fix
  [cards state]
::
++  print-sane
  ^-  (quip card _state)
  =/  =issues
    =>  foreign-keys
    =>  check-all
    abet
  `state
::
++  fix-issue
  |=  =issue
  ^-  card
  |^
  ?-   -.issue 
    ::
      %lib-pull-hook-desync
    =*  rid  resource.issue
    (poke-our app.issue pull-hook-action+!>([%add entity.rid rid]))
    ::
      %lib-push-hook-desync
    (poke-our app.issue push-hook-action+!>([%add resource.issue]))
    ::
      %md-hook-desync
    =/  rid=resource
      (de-path:resource path.issue)
    =/  act
      ?:  =(entity.rid our.bowl)
        [%add-owned path.issue]
      [%add-synced entity.rid path.issue]
    (poke-our %metadata-hook metadata-hook-action+!>(act))
    ::
      %contact-hook-desync
    =/  rid=resource
      (de-path:resource path.issue)
    =/  act
      ?:  =(entity.rid our.bowl)
        [%add-owned path.issue]
      [%add-synced entity.rid path.issue]
    (poke-our %contact-hook contact-hook-action+!>(act))
      %chat-desync
    =/  =ship
      (slav %p (snag 0 path.issue))
    =/  act
      ?:  =(ship our.bowl)
        [%add-owned path.issue %.n]
      [%add-synced ship path.issue %.n]
    (poke-our %chat-hook chat-hook-action+!>(act))
  ==
  ::
  ++  poke-our
    |=  [app=term =cage]
    ^-  card
    [%pass /fix %agent [our.bowl app] %poke cage]
  --
::
++  join-cord
  (cury cat 3)
::
++  scry-groups
  (scry ,(set resource) /y/group-store/groups)
::
++  tracking-pull-hook
  |=  hook=term
  %+  scry
    ,(set resource)
  /x/[hook]/tracking/noun
::
++  sharing-push-hook
  |=  hook=term
  %+  scry
    ,(set resource)
  /x/[hook]/sharing/noun
::
++  scry-md-syncs
  ^-  (set resource)
  =-  (~(run in -) de-path:resource)
  %+  scry
    ,(set path)
  /x/metadata-hook/synced/noun
::
++  scry-contact-syncs
  ^-  (set resource)
  =-  (~(run in -) de-path:resource)
  %+  scry
    ,(set path)
  /x/contact-hook/synced/noun
::
++  scry-chat-syncs
  ^-  (set path)
  %+  scry
    ,(set path)
  /x/chat-hook/synced/noun
::
++  scry-chats
  ^-  (set path)
  %+  scry
    ,(set path)
  /x/chat-store/keys/noun
::
++  scry
  |*  [=mold =path]
  ^-  mold
  ?>  ?=(^ path)
  ?>  ?=(^ t.path)
  .^  mold
    (cat 3 %g i.path)
    (scot %p our.bowl)
    i.t.path
    (scot %da now.bowl)
    t.t.path
  ==
--
