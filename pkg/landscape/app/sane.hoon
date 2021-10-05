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
::
/-  *metadata-store, contacts=contact-store, *group
/+  default-agent, verb, dbug, resource, graph, mdl=metadata, group
~%  %sane-app  ..part  ~
|%
+$  card  card:agent:gall
::
+$  state-zero  [%0 ~]
::
+$  issue  
  $%  [%lib-pull-hook-desync app=term =resource]
      [%lib-push-hook-desync app=term =resource]
      [%dangling-md =resource]
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
+*  this       .
    sane-core  +>
    sc         ~(. sane-core bowl)
    def        ~(. (default-agent this %|) bowl)
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
      %fix    fix-sane:sc
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
  ?:  ?=([%x %bad-path ~] path)  ~
  (on-peek:def path)
::
++  on-arvo  on-arvo:def
++  on-fail   on-fail:def
--
::
|_  =bowl:gall
++  gra  ~(. graph bowl)
++  md   ~(. mdl bowl)
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
    (zing (turn issues fix-issue))
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
    =>  (lib-hooks-desync %graph scry-graphs)
    =>  (lib-hooks-desync %metadata scry-groups)
    =>  contacts
    metadata
  ::
  ++  contacts
    ^+  fk-core
    =/  groups=(list resource)
       ~(tap in scry-groups)
    |-
    ?~  groups
      fk-core
    =*  group  i.groups
    =?  fk-core
      ?&  (is-managed:grp group)
          !=(our.bowl entity.group)
          !(~(has in (tracking-pull-hook %contact-pull-hook)) group)
      ==
      (report %lib-pull-hook-desync %contact-pull-hook group)
    $(groups t.groups)
  ::
  ++  metadata
    ^+  fk-core
    =/  md-groups=(list resource)
      ~(tap in ~(key by md-group-indices))
    |- 
    ?~  md-groups
      fk-core
    =?  fk-core  !(~(has in scry-groups) i.md-groups)
      (report %dangling-md i.md-groups)
    $(md-groups t.md-groups)
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
  ~&  issues
  `state
::
++  fix-issue
  |=  =issue
  |^
  ^-  (list card)
  ?-   -.issue 
    ::
      %lib-pull-hook-desync
    =*  rid  resource.issue
    (poke-our app.issue pull-hook-action+!>([%add entity.rid rid]))^~
    ::
      %lib-push-hook-desync
    (poke-our app.issue push-hook-action+!>([%add resource.issue]))^~
    ::
      %dangling-md
    =/  app-indices
      (~(get ju md-group-indices) resource.issue)
    %+  turn
      ~(tap in app-indices)
    |=  =md-resource
    ^-  card
    (poke-our %metadata-store metadata-action+!>([%remove resource.issue md-resource]))
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
++  scry-graphs
  =+  .^   app-indices=(jug term [resource resource])
        %gy
        (scot %p our.bowl)
        %metadata-store
        (scot %da now.bowl)
        /app-indices
      ==
  =/  resources=(set [resource resource])
    (~(get ju app-indices) %graph)
  %-  ~(int in get-keys:gra)
  (~(gas in *(set resource)) (turn ~(tap in resources) |=([resource r=resource] r)))
::
++  md-group-indices
  (scry (jug resource md-resource) /y/metadata-store/group-indices)
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
