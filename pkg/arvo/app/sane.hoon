::  %sane: sanity checker for the landscape suite of applications
::
::    Userspace currently uses certain identifiers as foreign keys, and
::    expects those foreign keys to exist in a number of locations. 
::
::    These foreign key relationships are prone to breaking during OTAs 
::    and there are enough of them that they rarely get tested for 
::    manually. %sane is a gall app that will check the validity of
::    these relationships. The MVP will just print the errors as it 
::    finds them.
::
::    Pokes:
::      %off - turn off checking on a reload
::      %lax - check on reload, printing errors
::      %strict - check on reload, aborting commit if invalid
::      %check-print  - run a check now, printing errors
::      %check-crash - run a check now, crashing nondeterministically
::        if invalid 
::    
::    Currently validates:
::      - Entries in {contact,metadata,group} stores are in sync with 
::         their hooks
::      - Each group has its associated metadata and contacts
::
::    Future validation: 
::      - Subscriptions are correctly setup for hooks
::      - Graph store integration
::    
::

::
/-  *metadata-store, contacts=contact-store, *group
/+  default-agent, verb, dbug, resource
~%  %sane-app  ..is  ~
|%
+$  card  card:agent:gall
::
+$  state-zero  [%0 =mode]
::
+$  issues  (list tank)
::
+$  mode    ?(%strict %lax %off)
::
+$  action  ?(mode check)
::
+$  check   ?(%check-print %check-crash)
::
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
  :_  this
  ~[subscribe-to-agent-builds]
++  on-save  !>(state)
::
::
++  on-load
  |=  =vase
  =/  old
    !<(state-zero vase)
  `this(mode mode.old)
::
::
++  on-poke  
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?.  =(%noun mark)
    (on-poke:def mark vase)
  ~!  action
  =/  act=action
    !<(action vase)
  ?:  ?=(^mode act)
    `this(mode act)
  %-  (check-sane:sc act)
  `this
::
::
++  on-agent  on-agent:def
::
++  on-watch  on-watch:def
::
++  on-leave  on-leave:def

++  on-peek   
  |=  =path
  ^-  (unit (unit cage))
  ?.  ?=([%x %bad-path ~] path)
    (on-peek:def path)
  ~
::
++  on-arvo   
  |=  [=wire =sign-arvo]
  ?>  ?=([%rebuilds ~] wire)
  ?>  ?=([%c %wris *] sign-arvo)
  =/  ucheck
    check-type:sc 
  :_  this
  %.  ~[subscribe-to-agent-builds]
  ?~  ucheck  same
  (check-sane:sc u.ucheck)
::
++  on-fail   on-fail:def
--
::
|_  =bowl:gall
++  subscribe-to-agent-builds
  ^-  card
  ~&  >>  "Subscribing..."
  =/  =mool:clay
    :-  da+now.bowl
    %-  ~(gas in *(set [care:clay path]))
    :~  [%a /app/metadata-hook/hoon]
        [%a /app/metadata-store/hoon]
        [%a /app/group-store/hoon]
        [%a /app/group-pull-hook/hoon]
        [%a /app/group-push-hook/hoon]
        [%a /app/contact-store/hoon]
        [%a /app/contact-hook/hoon]
    ==
  [%pass /rebuilds %arvo %c %warp our.bowl %home ~ %mult mool]
::
++  check-type
  ^-  (unit check)
  ?+  mode  ~
    %strict  `%check-crash
    %lax     `%check-print
  ==
::
++  print
  |=  [pri=@ud =issues]
  ^+  same
  %.  same
  %-  %*(. slog pri pri)
  issues
::
++  xor
  |*  [a=(set) b=(set)]
  %-  
    %~  uni  in  (~(dif in a) b)
  (~(dif in b) a)
::
++  check-sane
  |=  =check
  ^+  same
  =/  =issues
    ;:  weld
      store-hook-desync
      metadata-group-desync
      contact-group-desync
    ==
  ?~  issues
    ((print 1 ~[leaf+"Sane!"]) same)
  %-  (print 3 issues)
  ?:  =(%check-print check)
    same
  =/  failure
    ~|  %crashing-to-abort-merge
    (bex (bex 256))
  same
::  +store-hook-desync: check desync between store and hookk
::
::    check desync between store and hook for contacts,
::    metadata, group and graph
++  store-hook-desync
  ^-  issues
  =|  =issues
  |^
  =.  issues
    metadata-desync
  =.  issues
    contact-desync
  issues
  ++  report-desync
    |=  [app=term =(set path)]
    ^+  issues
    %+  weld
      issues
    %+  turn
      ~(tap in set)
    |=  =path
    `tank`leaf+"store-hook desync: {<app>}: {<path>}"
  ::
  ++  metadata-desync
    ^+  issues
    =/  groups=(set path)
      =-  ~(key by -)
      (scry (jug path md-resource) /y/metadata-store/group-indices)
    =/  group-syncs
      (scry (set path) /x/metadata-hook/synced/noun)
    =/  desyncs=(set path)
      (xor groups group-syncs)
    (report-desync %metadata-store desyncs)
  ++  contact-desync
    ^+  issues
    =/  contacts
      contact-store-paths
    =/  contact-syncs
      (scry (set path) /x/contact-hook/synced/noun)
    =/  desyncs
      (xor contact-syncs contacts)
    (report-desync %contact-store desyncs)
  --
::  +metadata-group-desync: check desync between metadata and groups
::
++  metadata-group-desync
  ^-  issues
  =/  groups=(set path)
    group-store-paths
  =/  metadata
    metadata-store-paths
  =/  desyncs
    (xor groups metadata)
  %+  turn
    ~(tap in desyncs)
  |=  =path
  leaf+"metadata-group-desync: {<path>}"
::  +contact-group-desync: check desync between contacts and groups
::
++  contact-group-desync
  ^-  issues
  =/  groups=(set path)
    managed-group-store-paths
  =/  contacts
    contact-store-paths
  =/  desyncs
    (xor contacts groups)
  %+  turn
    ~(tap in desyncs)
  |=  =path
  leaf+"contact-group-desync: {<path>}"
::
++  contact-store-paths
  ^-  (set path)
  %-  %~  del  in
    ~(key by (scry rolodex:contacts /x/contact-store/all/noun))
  /~/default
::
++  metadata-store-paths
  ^-  (set path)
  ~(key by (scry (jug path md-resource) /y/metadata-store/group-indices))
::
++  group-store-paths
  ^-  (set path)
  %-  sy
  %+  turn
    ^-  (list resource)
    ~(tap in (scry (set resource) /y/group-store/groups))
  en-path:resource
::
++  managed-group-store-paths
  %-  sy
  ^-  (list path)
  %+  murn
     ~(tap in group-store-paths) 
  |=  =path
  ^-  (unit ^path)
  =/  scry-pax=^path
    :(weld /x/group-store/groups path /noun)
  ?:  hidden:(need (scry (unit group) scry-pax))
    ~
  `path
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
