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
/-  *metadata-store, contacts=contact-store, *group
/+  default-agent, dbug, resource
~%  %sane-app  ..is  ~
|%
+$  card        card:agent:gall
+$  state-zero  [%0 =check]
+$  issues      (list tank)
+$  check       ?(%off %check-print %check-remove)
--
::
=|  state-zero
=*  state  -
::
%-  agent:dbug
^-  agent:gall
=<
|_  =bowl:gall
+*  this  .
    sc    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  
  ^-  (quip card _this)
  :_  this
  ~[subscribe-to-agent-builds]
::
++  on-save  !>(state)
::
++  on-load
  |=  =vase
  =/  old  !<(state-zero vase)
  `this(check check.old)
::
++  on-poke  
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?.  =(%noun mark)
    (on-poke:def mark vase)
  =/  =^check  !<(^check vase)
  :_  this(check check)
  (check-sane:sc check)
::
++  on-peek   
  |=  =path
  ^-  (unit (unit cage))
  ?:  ?=([%x %bad-path ~] path)
    ~
  (on-peek:def path)
::
++  on-arvo   
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?>  ?=([%rebuilds ~] wire)
  ?>  ?=([%c %wris *] sign-arvo)
  :_  this
  :-  subscribe-to-agent-builds
  (check-sane:sc check)
::
++  on-agent  on-agent:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
::
|_  =bowl:gall
++  check-sane
  |=  =^check
  ^-  (list card)
  |^
  ?:  ?=(%off check)   ~
  =/  checked-issues=issues
    %-  zing
    :~  store-hook-desync
        metadata-group-desync
        contact-group-desync
    ==
  ?~  checked-issues  ~
  ?-  check
    %check-print   (print 3 checked-issues)
    %check-remove  ~
  ==
  ::
  ::  +store-hook-desync: check desync between store and hook
  ::
  ::    check desync between store and hook for contacts,
  ::    metadata, group and graph
  ++  store-hook-desync
    |^  ^-  issues
    %+  weld
      metadata-desync
    contact-desync
    ::
    ++  metadata-desync
      ^-  issues
      =/  groups=(set path)
        =-  ~(key by -)
        (scry (jug path md-resource) /y/metadata-store/group-indices)
      =/  group-syncs
        (scry (set path) /x/metadata-hook/synced/noun)
      =/  desyncs=(set path)
        (xor groups group-syncs)
      (report-desync %metadata-store desyncs)
    ::
    ++  contact-desync
      ^-  issues
      =/  contact-syncs
        (scry (set path) /x/contact-hook/synced/noun)
      =/  desyncs
        %+  xor
          contact-syncs
        contact-store-paths
      (report-desync %contact-store desyncs)
    ::
    ++  report-desync
      |=  [app=term paths=(set path)]
      ^-  issues
      %+  turn  ~(tap in paths)
      |=  =path
      `tank`leaf+"store-hook desync: {<app>}: {<path>}"
    --
  ::
  ::  +metadata-group-desync: check desync between metadata and groups
  ::
  ++  metadata-group-desync
    ^-  issues
    =/  metadata-store-paths=(set path)
      ~(key by (scry (jug path md-resource) /y/metadata-store/group-indices))
    =/  desyncs
      %+  xor
        group-store-paths
      metadata-store-paths
    %+  turn  ~(tap in desyncs)
    |=  =path
    leaf+"metadata-group-desync: {<path>}"
  ::
  ::  +contact-group-desync: check desync between contacts and groups
  ::
  ++  contact-group-desync
    |^  ^-  issues
    =/  desyncs
      %+  xor
        managed-group-store-paths
      contact-store-paths
    %+  turn  ~(tap in desyncs)
    |=  =path
    leaf+"contact-group-desync: {<path>}"
    ::
    ++  managed-group-store-paths
      %-  silt
      ^-  (list path)
      %+  murn  ~(tap in group-store-paths) 
      |=  =path
      ^-  (unit ^path)
      =/  scry-pax=^path
        :(weld /x/group-store/groups path /noun)
      ?:  hidden:(need (scry (unit group) scry-pax))
        ~
      `path
    --
  ::
  ++  print
    |=  [pri=@ud =issues]
    ^-  (list card)
    ::  TODO
    ~
  ::
  ++  xor
    |*  [a=(set) b=(set)]
    (~(uni in (~(dif in a) b)) (~(dif in b) a))
  ::
  ++  contact-store-paths
    ^-  (set path)
    %-  %~  del  in
      ~(key by (scry rolodex:contacts /x/contact-store/all/noun))
    /~/default
  ::
  ++  group-store-paths
    ^-  (set path)
    %-  silt
    %+  turn
      ^-  (list resource)
      ~(tap in (scry (set resource) /y/group-store/groups))
    en-path:resource
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
::
++  subscribe-to-agent-builds
  ^-  card
  =-  [%pass /rebuilds %arvo %c %warp our.bowl %home ~ %mult -]
  ^-  mool:clay
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
--
