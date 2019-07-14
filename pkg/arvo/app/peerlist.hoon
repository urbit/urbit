::  peerlist: (manual) userspace peer discovery
::
::    a drop-in solution to peer discovery for peer-to-peer applications.
::    for usage, see +local-poke.
::
/-  *peerlist
::
|%
+$  state
  $:  ::  peers: all relations
      ::
      =peers
      ::  groups: locally defined clusters
      ::
      =groups
      ::  settings: behavior configuration
      ::
      =settings
  ==
::
+$  filter
  $%  ::  %all: all relations
      ::
      %all
      ::  %ours: relations where ?=(^ we-since)
      ::
      %ours
      ::  %theirs: relations where ?=(^ they-since)
      ::
      %theirs
      ::  %mutuals: only mutual relations
      ::
      %mutuals
      ::  %none: no relations
      ::
      %none
  ==
::
+$  move  [bone card]
+$  card
  $%  [%poke wire dock %peerlist-foreign-poke foreign-poke]
      [%info wire toro:clay]
  ==
--
::
::
|_  [=bowl:gall state]
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  [(list move) _this]
  ?~  old
    [~ this]
  [~ this(+<+ u.old)]
::
::  utilities
::
++  is-mutual
  |=  relation
  &(?=(^ we-since) ?=(^ they-since))
::
++  groups-with
  |=  who=ship
  %-  ~(gas in *(set tag))
  %+  murn  ~(tap by groups)
  |=  [=tag =group]
  ^-  (unit ^tag)
  ?.  (~(has in group) who)  ~
  `tag
::
++  default-relation
  %*(. *relation public default-public.settings)
::
::  management
::
++  do-add
  |=  [who=ship group=(unit tag)]
  =/  old=relation
    (~(gut by peers) who default-relation)
  =/  new=?  ?=(~ we-since.old)
  =?  peers  new
    %+  ~(put by peers)  who
    old(we-since `now.bowl)
  =?  groups  ?=(^ group)
    (~(put ju groups) u.group who)
  :_  this
  ?.  new  ~
  [(peer who) ~]
::
++  do-remove
  |=  [who=ship from=(unit tag)]
  =/  have=(unit [who=ship =relation groups=(set tag)])
    =+  old=(~(get by peers) who)
    ?~  old  ~
    `[who u.old (groups-with who)]
  ?^  from
    ::  remove from one group
    ::
    ~&  "removing {(scow %p who)} from %{(trip u.from)}"
    =*  from-group=tag  u.from
    =-  [~ this(groups -)]
    (~(del ju groups) from-group who)
  ::  remove as peer
  ::
  ~&  "dropping {(scow %p who)}"
  =?  peers  ?=(^ have)
    ?~  they-since.relation.u.have
      ::  if they don't care, remove relation
      ::
      (~(del by peers) who)
    ::  if they still care, keep them registered
    ::
    %+  ~(put by peers)  who
    relation.u.have(we-since ~)
  ::  & remove from all groups
  ::
  =.  groups
    %-  ~(gas by *^groups)
    %+  murn  ~(tap by groups)
    |=  [=tag =group]
    ^-  (unit _+<)
    =+  (~(del in group) who)
    ?:(=(~ -) ~ `[tag -])
  :_  this
  ::  only send drop poke if we used to care
  ::
  ?.  ?&  ?=(^ have)
          ?=(^ we-since.relation.u.have)
      ==
    ~
  [(drop who) ~]
::
++  do-fill
  |=  =tag
  =-  [~ this(groups -)]
  (~(put by groups) tag ~(key by peers))
::
++  do-clear
  |=  =tag
  [~ this(groups (~(del by groups) tag))]
::
++  do-reset
  =/  byes=(list ship)  ~(tap in ~(key by peers))
  =|  moz=(list move)
  |-
  ?~  byes  [moz this]
  =^  mon  this  (do-remove i.byes ~)
  $(moz (weld mon moz), byes t.byes)
::
++  do-export
  |=  [=spur which=(set tag)]
  =-  [[[ost.bowl %info [%export spur] -] ~] this]
  %+  foal:space:userlib
    :*  (scot %p our.bowl)
        q.byk.bowl
        (scot %da now.bowl)
        (weld spur /peerlist-groups)
    ==
  :-  %peerlist-groups
  !>
  ?~  which  groups
  %-  ~(gas by *^groups)
  %+  skim  ~(tap by groups)
  ::TODO  (cork head ~(has in `(set tag)`which))
  |=  [=tag group]
  ::NOTE  cast because tmi
  (~(has in `(set ^tag)`which) tag)
::
++  do-import
  |=  =path
  ^-  [(list move) _this]
  =/  file  .^(^groups %cx (weld path /peerlist-groups))
  =/  load=(list [=tag =ship])
    ::TODO  tap:ju
    %-  zing
    %+  turn  ~(tap by file)
    |=  [=tag =group]
    ^-  (list [^tag ship])
    %+  turn  ~(tap in group)
    ::TODO  tack
    |=(=ship [tag ship])
  ::
  =|  moz=(list move)
  |-
  ?~  load  [moz this]
  =^  mon  this  (do-add [ship `tag]:i.load)
  $(moz (weld mon moz), load t.load)
::
++  do-settings
  |=  new=^settings
  [~ this(settings new)]
::
::  move construction
::
++  peer
  (cury make-poke %peer now.bowl)
::
++  drop
  (cury make-poke %drop ~)
::
++  make-poke
  |=  [poke=foreign-poke who=ship]
  ^-  move
  [ost.bowl %poke / [who dap.bowl] %peerlist-foreign-poke poke]
::
::  interaction interface
::
++  poke-noun
  |=  poke=*
  ^-  [(list move) _this]
  ~|  %weird-poke-data
  =/  poke  ;;($%(local-poke foreign-poke) poke)
  ?+  -.poke
    ?.  =(src.bowl our.bowl)  [~ this]
    (poke-peerlist-local-poke poke)
  ::
      ?(%peer %drop)
    ?:  =(src.bowl our.bowl)  [~ this]
    (poke-peerlist-foreign-poke poke)
  ==
::
++  poke-peerlist-local-poke
  |=  poke=local-poke
  ?.  =(our.bowl src.bowl)  [~ this]
  ~&  [%poked -.poke]
  ^-  [(list move) _this]
  ?-  -.poke
    %add       (do-add +.poke)
    %remove    (do-remove +.poke)
    %fill      (do-fill +.poke)
    %clear     (do-clear +.poke)
    %reset     do-reset
    %export    (do-export +.poke)
    %import    (do-import +.poke)
    %settings  (do-settings +.poke)
  ::
      %debug
    ~&  peers+peers
    ~&  groups+groups
    [~ this]
  ==
::
++  poke-peerlist-foreign-poke
  |=  poke=foreign-poke
  ~&  [%foreign-poke src.bowl -.poke]
  ^-  [(list move) _this]
  =*  who=@p  src.bowl
  =/  =relation
    (~(gut by peers) who default-relation)
  ?-  -.poke
      %peer
    ~&  "{(scow %p who)} added you as a peer"
    =-  [~ this(peers -)]
    %+  ~(put by peers)  who
    relation(they-since `since.poke)
  ::
      %drop
    ~&  "{(scow %p who)} removed you as a peer"
    =-  [~ this(peers -)]
    ?~  we-since.relation
      (~(del by peers) who)
    %+  ~(put by peers)  who
    relation(they-since ~)
  ==
::
::  data interface
::
::    /peers/(filter)/(group)
::      local-only, all peers that match optional filter and group
::    /groups/(tag)
::      local-only, all groups, or group with tag
::    /peeps
::      all of /peers/ours that are public
::
++  peers-result
  |=  [=filter tag=(unit tag)]
  ^+  peers
  %-  ~(gas by *(map ship relation))
  %+  skim  ~(tap by peers)
  |=  [who=ship relation]
  ?&  ?-  filter
        %all      &
        %ours     ?=(^ we-since)
        %theirs   ?=(^ they-since)
        %mutuals  &($(filter %ours) $(filter %theirs))
        %none     |
      ==
    ::
      ?|  ?=(~ tag)
          (~(has ju groups) u.tag who)
  ==  ==
::
++  groups-result
  |=  tag=(unit tag)
  ^+  groups
  ?~  tag  groups
  [u.tag^(~(get ju groups) u.tag) ~ ~]
::
++  peeps-results
  %-  ~(gas by *(map ship @da))
  %+  murn  ~(tap by (peers-result %ours ~))
  |=  [who=ship relation]
  ^-  (unit [ship @da])
  ?~  we-since  ~
  ?.  public  ~
  `[who u.we-since]
::
++  peek-x
  |=  =path
  ^-  (unit (unit [%noun noun]))
  ?+  path  ~
      [%peers ?(~ [@ ~] [@ @ ~])]
    ?.  =(src.bowl our.bowl)  [~ ~]
    :+  ~  ~
    :-  %noun
    =*  args  t.path
    %-  peers-result
    ?~  args  [%all ~]
    ?.  ?=(filter i.args)  [%none ~]
    :-  i.args
    ?~  t.args  ~
    `i.t.args
  ::
      [%groups ?(~ [@ ~])]
    ?.  =(src.bowl our.bowl)  [~ ~]
    :+  ~  ~
    :-  %noun
    =*  args  t.path
    %-  groups-result
    ?~(args ~ `i.args)
  ::
      [%peeps ~]
    ``noun+peeps-results
  ==
--
