::  group-store: Store groups of ships
::
::    group-store stores groups of ships, so that resources in other apps can be
::    associated with a group. The current model of group-store rolls
::    permissions and invites inside this store for simplicity reasons, although
::    these should be prised apart in a future revision of group store.
::
::    ## Scry paths
::
::    /y/groups:
::      A listing of the current groups
::    /y/groups/[group-id]/tag-queries:
::      A listing of the tag queries for a group
::    /x/groups/[group-id]:
::      The group itself
::    /x/groups/[group-id]/tag-queries/[tag]:
::      The subset with tag
::    /x/groups/[group-id]/permitted/[ship]:
::      A flag indicated if the ship is permitted
::
::
::    ## Subscription paths
::
::    /groups:
::      A stream of the current updates to the state, sending the initial state
::      upon subscribe.
::
::    ##  Pokes
::
::    %group-action:
::      Modify the group. Further documented in /sur/group-store.hoon
::
::
/-  *group
/+  store=group-store, default-agent, verb, dbug
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-zero
      state-one
  ==
::
+$  state-zero
  $:  %0
      * :: =groups
  ==
::
::
+$  state-one
  $:  %1
      =groups
  ==
::
+$  diff
  $%  [%group-update update:store]
      [%group-initial groups]
  ==
--
::
=|  state-one
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this        .
      group-core  +>
      gc          ~(. group-core bowl)
      def         ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    :_  this
    [%pass / %arvo %d %flog %text "{(trip dap.bowl)} started"]~
  ++  on-save   !>(state)
  ++  on-load
    |=  =old=vase
    =/  old  !<(versioned-state old-vase)
    ?.  ?=(%1 -.old)
      `this
    `this(state old)
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    =^  cards  state
      ?:  ?=(%group-update mark)
        (poke-group-update:gc !<(update:store vase))
      (on-poke:def mark vase)
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    ?>  ?=([%groups ~] path)
    :_  this
    [%give %fact ~ %group-initial !>(groups)]~
  ::
  ++  on-leave  on-leave:def
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def path)
      [%x %groups @ @ ~]  ``noun+!>((peek-group t.t.path))
    ::
        [%x %groups @ @ %permitted @ ~]
      ``noun+!>((peek-group-permitted t.t.path (slav %p i.t.t.t.t.t.path)))
    ::
        [%x %groups @ @ %permission-role @ ~]
      ``noun+!>((peek-group-permitted t.t.path (slav %p i.t.t.t.t.t.path)))
    ==
  ::
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  bol=bowl:gall
++  ship-rank
  |=  =ship
  ^-  rank
  =/  size
    (met 3 ship)
  ?:  (lte size 2)
    %0
  ?:  (lte size 4)
    %1
  ?:  (lte size 8)
    %2
  %3
++  peek-group-permitted
  |=  [=path =ship]
  ^-  ?
  =/  maybe-group
    (peek-group path)
  ?~  maybe-group
    %.n
  =*  group  u.maybe-group
  =*  policy  policy.group
  ?-  -.policy
      %invite
    |((~(has in pending.policy) ship) (~(has in members.group) ship))
      %open
    &(!(~(has in banned.policy) ship) (~(has in ranks.policy) (ship-rank ship)))
  ==
::
++  peek-group
  |=  =path
  ^-  (unit group)
  =/  m-group-id
    (group-id:de-path:store path)
  ?~  m-group-id  ~
  =*  group-id  u.m-group-id
  (~(get by groups) group-id)
::
++  poke-group-update
  |=  =update:store
  ^-  (quip card _state)
  ?>  (team:title our.bol src.bol)
  |^
  ?-  -.update
      %add-group       (add-group +.update)
      %add-members     (add-members +.update)
      %remove-members  (remove-members +.update)
      %add-tag         (add-tag +.update)
      %remove-tag      (remove-tag +.update)
      %change-policy   (change-policy +.update)
      %remove-group    (remove-group +.update)
      %initial-group   (initial-group +.update)
      %initial         [~ state]
  ==
  ::  +add-group: add group to store
  ::
  ::    no-op if group-already exists
  ::
  ++  add-group
    |=  [=group-id =group]
    ^-  (quip card _state)
    ?:  (~(has by groups) group-id)
      [~ state]
    =.  groups
      (~(put by groups) group-id group)
    :_  state
    (send-diff %add-group group-id group)
  ::  +add-members: add members to group
  ::
  ::    no-op if group does not exist
  ::
  ++  add-members
    |=  [=group-id new-ships=(set ship) tags=(set tag)]
    ^-  (quip card _state)
    ?.  (~(has by groups) group-id)
      [~ state]
    =/  =group  (~(got by groups) group-id)
    =.  members.group  (~(uni in members.group) new-ships)
    =.  tag-queries.group
      (merge-tags tag-queries.group new-ships tags)
    =.  groups
      (~(put by groups) group-id group)
    :_  state
    (send-diff %add-members group-id new-ships tags)
  ::  +remove-members: remove members from group
  ::
  ::    no-op if group does not exist
  ::    TODO: remove tags as well
  ::
  ++  remove-members
    |=  [=group-id ships=(set ship)]
    ^-  (quip card _state)
    ?.  (~(has by groups) group-id)
      [~ state]
    =/  =group
      (~(got by groups) group-id)
    =.  members.group
      (~(dif in members.group) ships)
    =.  groups
      (~(put by groups) group-id group)
    :_   state
    (send-diff %remove-members group-id ships)
  ::  +add-tag: add tag to ships
  ::
  ::    no-op if group does not exist
  ::    crash if ships are not in group (is this right?)
  ::
  ++  add-tag
    |=  [=group-id =tag ships=(set ship)]
    ^-  (quip card _state)
    ?:  (~(has by groups) group-id)
      [~ state]
    =/  =group
      (~(got by groups) group-id)
    ?>  ?=(~ (~(dif in ships) members.group))
    =.  tag-queries.group
      (merge-tags tag-queries.group ships (sy tag ~))
    =.  groups
      (~(put by groups) group-id group)
    :_  state
    (send-diff %add-tag group-id tag ships)
  ::  +remove-tag: remove tag from ships
  ::
  ::    no-op if group does not exist
  ::    crash if ships are not in group or tag does not exist (is this right?)
  ::
  ++  remove-tag
    |=  [=group-id =tag ships=(set ship)]
    ^-  (quip card _state)
    ?:  (~(has by groups) group-id)
      [~ state]
    =/  =group
      (~(got by groups) group-id)
    ?>  ?&  ?=(~ (~(dif in ships) members.group))
            (~(has by tag-queries.group) tag)
        ==
    =/  tag-query
      (~(got by tag-queries.group) tag)
    =.  tag-query
      (~(dif in tag-query) ships)
    =.  tag-queries.group
      (~(put by tag-queries.group) tag tag-query)
    :_  state
    (send-diff %remove-tag group-id tag ships)
  ::  initial-group: initialize foreign group
  ::
  ++  initial-group
    |=  [=group-id =group]
    ^-  (quip card _state)
    =.  groups
      (~(put by groups) group-id group)
    :_  state
    (send-diff %initial-group group-id group)
  ::  +change-policy: modify group access control
  ::
  ::
  ++  change-policy
    |=  [=group-id =diff:policy]
    ^-  (quip card _state)
    ?.  (~(has by groups) group-id)
      [~ state]
    =/  =group
      (~(got by groups) group-id)
    =*  policy  policy.group
    |^
    =.  policy
      =-  ~&  -  -
      ?+  -.diff  !!
        %allow-ranks  (allow-ranks +.diff)
        %ban-ranks  (ban-ranks +.diff)
        %allow-ships  (allow-ships +.diff)
        %ban-ships  (ban-ships +.diff)
      ==
    =.  groups
      (~(put by groups) group-id group)
    :_  state
    (send-diff %change-policy group-id diff)
    ::
    ++  allow-ranks
      |=  ranks=(set rank)
      ^-  ^policy
      ?>  ?=(%open -.policy)
      =.  ranks.policy
        (~(uni in ranks.policy) ranks)
      policy
    ::
    ++  ban-ranks
      |=  ranks=(set rank)
      ^-  ^policy
      ?>  ?=(%open -.policy)
      =.  ranks.policy
        (~(dif in ranks.policy) ranks)
      policy
    ::
    ++  allow-ships
      |=  ships=(set ship)
      ^-  ^policy
      ?>  ?=(%open -.policy)
      =.  banned.policy
        (~(dif in banned.policy) ships)
      policy
    ::
    ++  ban-ships
      |=  ships=(set ship)
      ^-  ^policy
      ?>  ?=(%open -.policy)
      =.  banned.policy
        (~(uni in banned.policy) ships)
      =.  members.group
        (~(dif in members.group) banned.policy)
      policy
    --
  ::  +remove-group: remove group from store
  ::
  ::    no-op if group does not exist
  ++  remove-group
    |=  [=group-id ~]
    ^-  (quip card _state)
    ?.  (~(has by groups) group-id)
      `state
    =.  groups
      (~(del by groups) group-id)
    :_  state
    (send-diff %remove-group group-id ~)
  ::
  --
::  +merge-tags: merge tags
++  merge-tags
  |=  [=tag-queries ships=(set ship) tags=(set tag)]
  ^+  tag-queries
  =/  tags  ~(tap in tags)
  |-
  ?~  tags
    tag-queries
  =*  tag  i.tags
  =/  current-query=(set ship)
    (~(gut by tag-queries) tag ~)
  %=    $
    tags  t.tags
  ::
      tag-queries
    %+  ~(put by tag-queries)
      tag
    (~(uni in current-query) ships)
  ==
::  +send-diff: update subscribers of new state
::
::    We only allow subscriptions on /groups
::    so just give the fact there.
++  send-diff
  |=  =update:store
  ^-  (list card)
  [%give %fact ~[/groups] %group-update !>(update)]~
::
--
