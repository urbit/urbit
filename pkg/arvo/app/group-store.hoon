::  group-store: Store groups of ships
::
::    group-store stores groups of ships, so that resources in other apps can be
::    associated with a group. The current model of group-store rolls
::    permissions and invites inside this store for simplicity reasons, although
::    these should be prised apart in a future revision of group store.
::
::
::    ## Scry paths
::
::    /y/groups:
::      A listing of the current groups
::    /x/groups/[resource]:
::      The group itself
::    /x/groups/[resource]/join/[ship]:
::      A flag indicated if the ship is permitted to join
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
/-  *group, permission-store
/+  store=group-store, default-agent, verb, dbug, resource
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
      =groups:state-zero:store
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
  ++  on-init  on-init:def
  ++  on-save   !>(state)
  ++  on-load
    |=  =old=vase
    =/  old  !<(versioned-state old-vase)
    ?:  ?=(%1 -.old)
      `this(state old)
    |^
    :-  ~[kick-all]
    =*  paths  ~(key by groups.old)
    =/  [unmanaged=(list path) managed=(list path)]
      (skid ~(tap in paths) |=(=path =('~' (snag 0 path))))
    =.  groups  (all-unmanaged unmanaged)
    =.  groups  (all-managed managed)
    this
    ::
    ++  all-managed
      |=  paths=(list path)
      ^+  groups
      ?~  paths
        groups
      =/  [rid=resource =group]
        (migrate-group i.paths)
      %=    $
          paths  t.paths
        ::
          groups
        (~(put by groups) rid group)
      ==
    ::
    ++  all-unmanaged
      |=  paths=(list path)
      ^+  groups
      ?~  paths
        groups
      =/  [=resource =group]
        (migrate-unmanaged i.paths)
      %=    $
          paths  t.paths
        ::
          groups
        (~(put by groups) resource group)
      ==
    ++  kick-all
      ^-  card
      :+  %give  %kick
      :_  ~
      %~  tap  by
      %+  roll  ~(val by sup.bowl)
      |=  [[=ship pax=path] paths=(set path)]
      (~(put in paths) pax)
    ::
    ++  migrate-unmanaged
      |=  pax=path
      ^-  [resource group]
      =/  [=policy members=(set ship)]
        (unmanaged-permissions pax)
      ?>  =('~' -.pax)
      =.  pax  +.pax
      =/  rid=resource
        (de-path:resource pax)
      =/  =tags
        (~(put ju *tags) %admin entity.rid)
      [rid members tags policy %.y]
    ::
    ++  unmanaged-permissions
      |=  pax=path
      ^-  [policy (set ship)]
      =/  perm
        (need (scry-group-permissions pax))
      ?:  ?=(%black kind.perm)
        :-  [%open ~ who.perm]
        ~
      :_  who.perm
      *invite:policy
    ::
    ++  migrate-group
      |=  pax=path
      =/  members
        (~(got by groups.old) pax)
      =^  =policy  members
        (migrate-permissions pax members)
      =/  rid=resource
        (de-path:resource pax)
      =/  =tags
        (~(put ju *tags) %admin entity.rid)
      [rid members tags policy %.n]
    ::
    ++  migrate-permissions
      |=  [pax=path ships=(set ship)]
      ^-  [policy (set ship)]
      =/  perm
        (scry-group-permissions pax)
      ?~  perm
        [*invite:policy ships]
      ?>  ?=(%white kind.u.perm)
      [[%invite ~] (~(uni in ships) who.u.perm)]
    ::
    ++  scry-unmanaged-groups
      ^-  (set path)
      .^  (set path)
        %gx
        (scot %p our.bowl)
        %permission-store
        (scot %da now.bowl)
        /keys/noun
      ==
    ::
    ++  scry-group-permissions
      |=  pax=path
      ^-  (unit permission:permission-store)
      .^  (unit permission:permission-store)
        %gx
        (scot %p our.bowl)
        %permission-store
        (scot %da now.bowl)
        ;:  weld
          /permission
          pax
          /noun
        ==
      ==
    --
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    =^  cards  state
      ?:  ?=(?(%group-update %group-action) mark)
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
    [%give %fact ~ %group-update !>([%initial groups])]~
  ::
  ++  on-leave  on-leave:def
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def path)
        [%y %groups ~]
      =/  =arch
        :-  ~
        %-  malt
        %+  turn
          ~(tap by groups)
        |=  [rid=resource *]
        ^-  [@ta ~]
        =/  group=^path
          (en-path:resource rid)
        [(spat group) ~]
      ``noun+!>(arch)
    ::
        [%x %groups %ship @ @ ~]
      =/  rid=(unit resource)
        (de-path-soft:resource t.t.path)
      ?~  rid   ~
      ``noun+!>((peek-group u.rid))
    ::
       [%x %groups %ship @ @ %join @ ~]
      =/  rid=(unit resource)
        (de-path-soft:resource t.t.path)
      =/  =ship
        (slav %p i.t.t.t.t.t.path)
      ?~  rid  ~
      ``noun+!>((peek-group-join u.rid ship))
    ==
  ::
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  bol=bowl:gall
++  peek-group
  |=  rid=resource
  ^-  (unit group)
  (~(get by groups) rid)

++  peek-group-join
  |=  [rid=resource =ship]
  =/  =group
    (~(gut by groups) rid *group)
  =*  policy  policy.group
  ?-  -.policy
      %invite
    |((~(has in pending.policy) ship) (~(has in members.group) ship))
      %open
    !|((~(has in banned.policy) ship) (~(has in ban-ranks.policy) (clan:title ship)))
  ==
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
      %groupify        (groupify +.update)
      %initial-group   (initial-group +.update)
      %initial         [~ state]
  ==
  ::  +groupify: unset .hidden flag
  ::
  ++  groupify
    |=  [rid=resource ~]
    ^-  (quip card _state)
    =/  =group
      (~(got by groups) rid)
    =.  hidden.group  %.n
    =.  groups
      (~(put by groups) rid group)
    :_  state
    (send-diff %groupify rid ~)
  ::  +add-group: add group to store
  ::
  ::    always include ship in own groups, no-op if group already exists
  ::
  ++  add-group
    |=  [rid=resource =policy hidden=?]
    ^-  (quip card _state)
    ?<  (~(has by groups) rid)
    =|  =group
    =.  members.group
      (~(put in members.group) our.bol)
    =.  policy.group   policy
    =.  hidden.group   hidden
    =.  tags.group
      (~(put ju tags.group) %admin our.bol)
    =.  groups
      (~(put by groups) rid group)
    :_  state
    (send-diff %add-group rid policy hidden)
  ::  +add-members: add members to group
  ::
  ++  add-members
    |=  [rid=resource new-ships=(set ship)]
    ^-  (quip card _state)
    =/  =group  (~(got by groups) rid)
    =.  members.group  (~(uni in members.group) new-ships)
    =*  policy  policy.group
    =.  policy
      ?.  ?=(%invite -.policy)
        policy
      =.  pending.policy
        (~(dif in pending.policy) new-ships)
      policy
    =.  groups
      (~(put by groups) rid group)
    :_  state
    (send-diff %add-members rid new-ships)
  ::  +remove-members: remove members from group
  ::
  ::    no-op if group does not exist
  ::
  ::
  ++  remove-members
    |=  [rid=resource ships=(set ship)]
    ^-  (quip card _state)
    =/  =group
      (~(got by groups) rid)
    =.  members.group
      (~(dif in members.group) ships)
    =.  tags.group
      (remove-tags group ships)
    =.  groups
      (~(put by groups) rid group)
    :_   state
    (send-diff %remove-members rid ships)
  ::  +add-tag: add tag to ships
  ::
  ::    crash if ships are not in group
  ::
  ++  add-tag
    |=  [rid=resource =tag ships=(set ship)]
    ^-  (quip card _state)
    =/  =group
      (~(got by groups) rid)
    ?>  ?=(~ (~(dif in ships) members.group))
    =.  tags.group
      (merge-tags tags.group ships (sy tag ~))
    =.  groups
      (~(put by groups) rid group)
    :_  state
    (send-diff %add-tag rid tag ships)
  ::  +remove-tag: remove tag from ships
  ::
  ::    crash if ships are not in group or tag does not exist
  ::
  ++  remove-tag
    |=  [rid=resource =tag ships=(set ship)]
    ^-  (quip card _state)
    =/  =group
      (~(got by groups) rid)
    ?>  ?&  ?=(~ (~(dif in ships) members.group))
            (~(has by tags.group) tag)
        ==
    =/  tag-ships
      (~(got by tags.group) tag)
    =.  tag-ships
      (~(dif in tag-ships) ships)
    =.  tags.group
      (~(put by tags.group) tag tag-ships)
    =.  groups
      (~(put by groups) rid group)
    :_  state
    (send-diff %remove-tag rid tag ships)
  ::  initial-group: initialize foreign group
  ::
  ++  initial-group
    |=  [rid=resource =group]
    ^-  (quip card _state)
    =.  groups
      (~(put by groups) rid group)
    :_  state
    (send-diff %initial-group rid group)
  ::  +change-policy: modify group access control
  ::
  ::    If the change will kick members, then send a separate
  ::    %remove-members diff after the %change-policy diff
  ++  change-policy
    |=  [rid=resource =diff:policy]
    ^-  (quip card _state)
    ?.  (~(has by groups) rid)
      [~ state]
    =/  =group
      (~(got by groups) rid)
    |^
    =^  cards  group
      ?-  -.diff
        %open     (open +.diff)
        %invite   (invite +.diff)
        %replace  (replace +.diff)
      ==
    =.  groups
      (~(put by groups) rid group)
    :_  state
    %+  weld
      (send-diff %change-policy rid diff)
    cards
    ::
    ++  open
      |=  =diff:open:policy
      ?-  -.diff
        %allow-ranks     (allow-ranks +.diff)
        %ban-ranks       (ban-ranks +.diff)
        %allow-ships     (allow-ships +.diff)
        %ban-ships       (ban-ships +.diff)
      ==
    ::
    ++  invite
      |=  =diff:invite:policy
      ?-  -.diff
        %add-invites     (add-invites +.diff)
        %remove-invites  (remove-invites +.diff)
      ==
    ::
    ++  allow-ranks
      |=  ranks=(set rank:title)
      ^-  (quip card _group)
      ?>  ?=(%open -.policy.group)
      =.  ban-ranks.policy.group
        (~(dif in ban-ranks.policy.group) ranks)
      `group
    ::
    ++  ban-ranks
      |=  ranks=(set rank:title)
      ^-  (quip card _group)
      ?>  ?=(%open -.policy.group)
      =.  ban-ranks.policy.group
        (~(uni in ban-ranks.policy.group) ranks)
      `group
    ::
    ++  allow-ships
      |=  ships=(set ship)
      ^-  (quip card _group)
      ?>  ?=(%open -.policy.group)
      =.  banned.policy.group
        (~(dif in banned.policy.group) ships)
      `group
    ::
    ++  ban-ships
      |=  ships=(set ship)
      ^-  (quip card _group)
      ?>  ?=(%open -.policy.group)
      =.  banned.policy.group
        (~(uni in banned.policy.group) ships)
      =/  to-remove=(set ship)
        (~(int in members.group) banned.policy.group)
      :-  ~[(poke-us %remove-members rid to-remove)]
      group
    ::
    ++  add-invites
      |=  ships=(set ship)
      ^-  (quip card _group)
      ?>  ?=(%invite -.policy.group)
      =.  pending.policy.group
        (~(uni in pending.policy.group) ships)
      `group
    ::
    ++  remove-invites
      |=  ships=(set ship)
      ^-  (quip card _group)
      ?>  ?=(%invite -.policy.group)
      =.  pending.policy.group
        (~(uni in pending.policy.group) ships)
      `group
    ++  replace
      |=  =policy
      ^-  (quip card _group)
      =.  policy.group
        policy
      `group
    --
  ::  +remove-group: remove group from store
  ::
  ::    no-op if group does not exist
  ++  remove-group
    |=  [rid=resource ~]
    ^-  (quip card _state)
    ?.  (~(has by groups) rid)
      `state
    =.  groups
      (~(del by groups) rid)
    :_  state
    (send-diff %remove-group rid ~)
  ::
  --

++  merge-tags
  |=  [=tags ships=(set ship) new-tags=(set tag)]
  ^+  tags
  =/  tags-list  ~(tap in new-tags)
  |-
  ?~  tags-list
    tags
  =*  tag  i.tags-list
  =/  old-ships=(set ship)
    (~(gut by tags) tag ~)
  %=    $
    tags-list  t.tags-list
  ::
      tags
    %+  ~(put by tags)
      tag
    (~(uni in old-ships) ships)
 ==
++  remove-tags
  |=  [=group ships=(set ship)]
  ^-  tags
  %-  malt
  %+  turn
    ~(tap by tags.group)
  |=  [=tag tagged=(set ship)]
  :-  tag
  (~(dif in tagged) ships)
::
++  poke-us
  |=  =action:store
  ^-  card
  [%pass / %agent [our.bol %group-store] %poke %group-action !>(action)]
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
