::  group-store [landscape]:
::
::  Store groups of ships
::
::    group-store stores groups of ships, so that resources in other apps can be
::    associated with a group. The current model of group-store rolls
::    and invites inside this store for simplicity reasons, although
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
/-  *group
/+  store=group-store, default-agent, verb, dbug, resource, *migrate, agentio
/+  gladio
|%
+$  card  card:agent:gall
++  ota-host  ~sogryp-dister-dozzod-dozzod
::
+$  versioned-state
  $%  state-zero
      state-one
      state-two
      state-three
      state-four
  ==
::
+$  state-zero
  [%0 *]
::
+$  state-one
  $:  %1
      =groups:groups-state-one
  ==
::
+$  state-two
  $:  %2
      =groups
  ==
::
+$  state-three
  $:  %3
      =groups
      wait=(set ship)
  ==
::
+$  state-four
  $:  %4
      =groups
      wait=(set ship)
  ==
--
::
=|  state-four
=*  state  -
::
%-  agent:dbug
%+  verb  &
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
    =|  cards=(list card)
    |^
    ?-    -.old
        %4  [(flop cards) this(state old)]
    ::
        %3 
      %_    $
          old    [%4 +.old] 
          cards
        :_  cards
        [%pass /pyre/rebuild %agent [our dap]:bowl %poke noun+!>(%rebuild)]
      ==
    ::
        %2
      %_    $
          old    [%3 groups.old ~]
          cards  
        %-  welp 
        :_  cards
        :~  [%pass /pyre/export %agent [our dap]:bowl %poke noun+!>(%export)]
            [%pass /pyre/migrate %agent [our dap]:bowl %poke noun+!>(%migrate)]
            [%pass / %agent [our %hood]:bowl %poke %kiln-install !>([%groups ota-host %groups])]
            [%pass / %agent [our %hood]:bowl %poke %kiln-install !>([%talk ota-host %talk])]
        ==
      ==
    ::
        %1  
      %_    $
        -.old  %2
        groups.old  (groups-1-to-2 groups.old)
      ==
      ::
      %0  $(old *state-two)
    ==
    ::
    ++  groups-1-to-2
      |=  =groups:groups-state-one
      ^+  ^groups
      %-  ~(run by groups)
      |=  =group:groups-state-one
      =/  =tags
        (tags-1-to-2 tags.group)
      [members.group tags [policy hidden]:group]
    ::
    ++  tags-1-to-2
      |=  =tags:groups-state-one
      ^-  ^tags
      %-  ~(gas by *^tags)
      %+  murn
        ~(tap by tags)
      |=  [=tag:groups-state-one ships=(set ship)]
      ?^  tag  ~
      `[tag ships]
    --
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    =^  cards  state
      ?+    mark  (on-poke:def mark vase)
        %sane  (poke-sane:gc !<(?(%check %fix) vase))
      ::
          %noun 
        ?+  q.vase  !!
          %migrate  poke-migrate:gc
          %migrate-my-channels  poke-migrate-my-channels:gc
          %export   poke-export:gc
          %rebuild  poke-rebuild:gc
        ==
      ::
          ?(%group-update-0 %group-action)
        (poke-group-update:gc !<(update:store vase))
      ::
          %import
        (poke-import:gc q.vase)
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    ?:  ?=([%wait ~] path)
      `this
    ?>  ?=([%groups ~] path)
    :_  this
    [%give %fact ~ %group-update-0 !>([%initial groups])]~
  ::
  ++  on-leave  on-leave:def
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def path)
        [%x %wait ~]
      ``ships+!>(~(tap in wait))
        [%y %groups ~]
      ``noun+!>(`(set resource)`~(key by groups))
    ::
        [%x %groups %ship @ @ ~]
      =/  rid=(unit resource)
        (de-path-soft:resource t.t.path)
      ?~  rid   ~
      ``noun+!>(`(unit group)`(peek-group u.rid))
    ::
       [%x %groups %ship @ @ %join @ ~]
      =/  rid=(unit resource)
        (de-path-soft:resource t.t.path)
      =/  =ship
        (slav %p i.t.t.t.t.t.t.path)
      ?~  rid  ~
      ``noun+!>(`?`(peek-group-join u.rid ship))
    ::
        [%x %export ~]
      ``noun+!>(state)
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      ?+    wire  [- state]:(on-agent:def wire sign)
          [%pyre *]      (take-pyre:gc t.wire sign)
          [%gladio @ ~]  (take-migrate:gc sign)
      ::
          [%try-rejoin @ *]
        ?>  ?=(%poke-ack -.sign)
        =/  rid=resource  (de-path:resource t.t.wire)
        ?~  p.sign
          =/  =cage
            [%pull-hook-action !>([%add entity.rid rid])]
          :_  state
          [%pass / %agent [our.bowl %group-pull-hook] %poke cage]~
        =/  nack-count=@ud  (slav %ud i.t.wire)
        =/  wakeup=@da
          (add now.bowl (mul ~s1 (bex (min 19 nack-count))))
        :_  state
        [%pass wire %arvo %b %wait wakeup]~
      ==
    [cards this]
  ::
  ++  on-arvo
    |=  [=(pole knot) =sign-arvo]
    ^-  (quip card _this)
    ?:  ?=([%gladio %backoff ship=@ ~] pole)
      =^  cards  state
        (take-backoff:gc (slav %p ship.pole) sign-arvo)
      [cards this]
    ?.  ?=([%try-rejoin count=@ res=*] pole)
      (on-arvo:def pole sign-arvo)
    =/  =resource       (de-path:resource res.pole)
    =/  nack-count=@ud  (slav %ud count.pole)
    ?>  ?=([%behn %wake *] sign-arvo)
    ~?  ?=(^ error.sign-arvo)
      "behn errored in backoff timers, continuing anyway"
    :_  this
    [(try-rejoin:gc resource +(nack-count))]~
  ::
  ++  on-fail   on-fail:def
  --
::
|_  bol=bowl:gall
+*  io  ~(. agentio bol)
++  poke-rebuild
  ^-  (quip card _state)
  |^  
  =.  wait
    put-missing
  =^  cards  state
    rewatch
  [cards state]
  ::
  ++  rewatch
    =/  wait  ~(tap in wait)
    =|  cards=(list card)
    |-
    ?~  wait
      [cards state]
    =/  wir  /gladio/(scot %p i.wait)
    =.  cards
      :_(cards (watch-init-migrate i.wait))
    ::  if we have a subscription already, leave first to restart
    =?  cards
        (~(has by wex.bol) [wir i.wait %groups])
      :_(cards [%pass wir %agent [i.wait %groups] %leave ~])
    $(wait t.wait)
  ::
  ++  put-missing
    =/  wex  ~(tap by wex.bol)
    |-
    ?~  wex
      wait
    =/  [[=wire =ship =term] [acked=? =(pole knot)]]
      i.wex
    ?.  ?=([%gladio ship=@ ~] pole)
      $(wex t.wex)
    $(wex t.wex, wait (~(put in wait) (slav %p ship.pole)))
  --
::
++  poke-export
  ^-  (quip card _state)
  :_  state
  =;  =cage
    [%pass /export %agent [our.bol %hood] %poke cage]~
  drum-put+!>([/groups/jam ~(export gladio bol)])
::
++  poke-migrate
  ^-  (quip card _state)
  =^  cards-1=(list card)  wait
    (~(migrate-start gladio bol) wait)
  =/  cards-2=(list card)
    (turn ~(tap in wait) watch-init-migrate)
  =/  cards  (welp cards-1 cards-2)
  [cards state(wait wait)]
++  poke-migrate-my-channels
  ^-  (quip card _state)
  =/  [cards=(list card) *]  (~(migrate-my-channels gladio bol) ~)
  [cards state]
::
++  watch-init-migrate
  |=  =ship
  ^-  card
  [%pass /gladio/(scot %p ship) %agent [ship %groups] %watch /init]
::
++  backoff-migrate
  |=  =ship
  ^-  card
  [%pass /gladio/backoff/(scot %p ship) %arvo %b %wait (add ~h1 now.bol)]
::
++  take-pyre
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _state)
  :_  state
  ?>  ?=(%poke-ack -.sign)
  ?~  p.sign
    ~
  [%pass / %pyre leaf/"{<wire>} failed" u.p.sign]~
::
++  take-backoff
  |=  [=ship sign=sign-arvo]
  ^-  (quip card _state)
  ?>  ?=([%behn %wake *] sign)
  ?:  ?=(^ error.sign)
    `state
  :_  state
  ~[(watch-init-migrate ship)]
::
++  take-migrate
  |=  =sign:agent:gall
  ^-  (quip card _state)
  ?.  (~(has in wait) src.bol)
    ::  already succeeded
    `state
  ?-    -.sign  
      ?(%poke-ack %fact)  `state
      %kick               :_(state (watch-init-migrate src.bol)^~)
      %watch-ack
    ?~  p.sign
      ::  they have public release
      ~&  migrating/src.bol
      =.  wait  (~(del in wait) src.bol)
      :_  state
      :-  [%give %fact ~[/wait] ships+!>(~(tap in wait))]
      (~(migrate-ship gladio bol) src.bol)
    :_  state
    ~[(backoff-migrate src.bol)]
  ==
::
++  peek-group
  |=  rid=resource
  ^-  (unit group)
  (~(get by groups) rid)
::
++  peek-group-join
  |=  [rid=resource =ship]
  ^-  ?
  =/  ugroup
    (~(get by groups) rid)
  ?~  ugroup
    %.n
  =*  group   u.ugroup
  =*  policy  policy.group
  ?-  -.policy
      %invite
    ?|  (~(has in pending.policy) ship)
        (~(has in members.group) ship)
    ==
      %open
    ?!  ?|
      (~(has in banned.policy) ship)
      (~(has in ban-ranks.policy) (clan:title ship))
    ==
  ==
++  poke-sane
  |=  input=?(%check %fix)
  ^-  (quip card _state)
  =;  cards=(list card)
    ?:  =(%check input)
      ~&  cards
      `state
    [cards state]
  %+  roll  ~(tap in ~(key by groups))
  |=  [rid=resource out=(list card)]
  ?.  ?&  =(entity.rid our.bol)
          !(~(has in members:(~(got by groups) rid)) our.bol)
      ==
    out
  =/  =wire
    sane+(en-path:resource rid)
  =*  poke-self  ~(poke-self pass:io wire)
  %+  weld  out
  :~  (poke-self group-update-0+!>([%add-members rid (silt our.bol ~)]))
      (poke-self group-update-0+!>([%add-tag rid %admin (silt our.bol ~)]))
  ==
::
++  poke-import
  |=  arc=*
  ^-  (quip card _state)
  |^
  =/  sty=state-four
    [%4 (remake-groups ;;((tree [resource tree-group]) +.arc)) ~]
  :_  sty
  %+  roll  ~(tap by groups.sty)
  |=  [[rid=resource grp=group] out=(list card)]
  ?:  =(entity.rid our.bol)
    %+  weld  out
    %+  roll  ~(tap in members.grp)
    |=  [recipient=@p out=(list card)]
    ?:  =(recipient our.bol)
      out
    ::  TODO: figure out contacts integration
    out
  :_  out
  (try-rejoin rid 0)
  ::
  ++  remake-groups
    |=  grps=(tree [resource tree-group])
    ^-  ^groups
    %-  remake-map
    (~(run by grps) remake-group)
  ::
  ++  remake-group
    |=  grp=tree-group
    ^-  group
    %=  grp
      members  (remake-set members.grp)
      tags     (remake-jug tags.grp)
      policy   (remake-policy policy.grp)
    ==
  ::
  +$  tree-group
    $:  members=(tree ship)
        tags=(tree [tag (tree ship)])
        policy=tree-policy
        hidden=?
    ==
  ::
  +$  tree-policy
    $%  [%invite pending=(tree ship)]
        [%open ban-ranks=(tree rank:title) banned=(tree ship)]
    ==
  ::
  ++  remake-policy
    |=  pl=tree-policy
    ^-  policy
    ?-  -.pl
      %invite  [%invite (remake-set pending.pl)]
      %open    [%open (remake-set ban-ranks.pl) (remake-set banned.pl)]
    ==
  --
::
++  try-rejoin
  |=  [rid=resource nack-count=@ud]
  ^-  card
  =/  =cage
    :-  %group-update-0
    !>  ^-  update:store
    [%add-members rid (sy our.bol ~)]
  =/  =wire
    [%try-rejoin (scot %ud nack-count) (en-path:resource rid)]
  [%pass wire %agent [entity.rid %group-push-hook] %poke cage]
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
      %expose          (expose +.update)
      %initial-group   (initial-group +.update)
      %initial         [~ state]
  ==
  ::  +expose: unset .hidden flag
  ::
  ++  expose
    |=  [rid=resource ~]
    ^-  (quip card _state)
    =/  =group
      (~(got by groups) rid)
    =.  hidden.group  %.n
    =.  groups
      (~(put by groups) rid group)
    :_  state
    (send-diff %expose rid ~)
  ::  +add-group: add group to store
  ::
  ::    no-op if group already exists
  ::
  ++  add-group
    |=  [rid=resource =policy hidden=?]
    ^-  (quip card _state)
    ?<  (~(has by groups) rid)
    =|  =group
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
    =.  groups
      %+  ~(jab by groups)  rid
      |=  group
      %=  +<
        members  (~(uni in members) new-ships)
      ::
          policy
        ?.  ?=(%invite -.policy)
          policy
        policy(pending (~(dif in pending.policy) new-ships))
      ==
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
    ?.  (~(has by groups) rid)  [~ state]
    =.  groups
      %+  ~(jab by groups)  rid
      |=  group
      %=  +<
        members  (~(dif in members) ships)
        tags  (remove-tags +< ships)
      ==
    :_   state
    (send-diff %remove-members rid ships)
  ::  +add-tag: add tag to ships
  ::
  ::    crash if ships are not in group
  ::
  ++  add-tag
    |=  [rid=resource =tag ships=(set ship)]
    ^-  (quip card _state)
    =.  groups
      %+  ~(jab by groups)  rid
      |=  group
      ?>  ?=(~ (~(dif in ships) members))
      +<(tags (merge-tags tags ships (sy tag ~)))
    :_  state
    (send-diff %add-tag rid tag ships)
  ::  +remove-tag: remove tag from ships
  ::
  ::    crash if ships are not in group or tag does not exist
  ::
  ++  remove-tag
    |=  [rid=resource =tag ships=(set ship)]
    ^-  (quip card _state)
    =.  groups
      %+  ~(jab by groups)  rid
      |=  group
      ?>  ?&  ?=(~ (~(dif in ships) members))
              (~(has by tags) tag)
          ==
      %=  +<
        tags  (dif-ju tags tag ships)
      ==
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
        (~(dif in pending.policy.group) ships)
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
::  TODO: move to +zuse
++  dif-ju
  |=  [=tags =tag remove=(set ship)]
  =/  ships  ~(tap in remove)
  |-
  ?~  ships
    tags
  $(tags (~(del ju tags) tag i.ships), ships t.ships)
::
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
  [%give %fact ~[/groups] %group-update-0 !>(update)]~
::
--
