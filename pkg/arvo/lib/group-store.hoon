/-  *group, sur=group-store
^?
=<  [. sur]
=,  sur
|%
::  +en-path: transform into path
::
++  en-path
  |%
  ::
  ++  group-id
    |=  ^group-id
    ^-  path
    /[(scot %p ship)]/[term]
  --
::  +de-path: transform from path
::
++  de-path
  |%
  ::
  ++  group-id
    |=  =path
    ^-  (unit ^group-id)
    ?.  ?=([@ @ *] path)
      ~
    =/  ship=(unit ship)
      (slaw %p i.path)
    ?~  ship  ~
    =*  term   i.t.path
    `[u.ship term]
  --
++  enjs
  =,  enjs:format
  |%
  ::
  ++  update
    |=  =^update
    ^-  json
    %+  frond  -.update
    ?-  -.update
      %add-group       (add-group update)
      %add-members     (add-members update)
      %add-tag         (add-tag update)
      %remove-members  (remove-members update)
      %remove-tag      (remove-tag update)
      %initial         (initial update)
      %initial-group   (initial-group update)
      %remove-group    (remove-group update)
      %change-policy   (change-policy update)
      %groupify        (groupify update)
    ==
  ++  group-id-path
    |=  =^group-id
    %-  spat
    %-  group-id:en-path
    group-id
  ++  initial-group
    |=  =^update
    ?>  ?=(%initial-group -.update)
    %-  pairs
    :~  group-id+(group-id group-id.update)
        group+(group group.update)
    ==
  ::
  ++  initial
    |=  =^initial
    ?>  ?=(%initial -.initial)
    %-  pairs
    %+  turn
      ~(tap by groups.initial)
    |=  [=^group-id grp=^group]
    ^-  [@t json]
    :_  (group grp)
    (group-id-path group-id)
  ::
  ++  group
    |=  =^group
    ^-  json
    %-  pairs
    :~  members+(set ship members.group)
        policy+(policy policy.group)
        tags+(tags tags.group)
    ==
  ++  rank
    |=  =rank:title
    ^-  json
    [%s rank]
  ++  tags
    |=  =^tags
    ^-  json
    |^
    :-  %o
    (~(uni by app) group)
    ++  group
      ^-  (map @t json)
      %-  malt
      %+  murn
        ~(tap by tags)
      |=  [=^tag ships=(^set ^ship)]
      ^-  (unit [@t json])
      ?^  tag
        ~
      `[tag (set ship ships)]
    ++  app
      ^-  (map @t json)
      =|  app-tags=(map @t json)
      =/  tags  ~(tap by tags)
      |-
      ?~  tags
        app-tags
      =*  tag  i.tags
      ?@  p.tag
        $(tags t.tags)
      =/  app=json
        (~(gut by app-tags) app.p.tag [%o ~])
      ?>  ?=(%o -.app)
      =.  p.app
        (~(put by p.app) tag.p.tag (set ship q.tag))
      =.  app-tags
        (~(put by app-tags) app.p.tag app)
      $(tags t.tags)
    --
  ::
  ++  set
    |*  [item=$-(* json) sit=(^set)]
    ^-  json
    :-  %a
    %+  turn
      ~(tap in sit)
    item
  ++  tag
    |=  =^tag
    ^-  json
    ?@  tag
      (frond %tag s+tag)
    %-  pairs
    :~  app+s+app.tag
        tag+s+tag.tag
    ==
  ::
  ++  policy
    |=  =^policy
    %+  frond  -.policy
    %-  pairs
    ?-  -.policy
         %invite
      :~  pending+(set ship pending.policy)
      ==
         %open
      :~  banned+(set ship banned.policy)
          ban-ranks+(set rank ban-ranks.policy)
      ==
    ==
  ++  policy-diff
    |=  =diff:^policy
    %+  frond  -.diff
    %-  pairs
    ?+  -.diff  !!
      %add-invites  [invitees+(set ship invitees.diff) ~]
      %remove-invites  [invitees+(set ship invitees.diff) ~]
      %allow-ranks  [ranks+(set rank ranks.diff) ~]
      %ban-ranks  [ranks+(set rank ranks.diff) ~]
      %allow-ships  [ranks+(set ship ships.diff) ~]
      %ban-ships  [ranks+(set ship ships.diff) ~]
    ==
  ::
  ++  groupify
    |=  =^update
    ^-  json
    ?>  ?=(%groupify -.update)
    (frond %group-id (group-id group-id.update))
  ::
  ++  remove-group
    |=  =^update
    ^-  json
    ?>  ?=(%remove-group -.update)
    (frond %group-id (group-id group-id.update))
  ::
  ++  group-id
    |=  =^group-id
    ^-  json
    %-  pairs
    :~  name+s+term.group-id
        ship+(ship ship.group-id)
    ==
  ++  add-group
    |=  =action
    ^-  json
    ?>  ?=(%add-group -.action)
    %-  pairs
    :~  group-id+(group-id group-id.action)
        policy+(policy policy.action)
        hidden+b+hidden.action
    ==
  ::
  ++  add-members
    |=  =action
    ^-  json
    ?>  ?=(%add-members -.action)
    %-  pairs
    :~  group-id+(group-id group-id.action)
        ships+(set ship ships.action)
    ==
  ::
  ++  remove-members
    |=  =action
    ^-  json
    ?>  ?=(%remove-members -.action)
    %-  pairs
    :~  group-id+(group-id group-id.action)
        ships+(set ship ships.action)
    ==
  ::
  ++  add-tag
    |=  =action
    ^-  json
    ?>  ?=(%add-tag -.action)
    %-  pairs
    :~  group-id+(group-id group-id.action)
        tag+(tag tag.action)
        ships+(set ship ships.action)
    ==
  ::
  ++  remove-tag
    |=  =action
    ^-  json
    ?>  ?=(%remove-tag -.action)
    %-  pairs
    :~  group-id+(group-id group-id.action)
        tag+(tag tag.action)
        ships+(set ship ships.action)
    ==
  ::
  ++  change-policy
    |=  =action
    ^-  json
    ?>  ?=(%change-policy -.action)
    %-   pairs
    :~   group-id+(group-id group-id.action)
         diff+(policy-diff diff.action)
    ==
  --
++  dejs
  =,  dejs:format
  |%
  ::
  ++  action
    ^-  $-(json ^action)
    %-  of
    :~
      add-group+add-group
      add-members+add-members
      remove-members+remove-members
      add-tag+add-tag
      remove-tag+remove-tag
      change-policy+change-policy
      remove-group+remove-group
      groupify+groupify
    ==
  ++  rank
    |=  =json
    ^-  rank:title
    ?>  ?=(%s -.json)
    ?:  =('czar' p.json)  %czar
    ?:  =('king' p.json)  %king
    ?:  =('duke' p.json)  %duke
    ?:  =('earl' p.json)  %earl
    ?:  =('pawn' p.json)  %pawn
    !!
  ++  tag
    |=  =json
    ^-  ^tag
    ?>  ?=(%o -.json)
    ?.  (~(has by p.json) 'app')
      =/  tag-json
        (~(got by p.json) 'tag')
      ?>  ?=(%s -.tag-json)
      ?:  =('admin' p.json)  %admin
      ?:  =('moderator' p.json)  %moderator
      ?:  =('janitor' p.json)  %janitor
      !!
    %.  json
    %-  ot
    :~  app+so
        tag+so
    ==

  ::  move to zuse also
  ++  oj
    |*  =fist
    ^-  $-(json (jug cord _(fist *json)))
    (om (as fist))
  ++  tags
    ^-  $-(json ^tags)
    *$-(json ^tags)
  :: TODO: move to zuse
  ++  ship
    (su ;~(pfix sig fed:ag))
  ++  policy
    ^-  $-(json ^policy)
    %-  of
    :~  invite+invite-policy
        open+open-policy
    ==
  ++  invite-policy
    %-  ot
    :~  pending+(as ship)
    ==
  ++  open-policy
    %-  ot
    :~  ban-ranks+(as rank)
        banned+(as ship)
    ==
  ++  policy-diff
    ^-  $-(json diff:^policy)
    %-  of
    :~  add-invites+(as ship)
        remove-invites+(as ship)
        allow-ranks+(as rank)
        allow-ships+(as ship)
        ban-ranks+(as rank)
        ban-ships+(as ship)
        replace+policy
    ==
  ++  group-id
    %-  ot
    :~  ship+ship
        name+so
    ==
  ::
  ++  remove-group
    |=  =json
    ?>  ?=(%o -.json)
    =/  id=^group-id
      (group-id (~(got by p.json) 'group-id'))
    [id ~]
  ::
  ++  groupify
    |=  =json
    ^-  [^group-id ~]
    ?>  ?=(%o -.json)
    =/  id=^group-id
      (group-id (~(got by p.json) 'group-id'))
    [id ~]
  ::
  ++  add-group
    %-  ot
    :~  group-id+group-id
        policy+policy
        hidden+bo
    ==
  ++  add-members
    %-  ot
    :~  group-id+group-id
        ships+(as ship)
        tags+(as tag)
    ==
  ++  remove-members
    ^-  $-(json [^group-id (set ^ship)])
    %-  ot
    :~  group-id+group-id
        ships+(as ship)
    ==
  ++  add-tag
    %-  ot
    :~  group-id+group-id
        tag+tag
        ships+(as ship)
    ==
  ++  remove-tag
    %-  ot
    :~  group-id+group-id
        tag+tag
        ships+(as ship)
    ==
  ++  change-policy
    %-   ot
    :~   group-id+group-id
         diff+policy-diff
    ==
  --

--
