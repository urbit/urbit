/-  *group, sur=group-store
/+  resource
^?
=<  [. sur]
=,  sur
|%
::
++  dekebab
  |=  str=cord
  ^-  cord
  =-  (fall - str)
  %+  rush  str
  =/  name
    %+  cook
     |=  part=tape
     ^-  tape
     ?~  part  part
     :-  (sub i.part 32)
     t.part
    (star low)
  %+  cook
    (cork (bake zing (list tape)) crip)
  ;~(plug (star low) (more hep name))
::
++  enkebab
  |=  str=cord
  ^-  cord
  ~|  str
  =-  (fall - str)
  %+  rush  str
  =/  name
    %+  cook
      |=  part=tape
      ^-  tape
      ?~  part  part
      :-  (add i.part 32)
      t.part
    ;~(plug hig (star low))
  %+  cook
    |=(a=(list tape) (crip (zing (join "-" a))))
  ;~(plug (star low) (star name))

++  migrate-path-map
  |*  map=(map path *)
  =/  keys=(list path)
    (skim ~(tap in ~(key by map)) |=(=path =('~' (snag 0 path))))
  |-
  ?~  keys
    map
  =*  key  i.keys
  ?>  ?=(^ key)
  =/  value
    (~(got by map) key)
  =.  map
    (~(put by map) t.key value)
  =.  map
    (~(del by map) key)
  $(keys t.keys, map (~(put by map) t.key value))
::
++  enjs
  =,  enjs:format
  |%
  ++  frond
    |=  [p=@t q=json]
    ^-  json
    (frond:enjs:format (dekebab p) q)
  ++  pairs
    |=  a=(list [p=@t q=json])
    ^-  json
    %-  pairs:enjs:format
    %+  turn  a
    |=  [p=@t q=json]
    ^-  [@t json]
    [(dekebab p) q]
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
      %expose        (expose update)
    ==
  ::
  ++  initial-group
    |=  =^update
    ?>  ?=(%initial-group -.update)
    %-  pairs
    :~  resource+(enjs:resource resource.update)
        group+(group group.update)
    ==
  ::
  ++  initial
    |=  =^initial
    ?>  ?=(%initial -.initial)
    %-  pairs
    ^-  (list [@t json])
    %+  turn
      ~(tap by groups.initial)
    |=  [rid=resource grp=^group]
    ^-  [@t json]
    :_  (group grp)
    (enjs-path:resource rid)
  ::
  ++  group
    |=  =^group
    ^-  json
    %-  pairs
    :~  members+(set ship members.group)
        policy+(policy policy.group)
        tags+(tags tags.group)
        hidden+b+hidden.group
    ==
  ::
  ++  rank
    |=  =rank:title
    ^-  json
    [%s rank]
  ++  tags
    |=  =^tags
    ^-  json
    %-  pairs
    %+  turn  ~(tap by tags)
    |=  [=^tag ships=(^set ^ship)]
    ^-  [@t json]
    :_  (set ship ships)
    ?@  tag  tag
    ;:  (cury cat 3)
      app.tag  '\\'
      tag.tag  '\\'
      (enjs-path:resource resource.tag)
    ==
  ::
  ++  set
    |*  [item=$-(* json) sit=(^set)]
    ^-  json
    :-  %a
    %+  turn
      ~(tap in sit)
    item
  ::
  ++  tag
    |=  =^tag
    ^-  json
    ?@  tag
      (frond %tag s+tag)
    %-  pairs
    :~  app+s+app.tag
        tag+s+tag.tag
        resource+s+(enjs-path:resource resource.tag)
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
    |^
    ?-  -.diff
      %invite   (invite +.diff)
      %open     (open +.diff)
      %replace  (policy +.diff)
    ==
    ++  open
      |=  =diff:open:^policy
      %+  frond  -.diff
      ?-  -.diff
        %allow-ranks  (set rank ranks.diff)
        %ban-ranks    (set rank ranks.diff)
        %allow-ships  (set ship ships.diff)
        %ban-ships    (set ship ships.diff)
      ==
    ++  invite
      |=  =diff:invite:^policy
      %+  frond  -.diff
      ?-  -.diff
        %add-invites      (set ship invitees.diff)
        %remove-invites   (set ship invitees.diff)
      ==
    --
  ::
  ++  expose
    |=  =^update
    ^-  json
    ?>  ?=(%expose -.update)
    (frond %resource (enjs:resource resource.update))
  ::
  ++  remove-group
    |=  =^update
    ^-  json
    ?>  ?=(%remove-group -.update)
    (frond %resource (enjs:resource resource.update))
  ::
  ++  add-group
    |=  =action
    ^-  json
    ?>  ?=(%add-group -.action)
    %-  pairs
    :~  resource+(enjs:resource resource.action)
        policy+(policy policy.action)
        hidden+b+hidden.action
    ==
  ::
  ++  add-members
    |=  =action
    ^-  json
    ?>  ?=(%add-members -.action)
    %-  pairs
    :~  resource+(enjs:resource resource.action)
        ships+(set ship ships.action)
    ==
  ::
  ++  remove-members
    |=  =action
    ^-  json
    ?>  ?=(%remove-members -.action)
    %-  pairs
    :~  resource+(enjs:resource resource.action)
        ships+(set ship ships.action)
    ==
  ::
  ++  add-tag
    |=  =action
    ^-  json
    ?>  ?=(%add-tag -.action)
    %-  pairs
    ^-  (list [p=@t q=json])
    :~  resource+(enjs:resource resource.action)
        tag+(tag tag.action)
        ships+(set ship ships.action)
    ==
  ::
  ++  remove-tag
    |=  =action
    ^-  json
    ?>  ?=(%remove-tag -.action)
    %-  pairs
    :~  resource+(enjs:resource resource.action)
        tag+(tag tag.action)
        ships+(set ship ships.action)
    ==
  ::
  ++  change-policy
    |=  =action
    ^-  json
    ?>  ?=(%change-policy -.action)
    %-  pairs
    :~  resource+(enjs:resource resource.action)
        diff+(policy-diff diff.action)
    ==
  --
++  dejs
  =,  dejs:format
  |%
  ::
  ++  ruk-jon
    |=  [a=(map @t json) b=$-(@t @t)]
    ^+  a
    =-  (malt -)
    |-
    ^-  (list [@t json])
    ?~  a  ~
    :-  [(b p.n.a) q.n.a]
    %+  weld
      $(a l.a)
    $(a r.a)
  ::
  ++  of
      |*  wer=(pole [cord fist])
      |=  jon=json
      ?>  ?=([%o [@ *] ~ ~] jon)
      |-
      ?-    wer
          :: [[key=@t wit=*] t=*]
          [[key=@t *] t=*]
        =>  .(wer [[* wit] *]=wer)
        ?:  =(key.wer (enkebab p.n.p.jon))
          [key.wer ~|(val+q.n.p.jon (wit.wer q.n.p.jon))]
        ?~  t.wer  ~|(bad-key+p.n.p.jon !!)
        ((of t.wer) jon)
      ==
  ++  ot
    |*  wer=(pole [cord fist])
    |=  jon=json
    ~|  jon
    %-  (ot-raw:dejs:format wer)
    ?>  ?=(%o -.jon)
    (ruk-jon p.jon enkebab)
  ::
  ++  update
    ^-  $-(json ^update)
    |=  jon=json
    ^-  ^update
    %.  jon
    %-  of
    :~
      add-group+add-group
      add-members+add-members
      remove-members+remove-members
      add-tag+add-tag
      remove-tag+remove-tag
      change-policy+change-policy
      remove-group+remove-group
      expose+expose
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
      ?:  =('admin' p.tag-json)  %admin
      ?:  =('moderator' p.tag-json)  %moderator
      ?:  =('janitor' p.tag-json)  %janitor
      !!
    %.  json
    %-  ot
    :~  app+so
        resource+dejs-path:resource
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
  ++  open-policy-diff
    %-  of
    :~  allow-ranks+(as rank)
        allow-ships+(as ship)
        ban-ranks+(as rank)
        ban-ships+(as ship)
    ==
  ++  invite-policy-diff
    %-  of
    :~  add-invites+(as ship)
        remove-invites+(as ship)
    ==
  ++  policy-diff
    ^-  $-(json diff:^policy)
    %-  of
    :~  invite+invite-policy-diff
        open+open-policy-diff
        replace+policy
    ==
  ::
  ++  remove-group
    |=  =json
    ^-  [resource ~]
    ?>  ?=(%o -.json)
    =/  rid=resource
      (dejs:resource (~(got by p.json) 'resource'))
    [rid ~]
  ::
  ++  expose
    |=  =json
    ^-  [resource ~]
    ?>  ?=(%o -.json)
    =/  rid=resource
      (dejs:resource (~(got by p.json) 'resource'))
    [rid ~]
  ::
  ++  add-group
    %-  ot
    :~  resource+dejs:resource
        policy+policy
        hidden+bo
    ==
  ++  add-members
    %-  ot
    :~  resource+dejs:resource
        ships+(as ship)
    ==
  ++  remove-members
    ^-  $-(json [resource (set ^ship)])
    %-  ot
    :~  resource+dejs:resource
        ships+(as ship)
    ==
  ++  add-tag
    %-  ot
    :~  resource+dejs:resource
        tag+tag
        ships+(as ship)
    ==
  ++  remove-tag
    %-  ot
    :~  resource+dejs:resource
        tag+tag
        ships+(as ship)
    ==
  ++  change-policy
    %-  ot
    :~  resource+dejs:resource
        diff+policy-diff
    ==
  --
--
