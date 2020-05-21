^?
|%
::  $group-id: unique identifier for a group
::
+$  group-id  [=ship =term]
::  $groups: a mapping from group-ids to groups
::
+$  groups  (map group-id group)
::  $tag: an identifier used to identify a subset of members
::
::   Tags may be used and recognised differently across apps.
::   TODO: document 'blessed' tags
::   example tags:
::   %pending - denotes that member has been invited but not joined yet
::   %admin - denotes that member can add and remove members
::
+$  tag  term
::  $role-tag: a kind of $tag that identifies a privileged user
::
::    These roles are
::    %admin: Administrator, can do everything except delete the group
::    %moderator: Moderator, can add/remove/ban users
::    %janitor: Has no special meaning inside group-store,
::    but may be given additional privileges in other apps.
::    %member: Ordinary member, this tag is implied if the user is not in any
::    of the other roles
+$  role-tag
  ?(%admin %moderator %janitor %members)
::  $tag-queries: a mapping from a $tag to the members it identifies
::
+$  tag-queries  (jug tag ship)
::  $group: description of a group of users
::
::    members: members of the group
::    tag-queries: a map of subsets
::    policy: permissions for the group
::
+$  group
  $:  members=(set ship)
      =tag-queries
      =policy
  ==
::  $rank: ship class by length
::
::    0: galaxy or star -- 2  bytes
::    1: planet         -- 4  bytes
::    2: moon           -- 8  bytes
::    3: comet          -- 16 bytes
+$  rank  ?(%0 %1 %2 %3)
::  $policy: access control for a group
::
++  policy
  =<  policy
  |%
  ::
  +$  policy
    $%  invite
        open
    ==
  ::  $diff: change group policy
  +$  diff
    $%  diff:invite
        diff:open
        [%replace =policy]
    ==
  ::  $invite: allow only invited ships
  ++  invite
    =<  invite-policy
    |%
    ::
    +$  invite-policy
      [%invite pending=(set ship)]
    ::  $diff: add or remove invites
    ::
    +$  diff
      $%  [%add-invites invitees=(set ship)]
          [%remove-invites invitees=(set ship)]
      ==
    --
  ::  $open: allow all unbanned ships of approriate rank
  ::
  ++  open
    =<  open-policy
    |%
    ::
    +$  open-policy
      [%open ranks=(set rank) banned=(set ship)]
    :: $diff: ban or allow ranks and ships
    ::
    +$  diff
      $%  [%allow-ranks ranks=(set rank)]
          [%ban-ranks ranks=(set rank)]
          [%ban-ships ships=(set ship)]
          [%allow-ships ships=(set ship)]
      ==
    --
  --
--
