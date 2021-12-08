/-  *resource
::
^?
|%
::
++  groups-state-one
  |%
  +$  groups  (map resource group)
  ::
  +$  tag  $@(group-tag [app=term tag=term])
  ::
  +$  tags  (jug tag ship)
  ::
  +$  group
    $:  members=(set ship)
        =tags
        =policy
        hidden=?
    ==
  --
::  $groups: a mapping from group-ids to groups
::
+$  groups  (map resource group)
::  $group-tag: an identifier used by groups
::
::    These tags should have precise semantics, as they are shared across all
::    apps.
::
+$  group-tag  ?(role-tag)
::  $tag: an identifier used to identify a subset of members
::
::   Tags may be used and recognised differently across apps.
::   for example, you could use tags like `%author`, `%bot`, `%flagged`...
::
+$  tag  $@(group-tag [app=term =resource tag=term])
::  $role-tag: a kind of $group-tag that identifies a privileged user
::
::    These roles are
::    %admin: Administrator, can do everything except delete the group
::    %moderator: Moderator, can add/remove/ban users
::    %janitor: Has no special meaning inside group-store,
::    but may be given additional privileges in other apps.
::
+$  role-tag
  ?(%admin %moderator %janitor)
::  $tags: a mapping from a $tag to the members it identifies
::
+$  tags  (jug tag ship)
::  $group: description of a group of users
::
::    .members: members of the group
::    .tag-queries: a map of tags to subsets of members
::    .policy: permissions for the group
::    .hidden: is group unmanaged
+$  group
  $:  members=(set ship)
      =tags
      =policy
      hidden=?
  ==
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
    $%  [%invite diff:invite]
        [%open diff:open]
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
      [%open ban-ranks=(set rank:title) banned=(set ship)]
    :: $diff: ban or allow ranks and ships
    ::
    +$  diff
      $%  [%allow-ranks ranks=(set rank:title)]
          [%ban-ranks ranks=(set rank:title)]
          [%ban-ships ships=(set ship)]
          [%allow-ships ships=(set ship)]
      ==
    --
  --
--
