::
=<  policy
|%
+$  kind  ?(%read %write)
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
    [%invite current=(set ship) pending=(set ship)]
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
