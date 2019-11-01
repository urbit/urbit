|%
++  serial  @uvH
::
+$  invite
  $:  =ship           ::  ship to subscribe to upon accepting invite
      app=@tas        ::  app to subscribe to upon accepting invite
      =path           ::  path to subscribe to upon accepting invite
      recipient=ship  ::  recipient to receive invite
      text=cord       ::  text to describe the invite
  ==
::
::  +invites: each application using invites creates its own path that
::  contains a map of serial to invite. this allows it to only receive
::  invites that it is concerned with
::
+$  invites  (map path invitatory)   ::  main data structure
::
+$  invitatory  (map serial invite)  ::  containing or conveying an invitation
::
::
+$  invite-base
  $%  [%create =path]                       ::  create a path
      [%delete =path]                       ::  delete a path
      [%invite =path uid=serial =invite]    ::  receive an invite at path/uid
      [%decline =path uid=serial]           ::  decline an invite at path/uid
  ==
::
+$  invite-action
  $%  invite-base
      [%accept =path uid=serial]            ::  accept an invite at path/uid
  ==
::
+$  invite-update
  $%  invite-base
      [%invitatory =invitatory]             ::  receive invitatory
      [%accepted =path uid=serial =invite]  ::  an invite has been accepted
  ==
::
+$  invite-diff
  $%  [%invite-initial invites]
      [%invite-update invite-update]
  ==
--

