/-  *resource
|%
++  serial  @uvH
::
+$  invite
  $:  =ship           ::  ship to subscribe to upon accepting invite
      app=@tas        ::  app to subscribe to upon accepting invite
      =resource       ::  path to subscribe to upon accepting invite
      recipient=ship  ::  recipient to receive invite
      text=cord       ::  text to describe the invite
  ==
::
::  +invites: each application using invites creates its own resource that
::  contains a map of serial to invite. this allows it to only receive
::  invites that it is concerned with
::
+$  invites  (map resource invitatory)   ::  main data structure
::
+$  invitatory  (map serial invite)  ::  containing or conveying an invitation
::
+$  invite-base
  $%  [%create =resource]                     ::  create a resource
      [%delete =resource]                     ::  delete a resource
      [%invite =resource uid=serial =invite]  ::  receive an invite at res/uid
      [%decline =resource uid=serial]         ::  decline an invite at res/uid
  ==
::
+$  invite-action
  $%  invite-base
      [%accept =resource uid=serial]            ::  accept an invite at res/uid
  ==
::
+$  invite-update
  $%  invite-base
      [%initial =invites]
      [%invitatory =invitatory]                 ::  receive invitatory
      [%accepted =resource uid=serial =invite]  ::  an invite has been accepted
  ==
--

