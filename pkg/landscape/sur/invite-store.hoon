/-  *resource
|%
++  serial  @uvH
::
+$  invite
  $:  =ship           ::  ship to subscribe to upon accepting invite
      app=@tas        ::  app to subscribe to upon accepting invite
      =resource       ::  resource to subscribe to upon accepting invite
      recipient=ship  ::  recipient to receive invite
      text=cord       ::  text to describe the invite
  ==
::
+$  multi-invite
  $:  =ship                  ::  ship to subscribe to upon accepting invite
      app=@tas               ::  app to subscribe to upon accepting invite
      =resource              ::  resource to subscribe to upon accepting invite
      recipients=(set ship)  ::  recipient to receive invite
      text=cord              ::  text to describe the invite
  ==
::
::  +invites: each application using invites creates its own resource that
::  contains a map of serial to invite. this allows it to only receive
::  invites that it is concerned with
::
+$  invites     (map term invitatory)   ::  main data structure
::
+$  invitatory  (map serial invite)  ::  containing or conveying an invitation
::
+$  invite-base
  $%  [%create =term]                     ::  create a resource
      [%delete =term]                     ::  delete a resource
      [%invite =term uid=serial =invite]  ::  receive an invite at term/uid
      [%decline =term uid=serial]         ::  decline an invite at term/uid
  ==
::
+$  action
  $%  invite-base
      [%accept =term uid=serial]            ::  accept an invite at term/uid
      [%invites =term uid=serial invites=multi-invite]
  ==
::
+$  update
  $%  invite-base
      [%initial =invites]
      [%invitatory =invitatory]                 ::  receive invitatory
      [%accepted =term uid=serial =invite]      ::  an invite has been accepted
  ==
--

