|%
++  serial  @uvH
::
+$  invite
  $:  =path
      =ship
      recipient=ship
      app=@tas
      text=cord
  ==
::
+$  invitatory  (map serial invite)  ::  containing or conveying an invitation
::
+$  invites  (map path invitatory)
::
+$  invite-base
  $%  [%create =path]
      [%delete =path]
      [%invite =path uid=serial =invite]
      [%decline =path uid=serial]
  ==
::
+$  invite-action
  $%  invite-base
      [%accept =path uid=serial]
  ==
::
+$  invite-update
  $%  invite-base
      [%invitatory =invitatory]
      [%accepted =path uid=serial =invite]
  ==
--

