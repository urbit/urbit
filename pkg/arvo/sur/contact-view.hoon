/-  *contact-store, *group, *resource
::
|%
+$  contact-view-action
  $%  ::  %create: create in both groups and contacts
      ::
      [%create name=term =policy title=@t description=@t]
      ::  %join: join open group in both groups and contacts
      ::
      [%join =resource]
      ::  %invite: invite to invite-only group and contacts
      ::
      [%invite =resource =ship text=cord]
      ::  %remove: remove from both groups and contacts
      ::
      [%remove =path =ship]
      ::  %delete: delete in both groups and contacts
      ::
      [%delete =path]
      ::  %share: send %add contact-action to to recipient's contact-hook
      ::
      [%share recipient=ship =path =ship =contact]
      ::  %groupify: create contacts object for a preexisting group
      ::
      [%groupify =resource title=@t description=@t]
  ==
--
