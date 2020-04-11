/-  *contact-store
|%
+$  contact-view-action
  $%  ::  %create: create in both groups and contacts
      ::
      [%create =path ships=(set ship) title=@t description=@t]
      ::  %remove: remove from both groups and contacts
      ::
      [%remove =path =ship]
      ::  %delete: delete in both groups and contacts
      ::
      [%delete =path]
      ::  %share: send %add contact-action to to recipient's contact-hook
      ::
      [%share recipient=ship =path =ship =contact]
  ==
--
