/-  *contact-store
|%
+$  contact-view-action
  $%  ::  %create: create in both groups and contacts
      ::
      [%create =path ships=(set ship)]
      ::  %add: add to groups and send invites
      ::
      [%add =path ships=(set ship)]
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
