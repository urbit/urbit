/-  *group
^?
|%
+$  action
  $%  ::  %create: create a new chat
      ::
      $:  %create
          title=@t
          description=@t
          app-path=path
          group-path=path
          =policy
          members=(set ship)
          allow-history=?
          managed=?
      ==
      [%delete app-path=path]
      [%join =ship app-path=path ask-history=?]
      [%invite app-path=path ships=(set ship)]
      ::  %groupify: for unmanaged %village chats: recreate as group-based chat
      ::
      ::    will delete the old chat, recreate it based on a proper group,
      ::    and invite the current whitelist to that group.
      ::    existing messages get moved over.
      ::
      ::    if :existing is provided, associates chat with that group instead
      ::    of creating a new one. :inclusive indicates whether or not to add
      ::    chat members to the group, if they aren't there already.
      ::
      [%groupify app-path=path existing=(unit [group-path=path inclusive=?])]
  ==
--
