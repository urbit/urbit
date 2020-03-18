/-  *rw-security
|%
+$  chat-view-action
  $%  ::  %create: create a new chat
      ::
      ::    if :app-path and :group-path are different, :members must be empty,
      ::    as the :group-path is assumed to exist.
      ::    if :app-path and :group-path are identical, and the :group-path
      ::    doesn't yet exist, will create a new group with :members.
      ::
      $:  %create
          title=@t
          description=@t
          app-path=path
          group-path=path
          security=rw-security
          members=(set ship)
          allow-history=?
      ==
      [%delete app-path=path]
      [%join =ship app-path=path ask-history=?]
      ::  %groupify: for unmanaged %village chats: recreate as group-based chat
      ::
      ::    will delete the old chat, recreate it based on a proper group,
      ::    and invite the current whitelist to that group.
      ::    existing messages get moved over.
      ::
      [%groupify app-path=path]
  ==
--
