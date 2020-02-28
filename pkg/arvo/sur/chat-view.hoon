/-  *rw-security
|%
+$  chat-view-action
  $%  $:  %create
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
  ==
--
