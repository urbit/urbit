/-  *rw-security
|%
+$  chat-view-action
  $%  $:  %create
          =path
          security=rw-security
          read=(set ship)
          write=(set ship)
          allow-history=?
      ==
      [%delete =path]
      [%join =ship =path ask-history=?]
  ==
--
