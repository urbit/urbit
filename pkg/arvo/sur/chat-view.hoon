/-  *rw-security
|%
+$  chat-view-action
  $%  $:  %create
          =path
          security=rw-security
          members=(set ship)
          allow-history=?
      ==
      [%delete =path]
      [%join =ship =path ask-history=?]
  ==
--
