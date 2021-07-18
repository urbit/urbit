/-  sur=hark-chat-hook
^?
=<  [. sur]
=,  sur
|%
++  dejs
  =,  dejs:format
  |%
  ++  action
    %-  of
    :~  listen+pa
        ignore+pa
        set-mentions+bo
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
  ++  update
    |=  upd=^update
    %+  frond  -.upd
    ?-  -.upd
      ?(%listen %ignore)  (path chat.upd)
      %set-mentions  b+mentions.upd
      %initial   a+(turn ~(tap in watching.upd) path)
    ==
  --
--

