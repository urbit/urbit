/-  sur=hark-chat-hook
^?
=<  [. sur]
=,  sur
|%
::
++  dejs
  =,  dejs:format
  |%
  ::
  ++  action
    %-  of
    :~  listen+pa
        ignore+pa
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
  ::
  ++  update
    |=  upd=^update
    %+  frond  -.upd
    ?-  -.upd
      ?(%listen %ignore)  (path chat.upd)
      %initial   a+(turn ~(tap in watching.upd) path)
    ==
  --
--

