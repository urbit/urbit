/-  sur=hark-group-hook
/+  resource
^?
=<  [. sur]
=,  sur
|%
++  dejs
  =,  dejs:format
  |%
  ++  action
    %-  of
    :~  listen+dejs-path:resource
        ignore+dejs-path:resource
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
  ++  res
    (cork enjs-path:resource (lead %s))
  ::
  ++  update
    |=  upd=^update
    %+  frond  -.upd
    ?-  -.upd
      ?(%listen %ignore)  (res group.upd)
      :: 
        %initial  
      :-  %a
      (turn ~(tap in watching.upd) res)
    ==
  --
--
