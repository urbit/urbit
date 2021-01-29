/-  sur=group-view
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
    :~  join+join
    ==
  ::
  ++  join
    %-  ot
    :~  resource+dejs:resource
        ship+ship
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
  ++  update
    |=  upd=^update
    %+  frond  %group-view-update
    %+  frond  -.upd
    ?-  -.upd
      %initial  (initial +.upd)
      %progress  (progress +.upd)
    ==
  ::
  ++  progress
    |=  [rid=resource prog=^progress]
    %-  pairs
    :~  resource+s+(enjs-path:resource rid)
        progress+s+prog
    ==
  ::
  ++  initial
    |=  resources=(set resource)
    ^-  json
    a+(turn ~(tap in resources) (cork enjs-path:resource (lead %s)))
  --
--
