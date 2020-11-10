/-  sur=hark-graph-hook
/+  graph-store, resource
^?
=<  [. sur]
=,  sur
|%
++  dejs
  =,  dejs:format
  |%
  ++  graph-indices
    %-  ot
    :~  graph+dejs-path:resource
        indices+(as graph-index)
    ==
  ::
  ++  graph-index
    ^-  $-(json index:graph-store)
    (su ;~(pfix net (more net dem)))
  ::
  ++  action
    %-  of
    :~  listen+dejs-path:resource
        ignore+dejs-path:resource
        set-mentions+bo
        set-watch-on-self+bo
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
  ++  graph-indices
    |=  [graph=resource indices=(set index:graph-store)]
    %-  pairs
    :~  graph+s+(enjs-path:resource graph)
        indices+a+(turn ~(tap in indices) index:enjs:graph-store)
    ==
  ::
  ++  action
    |=  act=^action
    ^-  json
    %+  frond  -.act
    ?-  -.act  
      %set-watch-on-self  b+watch-on-self.act
      %set-mentions  b+mentions.act
      ?(%listen %ignore)   s+(enjs-path:resource graph.act)
    ==
  ::
  ++  update
    |=  upd=^update
    ^-  json
    ?.  ?=(%initial -.upd)
      (action upd)
    %+  frond  -.upd
    %-  pairs
    :~  'watchOnSelf'^b+watch-on-self.upd
        'mentions'^b+mentions.upd
        watching+a+(turn ~(tap in watching.upd) |=(r=resource s+(enjs-path:resource r)))
    ==
  --
--
