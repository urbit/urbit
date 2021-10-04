/-  sur=hark-graph-hook, post
/+  graph-store, resource
^?
=<  [. sur]
=,  sur
|%
++  dejs
  =,  dejs:format
  |%
  ::
  ++  index
    ^-  $-(json index:graph-store)
    (su ;~(pfix fas (more fas dem)))
  ::
  ++  graph-index
    %-  ot
    :~  graph+dejs-path:resource
        index+index
    ==
  ::
  ++  action
    %-  of
    :~  listen+graph-index
        ignore+graph-index
        set-mentions+bo
        set-watch-on-self+bo
    ==
  --
::
++  enjs
  =,  enjs:format
  |%
  ::
  ++  graph-index
    |=  [graph=resource =index:post]
    %-  pairs
    :~  graph+s+(enjs-path:resource graph)
        index+(index:enjs:graph-store index)
    ==
  ::
  ++  action
    |=  act=^action
    ^-  json
    %+  frond  -.act
    ?-  -.act
      %set-watch-on-self  b+watch-on-self.act
      %set-mentions  b+mentions.act
      ?(%listen %ignore)   (graph-index graph.act index.act)
    ==
  ::
  ::
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
        :+  %watching  %a
        (turn ~(tap in watching.upd) graph-index)
    ==
  --
--
