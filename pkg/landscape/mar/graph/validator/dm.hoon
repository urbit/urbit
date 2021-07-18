/-  *post, met=metadata-store, graph=graph-store, hark=hark-graph-hook
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ::
  ++  graph-indexed-post
    ^-  indexed-post
    ?>  ?=(?([@ ~] [@ @ ~]) index.p.i)
    ?>  (lth i.index.p.i (bex 128))
    i
  ::
  ++  notification-kind
    ^-  (unit notif-kind:hark)
    ?+  index.p.i  ~
      [@ @ ~]  `[%message [1 2] %count %none]
    ==
  ::
  --
++  grab
  |%
  ++  noun  indexed-post
  --
::
++  grad  %noun
--
