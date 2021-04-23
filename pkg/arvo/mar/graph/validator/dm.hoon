/-  *post, met=metadata-store, graph=graph-store, hark=hark-graph-hook
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ++  notification-kind
    ^-  (unit notif-kind:hark)
    ?+  index.p.i  ~
      [@ ~]  `[%message [0 1] %count %none]
    ==
  ::
  --
++  grab
  |%
  ++  noun
    |=  p=*
    =/  ip  ;;(indexed-post p)
    ?>  ?=(?([@ ~] [@ @ ~]) index.p.ip)
    ip
  --
::
++  grad  %noun
--
