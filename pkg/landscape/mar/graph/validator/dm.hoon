/-  *post, met=metadata-store, hark=hark-graph-hook
/+  graph=graph-store
|_  i=indexed-post
::
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
    |=  title=cord
    ^-  (unit notif-kind:hark)
    ?+  index.p.i  ~
        [@ @ ~]  
      :-  ~
      :*  ~[text+'New messages from ' ship+author.p.i]
          (hark-contents:graph contents.p.i)
          [1 2]  %count  %none
      ==
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
