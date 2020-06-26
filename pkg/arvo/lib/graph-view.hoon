/-  sur=graph-view
/+  res=resource
^?
=<  [sur .]
=,  sur
|%
++  dejs
  =,  dejs:format
  |%
  ++  action
    |=  jon=json
    ^-  ^action
    =<  (decode jon)
    |%
    ++  decode
      %-  of
      :~  [%fetch fetch]
      ==
    ::
    ++  fetch
      %-  ot
      :~  [%connection ni]
          [%type query-type]
      ==
    ::
    ++  query-type
      %-  of
      :~  [%all ul]
          [%keys ul]
          [%tags ul]
          [%tag-queries ul]
          [%graph dejs:res]
          [%graph-subset graph-subset]
          [%node node]
          [%post node]
          [%node-children node]
          [%node-children-subset node-children-subset]
      ==
    ::
    ++  index  (su ;~(pfix net (more net dem)))
    ++  graph-subset
      %-  ot
      :~  [%resource dejs:res]
          [%start (mu ni)]
          [%end (mu ni)]
      ==
    ::
    ++  node
      %-  ot
      :~  [%resource dejs:res]
          [%index index]
      ==
    ::
    ++  node-children-subset
      %-  ot
      :~  [%resource dejs:res]
          [%start (mu ni)]
          [%end (mu ni)]
          [%index index]
      ==
    --
  --
--
