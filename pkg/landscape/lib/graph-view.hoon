/-  sur=graph-view, store=graph-store
/+  resource, group-store, metadata-store
^?
=<  [sur .]
=,  sur
|%
++  dejs
  =,  dejs:format
  |%
  ++  action
    |^
    ^-  $-(json ^action)
    %-  of
    :~  create+create
        delete+delete
        join+join
        leave+leave
        groupify+groupify
        eval+so
        pending-indices+pending-indices
        create-group-feed+create-group-feed
        disable-group-feed+disable-group-feed
        ::invite+invite
    ==
    ::
    ++  create
      %-  ou
      :~  resource+(un dejs:resource)
          title+(un so)
          description+(un so)
          mark+(uf ~ (mu so))
          associated+(un associated)
          module+(un so)
      ==
    ::
    ++  leave
      %-  ot
      :~  resource+dejs:resource
      ==
    ::
    ++  delete
      %-  ot
      :~  resource+dejs:resource
      ==
    ::
    ++  join
      %-  ot
      :~  resource+dejs:resource
          ship+(su ;~(pfix sig fed:ag))
      ==
    ::
    ++  groupify  
      %-  ou
      :~  resource+(un dejs:resource)
          to+(uf ~ (mu dejs:resource))
      ==
    ::
    ++  pending-indices  (op hex (su ;~(pfix fas (more fas dem))))
    ::
    ++  invite    !!
    ::
    ++  associated
      %-  of
      :~  group+dejs:resource
          policy+policy:dejs:group-store
      ==
    ::
    ++  create-group-feed
      %-  ot
      :~  resource+dejs:resource
          vip+vip:dejs:metadata-store
      ==
    ::
    ++  disable-group-feed
      %-  ot
      :~  resource+dejs:resource
      ==
    --
  --
::
++  enjs
  =,  enjs:format
  |%
  ++  action
    |=  act=^action
    ^-  json
    ?>  ?=(%pending-indices -.act)
    %+  frond  %pending-indices
    %-  pairs
    %+  turn  ~(tap by pending.act)
    |=  [h=hash:store i=index:store]
    ^-  [@t json]
    =/  idx  (index i)
    ?>  ?=(%s -.idx)
    [p.idx s+(scot %ux h)]
  ::
  ++  index
    |=  i=index:store
    ^-  json
    ?:  =(~ i)  s+'/'
    =/  j=^tape  ""
    |-
    ?~  i  [%s (crip j)]
    =/  k=json  (numb i.i)
    ?>  ?=(%n -.k)
    %_  $
        i  t.i
        j  (weld j (weld "/" (trip +.k)))
    ==
  --
--
