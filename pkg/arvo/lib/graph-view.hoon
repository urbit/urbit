/-  sur=graph-view
/+  resource, group-store
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
    ++  invite    !!
    ::
    ++  associated
      %-  of
      :~  group+dejs:resource
          policy+policy:dejs:group-store
      ==
    --
  --
--
