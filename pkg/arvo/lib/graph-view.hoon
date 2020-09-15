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
        ::invite+invite
    ==
    ::
    ++  create
      %-  ou
      :~  resource+(un dejs:resource)
          title+(un so)
          description+(un so)
          mark+(uf ~ (mu so))
          app+(un so)
          associated+(un associated)
      ==
    ::
    ++  leave
      %-  ot
      :~  resource+dejs:resource
          app+so
      ==
    ::
    ++  delete
      %-  ot
      :~  resource+dejs:resource
          app+so
      ==
    ::
    ++  join
      %-  ot
      :~  resource+dejs:resource
          app+so
          ship+(su ;~(pfix sig fed:ag))
      ==
    ::
    ++  groupify  
      %-  ou
      :~  resource+(un dejs:resource)
          app+(un so)
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
