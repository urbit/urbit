/-  sur=graph-view
/+  resource, group-store
^?
=<  [sur .]
=,  sur
|%
++  app-to-mark
  |=  app=app-name
  ?+  app  !!
    %chat  %graph-validator-chat
  ==
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
        ::groupify+groupify
        ::invite+invite
    ==
    ::
    ++  create
      %-  ot
      :~  resource+dejs:resource
          title+so
          description+so
          app+app
          associated+associated
      ==
    ::
    ++  app
      |=  jon=json
      ^-  app-name
      ?>  ?=(%s -.jon)
      %chat
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
    ++  groupify  !!
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
