/-  rpc=json-rpc
/+  naive
::
|%
++  point-to-json
    |=  =point:naive
    ^-  json
    |^
    :-  %o
    %-  molt
    ^-  (list [@t json])
    :~  ['dominion' s+dominion.point]
      ::
        :-  'ownership'
        :-  %o
        %-  molt
        =*  own  own.point
        ^-  (list [@t json])
        :~  ['owner' (own-to-json owner.own)]
            ['spawnProxy' (own-to-json spawn-proxy.own)]
            ['managementProxy' (own-to-json management-proxy.own)]
            ['votingProxy' (own-to-json voting-proxy.own)]
            ['transferProxy' (own-to-json transfer-proxy.own)]
        ==
      ::
        :-  'network'
        :-  %o
        =,  enjs:format
        %-  molt
        =*  net  net.point
        ^-  (list [@t json])
        :*  ['life' (numb life.net)]
            ['pass' s+(crip ((x-co:co 20) pass.net))]
            ['rift' (numb rift.net)]
            :-  'sponsor'
            :-  %o
            %-  molt  ^-  (list [@t json])
            ~[['has' b+has.sponsor.net] ['who' (ship who.sponsor.net)]]
          ::
            ?~  escape.net  ~
            ['escape' (ship u.escape.net)]~
    ==  ==
    ::
    ++  own-to-json
      |=  [=address:naive =nonce:naive]
      ^-  json
      :-  %o
      %-  molt  ^-  (list [@t json])
      :~  ['address' s+(crip "0x{((x-co:co 20) address)}")]
          ['nonce' (numb:enjs:format nonce)]
      ==
    --
--