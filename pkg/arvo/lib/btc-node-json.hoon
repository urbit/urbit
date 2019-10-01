/-  *btc-node-store
|%
++  rpc
  =,  ^rpc
  |%
  ++  request-to-rpc
    =,  enjs:format
    |=  req=request
    ^-  request:rpc:jstd
    :-  -.req
    ?-  -.req
        %generate
      :-  'generate'
      :-  %list
      ^-  (list json)
      :-  (numb blocks.req)
      ?~  max-tries.req  ~
      [(numb u.max-tries.req) ~]
    ::
        %get-block-count
      :-  'getblockcount'
      list+~
    ::
        %list-wallets
      :-  'listwallets'
      list+~
    ::
        %create-wallet
      :-  'createwallet'
      :-  %list
      :~  s+name.req
          b+disable-private-keys.req
      ==
    ==
  ::
  ++  parse-response
    =,  dejs:format
    |=  res=response:rpc:jstd
    ^-  response
    ~|  -.res
    ?>  ?=(%result -.res)
    ?+  id.res
      ~|  [%unsupported-response id.res]
      !!
    ::
        %generate
      :-  id.res
      %.  res.res
      (ar (su hex))
    ::
        %get-block-count
      :-  id.res
      (ni res.res)
    ::
        %list-wallets
      :-  id.res
      %.  res.res
      (ar so)
    ::
        %create-wallet
      :-  id.res
      %.  res.res
      (ot name+so warning+so ~)
    ==
  --
--
