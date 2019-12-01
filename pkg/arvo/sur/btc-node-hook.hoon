|%
++  address  @uc
++  blockhash  @ux
::
+$  btc-node-hook-action  request:btc-rpc
+$  rule  %segwit
+$  capability
  $?  %longpoll
      %coinbasetxn
      %coinbasevalue
      %proposal
      %serverlist
      %workid
  ==
+$  mode  ?(mode=%template [mode=%proposal data=@ux workid=(unit @t)])
+$  mutable  ?(%value %time %transactions %prevblock)
+$  command  [method=@t duration=@dr]
+$  network  ?(%main %test %regtest)
+$  transaction
  $:  data=@ux
      txid=@ux
      hash=@ux
      depends=(list @ud)
      fee=@ud
      sigops=@ud
      weight=@ud
  ==
+$  connection  ?(%inbound %outbound)
+$  node-address  [address=@t connected=connection]
+$  node-info  [added-node=@t connected=? addresses=(list node-address)]
+$  upload-target
  $:  timeframe=@dr
      target=@ud
      target-reached=?
      serve-historical-blocks=?
      bytes-left-in-cycle=@ud
      time-left-in-cycle=@dr
  ==
+$  network-type  ?(%ipv4 %ipv6 %onion)
+$  network-info
  $:  name=network-type
      limited=?
      reachable=?
      proxy=@t
      proxy-randomize-credentials=?
  ==
+$  local-address  [address=@t port=@ud score=@ud]
+$  node-address-info  [time=@da services=@ud address=@t port=@ud]
+$  peer-info
  $:  id=@ud
      addr=@t
      addr-bind=@t
      addr-local=(unit @t)
      services=@t
      relay-txes=?
      last-send=@da
      last-recv=@da
      bytes-sent=@ud
      bytes-recv=@ud
      conn-time=@da
      time-offset=@ud
      ping-time=@rd
      min-ping=@rd
      ping-wait=(unit @ud)
      version=@ud
      subver=@t
      inbound=?
      addnode=?
      starting-height=@ud
      ban-score=@ud
      synced-headers=@ud
      synced-blocks=@ud
      inflight=(list @ud)
      whitelisted=?
      min-fee-filter=@rd
      bytes-sent-per-msg=(map @t @ud)
      bytes-recv-per-msg=(map @t @ud)
  ==
::
+$  banned  [address=@t banned-until=@da ban-created=@da ban-reason=@t]
+$  logging-category
  $?  %net
      %tor
      %mempool
      %http
      %bench
      %zmq
      %db
      %rpc
      %estimatefee
      %addrman
      %selectcoins
      %reindex
      %cmpctblock
      %rand
      %prune
      %proxy
      %mempoolrej
      %libevent
      %coindb
      %qt
      %leveldb
  ==
::
++  btc-rpc
  |%
  +$  request
    $%  ::  node management
        ::
        [%generate blocks=@ud max-tries=(unit @ud)]
        ::  chain state
        ::
        [%get-block-count ~]
        ::  wallet management
        ::
        [%list-wallets ~]
        [%create-wallet name=@t disable-private-keys=_|]
        :: control
        ::
        [%get-memory-info ~]
        [%get-rpc-info ~]
        [%help command=(unit @t)]
        ::
        $:  %logging
            include=?(%all %none (list logging-category))
            exclude=?(%all %none (list logging-category))
        ==
        ::
        [%stop ~]
        [%uptime ~]
        :: generating
        ::
        [%generate-to-address n-blocks=@ud =address max-tries=(unit @ud) ~]
        :: mining
        ::
        $:  %get-block-template
            rules=(list rule)
            capabilities=(list capability)
            mode=(unit mode)
        ==
        [%get-mining-info ~]
        [%get-network-hash-ps n-blocks=(unit @ud) height=(unit @ud)]
        [%prioritise-transaction txid=@ux fee-delta=@ud]
        [%submit-block hex-data=@ux]
        [%submit-header hex-data=@ux]
        :: network
        ::
        [%add-node node=@if port=@ud command=?(%add %remove %onetry)]
        [%clear-banned ~]
        [%disconnect-node node=?(node-id=@t [address=@if port=@ud])]
        [%get-added-node-info node=(unit @if)]
        [%get-connection-count ~]
        [%get-net-totals ~]
        [%get-network-info ~]
        [%get-node-addresses count=(unit @ud)]
        [%get-peer-info ~]
        [%list-banned ~]
        [%ping ~]
        ::
        $:  %set-ban
            subnet=@t
            command=?(%add %remove)
            ban-time=(unit ?([%dr @dr] [%da @da]))
        ==
        ::
        [%set-network-active state=?]
        ::
    ==
  ::
  +$  response
    $%  [%generate blocks=(list blockhash)]
        [%get-block-count count=@ud]
        [%list-wallets wallets=(list @t)]
        [%create-wallet name=@t warning=@t]
        ::
        $:  %get-memory-info
            used=@ud
            free=@ud
            total=@ud
            locked=@ud
            chunks-free=@ud
            chunks-used=@ud
        ==
        ::
        [%get-rpc-info active-commands=(list command)]
        [%help help=wall]
        [%logging logging-config=(map @t ?)]
        [%stop res=@t]
        [%uptime uptime=@dr]
        [%generate-to-address blockhashes=(list blockhash)]
        ::
        $:  %get-block-template
            version=@ud
            rules=(list rule)
            vb-available=(map @t @ud)
            vb-required=@ud
            previous-blockhash=blockhash
            transactions=(list transaction)
            coinbase-aux=@t
            coinbase-value=@ud
            target=blockhash
            min-time=@da
            mutable=(list mutable)
            nonce-range=@t
            sigop-limit=@ud
            size-limit=@ud
            weight-limit=@ud
            cur-time=@da
            bits=@ux
            height=@ud
            default-witness-commitment=blockhash
        ==
        ::
        $:  %get-mining-info
            blocks=@ud
            current-block-weight=@ud
            current-block-tx=@ud
            difficulty=@rd
            network-hash-ps=@rd
            pooled-tx=@ud
            chain=network
            warnings=@t
        ==
        [%get-network-hash-ps hash-ps=@rd]
        [%prioritise-transaction res=?]
        [%submit-block res=@t]
        [%submit-header res=@t]
        ::
        [%add-node res=null]
        [%clear-banned res=null]
        [%disconnect-node res=null]
        [%get-added-node-info node-info=(list node-info)]
        [%get-connection-count connection-count=@ud]
        ::
        $:  %get-net-totals
            total-bytes-recv=@ud
            total-bytes-sent=@ud
            time-millis=@da
            =upload-target
        ==
        ::
        $:  %get-network-info
            version=@ud
            subversion=@t
            protocol-version=@ud
            local-services=@t
            local-relay=?
            time-offset=@ud
            connections=@ud
            network-active=?
            networks=(list network-info)
            relay-fee=@rd
            incremental-fee=@rd
            local-addresses=(list local-address)
            warnings=@t
        ==
        ::
        [%get-node-addresses (list node-address-info)]
        [%get-peer-info (list peer-info)]
        [%list-banned (list banned)]
        [%ping res=null]
        [%set-ban res=null]
        [%set-network-active res=?]
        ::
    ==
  --
--
