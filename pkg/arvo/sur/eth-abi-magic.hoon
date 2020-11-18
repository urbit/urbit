=,  able:jael
|%
  ++  call
    $:  url=@ta
        to=address:ethereum
        action=cage
    ==
  ++  builders
    |%
      ++  watch-config
        |$  [topics]
        $:  url=@ta
            eager=?
            refresh-rate=@dr
            timeout-time=@dr
            from=number:block
            contracts=(list address:ethereum)
            =topics
        ==
      ++  send-tx
        |$  [name action]
        $:  url=@ta
            nonce=@ud
            gas-price=@ud
            gas=@ud
            to=address:ethereum
            value=@ud
            action=[name action]
            chain-id=@ux
        ==
      ++  call
        |$  [name action]
        $:  url=@ta
            to=address:ethereum
            action=[name action]
        ==
      ++  event-log-config
        |$  [event-data]
        $:  ::  null for pending logs
            $=  mined  %-  unit
            $:  log-index=@ud
                transaction-index=@ud
                transaction-hash=@ux
                block-number=@ud
                block-hash=@ux
                removed=?
            ==
          ::
            address=@ux
            =event-data
        ==
    --
--
