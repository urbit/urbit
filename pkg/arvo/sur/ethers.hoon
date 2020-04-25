=,  able:jael
|%
  ::  eth-watcher: ethereum event log collector
  ::

  ::  disavows: newest block first
  +$  disavows      (list id:block)
  +$  pending-logs  (map number:block loglist)
  ::
  +$  gift
    $%  ::  %history: full event log history, oldest first
        ::
        [$history =loglist]
        ::  %log: newly added log
        ::
        [$log event-log=generic-data]
        ::  %disavow: forget logs
        ::
        ::    this is sent when a reorg happens that invalidates
        ::    previously-sent logs
        ::
        [$disavow =id:block]
        [$read-call generic-data]
        [$read-tx generic-data]
    ==
  +$  poke
    $%  (make-action:builders $call generic-data)
        (make-action:builders $send-tx generic-data)
        [$event-subscribe =path config=watch-config]
        [$clear =path]
        [$add-abi name=@tas =json]
        [$add-contracts addresses=(list address:ethereum) abi=@tas]
    ==
  +$  generic-data  [@tas *]
  +$  loglist  (list (event-log-config:builders generic-data))
  +$  watch-config
    %-  watch-config:builders
    $:(@tas topics=(list ?(* (list *))))
::
  ++  builders
    |%
      ++  watch-config
        |$  [topics]
        $:  url=@ta
            eager=?
            refresh-rate=@dr
            from=number:block
            contracts=(list address:ethereum)
            =topics
        ==
      ++  make-action
        |$  [name action]
        $:  name
            =path
            url=@ta
            contract=address:ethereum
            =action
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
