::  eth-watcher utilities
::
/-  *eth-watcher
::
|%
::  +log-to-id: extract the event-id from an event-log
::
++  log-to-id
  |=  log=event-log:rpc:ethereum
  ^-  event-id:ethereum
  ?>  ?=(^ mined.log)
  :-  block-number.u.mined.log
  log-index.u.mined.log
::
::  +store-new-logs: add logs to an old loglist, ensuring newest-first ordering
::
::    assumes :new is already ordered newest-first
::
++  store-new-logs
  |=  [new=loglist old=loglist]
  ^-  loglist
  ?~  new  old
  =+  new-place=(log-to-id i.new)
  |-
  ?~  old  [i.new old]
  =+  old-place=(log-to-id i.old)
  ::  if the :old-place is older than :new-place,
  ::    put :new-place down, and grab the next one from :new
  ::  otherwise, keep looking through :old
  ::
  ?:  ?|  (gth block.new-place block.old-place)
          ?&  =(block.new-place block.old-place)
              (gth log.new-place log.old-place)
          ==
      ==
    [i.new ^$(new t.new)]
  [i.old $(old t.old)]
--
