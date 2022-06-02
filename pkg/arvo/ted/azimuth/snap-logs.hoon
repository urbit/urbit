::  Creates a snapshot of the azimuth state and its indices
::  (owners and sposnors) from a list of ethereum logs
::
/-  spider, *dice
/+  strand,
    azimuth,
    strandio,
    naive,
    lib=naive-transactions,
    ethereum,
    dice
::  /*  logs  %eth-logs  /app/azimuth/logs/eth-logs
=/  logs  ~
=,  strand=strand:spider
::
=>  |%  +$  card    card:agent:gall
        +$  task    task:clay
        +$  id      id:block:jael
        +$  events  (list event-log:rpc:ethereum)
    --
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =net file-name=@t] arg)
::
=/  [azimuth-contract=@ux chain-id=@]
  [azimuth chain-id]:(get-network:dice net)
::
%-  %-  slog  :_  ~
    leaf+"azimuth: creating snapshot with {<(lent logs)>} events"
::
=/  [=id nas=^state:naive]
  %+  roll  `events`logs
  |=  [log=event-log:rpc:ethereum =id nas=^state:naive]
  ?~  mined.log
    id^nas
  =/  =^input:naive
    :-  block-number.u.mined.log
    ?:  =(azimuth-contract address.log)
      :-  %log
      [address.log (data-to-hex:dice data.log) topics.log]
    ?~  input.u.mined.log
      [%bat *@]
    [%bat u.input.u.mined.log]
  =.  id
    ?.  (gth block-number.u.mined.log number.id)
      id
    [block-hash block-number]:u.mined.log
  =^  *  nas
    (%*(. naive lac |) verifier:lib chain-id nas input)
  id^nas
::
=/  [=sponsors =owners]  (create-indices:dice nas)
::
%-  %-  slog
    :~  leaf+"points: {<~(wyt by points.nas)>}"
        leaf+"sponsors: {<~(wyt by sponsors)>}"
        leaf+"owners: {<~(wyt by owners)>}"
        leaf+"block-number: {<number.id>}"
        leaf+"block-hash: {<hash.id>}"
    ==
::
=/  =path  /app/azimuth/[file-name]/azimuth-snapshot
=/  =cage
  :-  %azimuth-snapshot
  !>  ^-  snap-state
  [%0 id nas owners sponsors]
=/  =task  [%info %base %& [path %ins cage]~]
=/  =card  [%pass /next %arvo %c task]
;<  ~  bind:m  (send-raw-card:strandio card)
(pure:m !>('azimuth logs processed'))
