::  Creates a snapshot of the azimuth state and its indices
::  (owners and sposnors) from scrying /app/azimuth
::
::
/-  spider, *dice
/+  strand, strandio, naive, ethereum, dice
=,  strand=strand:spider
=,  jael
::
=>  |%  +$  card    card:agent:gall
        +$  task    task:clay
        +$  events  (list event-log:rpc:ethereum)
    --
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ file-name=@t] arg)
::
;<  nas=^state:naive  bind:m  (scry:strandio ^state:naive /gx/azimuth/nas/noun)
;<  =owners           bind:m  (scry:strandio owners /gx/azimuth/own/noun)
;<  =sponsors         bind:m  (scry:strandio sponsors /gx/azimuth/spo/noun)
;<  =events           bind:m  (scry:strandio events /gx/azimuth/logs/noun)
=/  =id:block         (last-block-id:dice events)
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
(pure:m !>('azimuth state saved'))
