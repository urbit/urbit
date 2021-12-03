::    =e -build-file %/lib/ethereum/hoon
::    =n -build-file %/lib/naive/hoon
::    =d -build-file %/sur/dice/hoon
::    =l .^((list event-log:rpc:e) %gx /=azimuth=/logs/noun)
::    =nas .^(^state:n %gx /=azimuth=/nas/noun)
::    =own .^(owners:d %gx /=azimuth=/own/noun)
::    =spo .^(sponsors:d %gx /=azimuth=/spo/noun)
::    =block-id =+  last=(rear l)  [block-hash block-number]:(need mined.last)
::    */app/azimuth/state/naive &naive [block-id nas own spo]
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
=+  !<([~ =net name=@t] arg)
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
    ==
::
=/  =path  /app/azimuth/[name]/naive
=/  =cage  naive+!>([%0 id nas owners sponsors])
=/  =task  [%info %base %& [path %ins cage]~]
=/  =card  [%pass /next %arvo %c task]
;<  ~  bind:m  (send-raw-card:strandio card)
(pure:m !>('naive state saved'))
