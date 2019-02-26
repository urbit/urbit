::  gaze: azimuth analytics
::
/+  *eth-watcher
::
=,  ethereum
=,  azimuth
::
|%
++  state
  $:  logs=loglist
  ==
::
::
++  move  (pair bone card)
++  card
  $%  [%poke wire [ship %eth-watcher] %eth-watcher-action action]
  ==
--
::
|_  [bowl:gall state]
++  prep
  |=  old=(unit state)
  ?~  old
    [~ ..prep]
  [~ ..prep(+<+ u.old)]
::
++  poke-noun
  |=  a=?(%look %simple)
  ^-  (quip move _+>)
  ?-  a
      %look
    :_  +>.$
    :_  ~
    ^-  move
    :-  ost
    :*  %poke
        `wire`/look
        `[ship %eth-watcher]`[our %eth-watcher]
        %eth-watcher-action
      ::
        ^-  action
        :+  %watch  dap
        :*  (need (de-purl:html 'http://eth-mainnet.urbit.org:8545'))
            public:contracts
            ~
            ~[azimuth:contracts]
            ~
        ==
    ==
  ::
      %simple
    =/  loz=loglist
      .^(loglist %gx /(scot %p our)/eth-watcher/(scot %da now)/[dap]/noun)
    ::  lockup: ships that went into lockup
    ::
    =/  lockup=(set ship)
      %-  ~(gas in *(set ship))
      %+  murn  loz
      |=  log=event-log:rpc
      ^-  (unit ship)
      =+  dif=(event-log-to-point-diff log)
      ?~  dif  ~
      =*  diz  q.u.dif
      ?.  ?=(%owner -.diz)  ~
      ?:  =(linear-star-release:contracts new.diz)  `p.u.dif
      ?:  =(conditional-star-release:contracts new.diz)  `p.u.dif
      ~
    ::
    =+  activated=(filter loz activated:azimuth-events lockup)
    =+  spawned=(filter loz spawned:azimuth-events lockup)
    =+  transfer=(filter loz owner-changed:azimuth-events lockup)
    =+  transfer-u=(~(gas in *(set ship)) transfer)
    =+  rekeyed=(filter loz changed-keys:azimuth-events lockup)
    =+  rekeyed-u=(~(gas in *(set ship)) rekeyed)
    ~&  ;:  weld
          "Since launch, there have been: "
          "- {<(lent activated)>} points activated "
          "(out of {<(lent spawned)>} spawned), "
          "- {<(sub (sub (lent transfer) (lent spawned)) (lent activated))>} point transfers "
          "({<(sub ~(wyt in transfer-u) (lent spawned))>} unique), "
          "- {<(lent rekeyed)>} key configurations "
          "({<~(wyt in rekeyed-u)>} unique)."
        ==
    [~ +>.$]
  ==
::
::  filter: find ships that were the subject of some :event
::
++  filter
  |=  [logs=loglist event=@ux exclude=(set ship)]
  %+  murn  logs
  |=  log=event-log:rpc
  ^-  (unit ship)
  ?.  =(event i.topics.log)  ~
  =+  dif=(event-log-to-point-diff log)
  ?~  dif  ~
  =/  who
    ?:  ?=(%spawned -.q.u.dif)  who.q.u.dif
    p.u.dif
  ?:  (~(has in exclude) who)  ~
  `who
--
