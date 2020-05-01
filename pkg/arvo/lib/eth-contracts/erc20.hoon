/-  *eth-contracts-erc20
|%
  ++  gift-to-json
  |=  =gift
  =,  enjs:format
  ?-  -.gift
      %history
    %+  frond  %history
    [%a (turn loglist.gift event-log-to-json)]
      %log
    %+  frond  %event-update
    (event-log-to-json event-log.gift)
      %read-call
    !!
      %read-tx
    !!
  ==
  ++  event-log-to-json
    |=  [=event-log]
    ^-  json
    =,  enjs:format
    ?-  -.event-data.event-log
          %approval
      %-  pairs
      :~
        [%type [%s %approval]]
        [%address [%s (crip ((x-co:co 1) address.event-log))]]
        :-  %payload
        %-  pairs
        :~
          [%owner [%s (crip ((x-co:co 1) owner.event-data.event-log))]]
          [%spender [%s (crip ((x-co:co 1) spender.event-data.event-log))]]
          [%value [%n (crip ((d-co:co 1) value.event-data.event-log))]]

        ==
      ==        %transfer
      %-  pairs
      :~
        [%type [%s %transfer]]
        [%address [%s (crip ((x-co:co 1) address.event-log))]]
        :-  %payload
        %-  pairs
        :~
          [%from [%s (crip ((x-co:co 1) from.event-data.event-log))]]
          [%to [%s (crip ((x-co:co 1) to.event-data.event-log))]]
          [%value [%n (crip ((d-co:co 1) value.event-data.event-log))]]

        ==
      ==
    ==
--