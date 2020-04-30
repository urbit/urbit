/-  *erc20-store
|%
  ++  update-to-json
    |=  =gift
    =,  enjs:format
    |^
      ?-  -.gift
          %initial
        %+  frond
          %initial
        %-  pairs
        :~  [%balances]
        ==
      ==
    ++  balances
      ^-  json
      :-  %a
      %+  turn  ~(tap by balances.gift)
      |=  [=contract-id [=address:ethereum balance=@ud]]
      %-  pairs
      :~  [%contract [%s contract-id]]
          [%balance  [%s ((d-co:co (met 3 balance)) balance)]]
      ==

    --
--
