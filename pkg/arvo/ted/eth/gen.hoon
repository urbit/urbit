/-  spider
/+  eth-abi=eth-abi-gen, strandio
=,  strand=strand:spider
=*  card  card:agent:gall
|%
++  write-eth-files
  |=  [bol=bowl:spider act=[name=@tas =json]]
  =*  card  card:agent:gall
  ^-  (list card)
  |^
    =/  =contract:eth-abi  (parse-contract:eth-abi json.act name.act)
    =|  cards=(list card)
    =.  cards
    :_  cards
      %+  write-file  /sur/eth-contracts/[name.act]/hoon
      [%hoon !>((code-gen-types:eth-abi name.act contract))]
    =.  cards
    :_  cards
      %+  write-file  /lib/eth-contracts/[name.act]/hoon
      [%hoon !>((code-gen-lib:eth-abi contract name.act))]
    =.  cards
    ?~  events.contract  cards
    :_  cards
      %+  write-file  /mar/[(crip (zing ~["eth-contracts-" (trip name.act) "-diff"]))]/hoon
      [%hoon !>((code-gen-diff-mark:eth-abi (trip name.act)))]
    =.  cards
    ?~  events.contract  cards
    :_  cards
      %+  write-file  /mar/[(crip (zing ~["eth-contracts-" (trip name.act) "-esub"]))]/hoon
      [%hoon !>((code-gen-esub-mark:eth-abi (trip name.act)))]
    =.  cards
    ?~  ~(val by write-functions.contract)  cards
    :_  cards
      %+  write-file  /mar/[(crip (zing ~["eth-contracts-" (trip name.act) "-send"]))]/hoon
      [%hoon !>((code-gen-send-mark:eth-abi (trip name.act)))]
    =.  cards
    ?~  ~(val by read-functions.contract)  cards
    :_  cards
      %+  write-file  /mar/[(crip (zing ~["eth-contracts-" (trip name.act) "-call"]))]/hoon
      [%hoon !>((code-gen-call-mark:eth-abi (trip name.act)))]
    =.  cards
    ?~  (get-all-functions-with-outputs:eth-abi contract)  cards
    :_  cards
      %+  write-file  /mar/[(crip (zing ~["eth-contracts-" (trip name.act) "-res"]))]/hoon
      [%hoon !>((code-gen-res-mark:eth-abi (trip name.act)))]
    cards
  ++  our-beak  /(scot %p our.bol)/[q.byk.bol]/(scot %da now.bol)
  ++  write-file
    |=  [pax=path cay=cage]
    ^-  card
    =.  pax  (weld our-beak pax)
    [%pass (weld /write pax) %arvo %c %info (foal:space:userlib pax cay)]
  --
--
^-  thread:spider
|=  args=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([name=@tas =json $~] args)
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  ~  bind:m  (send-raw-cards:strandio (write-eth-files bowl [name json]))
(pure:m !>(~))
