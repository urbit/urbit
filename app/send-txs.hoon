::
/+  ceremony
::
|%
++  state
  $:  txs=(list @ux)
  ==
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire [~ ~] mark %hiss hiss:eyre]
      [%wait wire @da]
  ==
--
::
|_  [bol=bowl:gall state]
::
++  poke-noun
  |=  a=@t
  ^-  [(list move) _+>]
  ?.  =('start' a)  [~ +>]
  ~&  'loading txs'
  =/  tox=(list cord)
    .^  (list cord)  %cx
        /(scot %p our.bol)/home/(scot %da now.bol)/txs/txt
    ==
  =.  txs
    %+  turn  tox
    (cork trip tape-to-ux:ceremony)
  ~&  'sending moves'
  :_  +>.$
  :_  ~
  ^-  move
  :-  ost.bol
  :^  %hiss  /aaa  [~ ~]
  :+  %json-rpc-response  %hiss
  %+  json-request:ethereum
    =+  (need (de-purl:html 'http://localhost:8545'))
    -(p.p |)
  :-  %a
  %+  turn  txs
  |=  tx=@ux
  %+  request-to-json:ethereum  ~
  [%eth-send-raw-transaction tx]
--
