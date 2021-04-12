::  list of ethereum transactions
::
/+  *ethereum
=,  format
=,  rpc
=,  mimes:html
::
|_  txs=(list transaction)
++  u-parser
  %-  cook  :_  nuck:so
  |=  =coin
  ?>  ?=(%$ -.coin)
  ?>  ?=(%u (end 3 p.p.coin))
  `@`q.p.coin
::
++  grab
  |%
  ++  mime
    |=  (pair mite octs)
    =/  wan=wain  (to-wain q.q)
    ?>  ?=(^ wan)
    %+  murn  t.wan
    |=  tx=@t
    ^-  (unit transaction)
    ?:  =('' tx)
      ~
    :-  ~
    %+  rash  tx
    ;~  (glue com)
      u-parser
      u-parser
      u-parser
      ;~(pfix (jest '0x') hex)
      u-parser
      ;~(pfix (jest '0x') hex)
      u-parser
    ==
  ++  noun  (list transaction)
  --
::
++  grow
  =>  v=.
  |%
  ++  mime
    =>  v
    :-  /text/plain
    %-  as-octs  %-  of-wain
    =-  (weld - '' ~)
    :-  'nonce,gas-price,gas,to,value,data,chain-id'
    %+  turn  txs
    |=  transaction
    ^-  @t
    %+  rap  3
    :~  (scot %ui nonce)
          ','
        (scot %ui gas-price)
          ','
        (scot %ui gas)
          ','
        (crip (address-to-hex to))
          ','
        (scot %ui value)
          ','
        (crip (prefix-hex (render-hex-bytes (max 1 (met 3 data)) `@`data)))
          ','
        (scot %ux chain-id)
    ==
  --
++  grad  %mime
--
