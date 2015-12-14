::
::::  /hoon/bit-api-call/mar
  ::
/?    314
/-    bit-api
[bit-api .]
|_  {access-token+@t req+bit-api-call}
::
++  wrap
  |=  {end-point+path req+$@($get {$post p+json})}
  [/com/coinbase/sandbox/api v1/end-point req ~ ['access_token' access-token]~]
::
++  order
  |=  {amount+@t currency+@t}  ^-  json
  (jobe qty/s/amount ?~(currency ~ [currency/s/currency ~]))
++  grow  
  |%  ++  httpreq
    %-  wrap
    |-
    ?-  -.req
      $list  [/accounts %get]
      $buy   [/buys %post (order +.req)]
      $sell  [/sells %post (order +.req)]
      $send  $(req [%txt-send (rsh 3 2 (scot %uc adr.req)) btc.req])
      $txt-send
        :+  /transactions/'send_money'  %post
        (joba %transaction (jobe to/s/to.req amount/s/btc.req ~))
      
    ==
  --
--
