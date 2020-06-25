/-  *bitcoin
|%
++  json-to-bitcoin-action
  |=  jon=json
  ^-  bitcoin-action
  =,  dejs:format
  |^  (parse-json jon)
  ++  parse-json
    %-  of
    :~  [%request request]
        [%add (cu trip so)]
    ==
  ::
  ++  request
    %-  ot
    :~  ['ship' (cu @p (su fed:ag))]
        ['network' (cu network so)]
    ==
  --
--
