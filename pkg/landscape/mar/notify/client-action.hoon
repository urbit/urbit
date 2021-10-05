/-  *notify
|_  act=client-action
++  grad  %noun
++  grow
  |%
  ++  noun  act
  --
++  grab
  |%
  ++  noun  client-action
  ++  json
    |=  jon=^json
    =,  dejs:format
    ^-  client-action
    |^
    %.  jon
    %-  of
    :~  connect-provider+connect-provider
        remove-provider+remove-provider
    ==
    ++  connect-provider
      %-  ot
      :~  who+(su fed:ag)
          service+so
          address+so
      ==
    ++  remove-provider
      %-  ot
      :~  who+(su fed:ag)
          service+so
      ==
    --
  --
--
