/-  *group-hook
=,  dejs:format
|_  act=action
++  grab
  |%
  ++  noun  action
  ++  json
    |=  jon=^json
    =<  (parse-action jon)
    |%
    ++  parse-action
      %-  of
      :~
        [%add add-action]
        [%remove pa]
      ==
    ::
    ++  add-action
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%path pa]
      ==
    --
  --
--
