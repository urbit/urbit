/-  *sync-hook
=,  dejs:format
|_  act=sync-hook-action
++  grab
  |%
  ++  noun  sync-hook-action
  ++  json
    |=  jon=^json
    =<  (parse-sync-hook-action jon)
    |%
    ++  parse-sync-hook-action
      %-  of
      :~
        [%add add-action]
        [%remove remove-action]
      ==
    ::
    ++  add-action
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%path (su ;~(pfix net (more net urs:ab)))]
      ==
    ::
    ++  remove-action
      (su ;~(pfix net (more net urs:ab)))
    --
  --
--
