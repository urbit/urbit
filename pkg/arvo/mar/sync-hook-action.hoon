/-  *sync-hook
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
        [%add-owned add-owned]
        [%remove-owned remove-owned]
        [%add-synced add-synced]
        [%remove-synced remove-synced]
      ==
    ::
    ++  add-owned
      (su ;~(pfix net (more net urs:ab)))
    ::
    ++  remove-owned
      (su ;~(pfix net (more net urs:ab)))
    ::
    ++  add-synced
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%path (su ;~(pfix net (more net urs:ab)))]
      ==
    ::
    ++  remove-synced
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%path (su ;~(pfix net (more net urs:ab)))]
      ==
    ::
    --
  --
--
