/-  *inbox-hook
=,  dejs:format
|_  act=inbox-hook-action
++  grab
  |%
  ++  noun  inbox-hook-action
  ++  json
    |=  jon=^json
    =<  (parse-inbox-hook-action jon)
    |%
    ++  parse-inbox-hook-action
      %-  of
      :~
        [%add-owned add-owned]
        [%add-synced add-synced]
        [%remove remove]
      ==
    ::
    ++  add-owned
      %-  ot
      :~  [%path (su ;~(pfix net (more net urs:ab)))]
          [%security sec]
      ==
    ::
    ++  add-synced
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%path (su ;~(pfix net (more net urs:ab)))]
      ==
    ::
    ++  remove
      (su ;~(pfix net (more net urs:ab)))
    ::
    ++  sec
      =,  dejs:format
      ^-  $-(json inbox-security)
      (su (perk %channel %village %journal %mailbox ~))
    ::
    --
  --
--

