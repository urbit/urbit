/-  *chat-hook
=,  dejs:format
|_  act=chat-hook-action
++  grab
  |%
  ++  noun  chat-hook-action
  ++  json
    |=  jon=^json
    =<  (parse-chat-hook-action jon)
    |%
    ++  parse-chat-hook-action
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
      ^-  $-(json chat-security)
      (su (perk %channel %village %journal %mailbox ~))
    ::
    --
  --
--

