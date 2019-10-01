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
        [%remove pa]
      ==
    ::
    ++  add-owned
      %-  ot
      :~  [%path pa]
          [%security sec] 
      ==
    ::
    ++  add-synced
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%path pa]
      ==
    ::
    ++  sec
      ^-  $-(^json chat-security)
      (su (perk %channel %village %journal %mailbox ~))
    ::
    --
  --
--

