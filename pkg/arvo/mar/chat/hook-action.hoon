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
          [%allow-history bo]
      ==
    ::
    ++  add-synced
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%path pa]
          [%ask-history bo]
      ==
    ::
    ++  sec
      ^-  $-(^json rw-security)
      (su (perk %channel %village %journal %mailbox ~))
    ::
    --
  --
--

