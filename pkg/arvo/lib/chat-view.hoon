/-  sur=chat-view, *rw-security
^?
=<  [sur .]
=,  sur
|%
++  dejs
  |%
  ++  action
    |=  jon=json
    ^-  ^action
    =,  dejs:format
    =<  (parse-json jon)
    |%
    ++  parse-json
      %-  of
      :~  [%create create]
          [%delete delete]
          [%join join]
          [%groupify groupify]
      ==
    ::
    ++  create
      %-  ot
      :~  [%title so]
          [%description so]
          [%app-path pa]
          [%group-path pa]
          [%security sec]
          [%members (as (su ;~(pfix sig fed:ag)))]
          [%allow-history bo]
      ==
    ::
    ++  delete
      (ot [%app-path pa]~)
    ::
    ++  join
      %-  ot
      :~  [%ship (su ;~(pfix sig fed:ag))]
          [%app-path pa]
          [%ask-history bo]
      ==
    ::
    ++  groupify
      =-  (ot [%app-path pa] [%existing -] ~)
      (mu (ot [%group-path pa] [%inclusive bo] ~))
    ::
    ++  sec
      =,  dejs:format
      ^-  $-(json rw-security)
      (su (perk %channel %village %journal %mailbox ~))
    --
  --
--
