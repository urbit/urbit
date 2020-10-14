/-  sur=chat-view, *rw-security
/+  group-store
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
          [%invite invite]
      ==
    ::
    ++  create
      %-  ot
      :~  [%title so]
          [%description so]
          [%app-path pa]
          [%group-path pa]
          [%policy policy:dejs:group-store]
          [%members (as (su ;~(pfix sig fed:ag)))]
          [%allow-history bo]
          [%managed bo]
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
    ++  invite
      %-  ot
      :~  app-path+pa
          ships+(as (su ;~(pfix sig fed:ag)))
      ==
    --
  --
--
