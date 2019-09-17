/+  *chat-json
=,  dejs:format
|_  act=chat-action
++  grab
  |%
  ++  noun  chat-action
  ++  json  
    |=  jon=^json
    =<  (parse-chat-action jon)
    |%
    ++  parse-chat-action
      %-  of
      :~  [%create create]
          [%delete delete]
          [%message message]
          [%read read]
      ==
    ::
    ++  create
      %-  ot
      :~  [%path (su ;~(pfix net (more net urs:ab)))]
          [%owner (su ;~(pfix sig fed:ag))]
      ==
    ::
    ++  delete
      (ot [%path (su ;~(pfix net (more net urs:ab)))]~)
    ::
    ++  message
      %-  ot
      :~  [%path (su ;~(pfix net (more net urs:ab)))]
          [%envelope envelope]
      ==
    ::
    ++  read
      %-  ot
      :~  [%path (su ;~(pfix net (more net urs:ab)))]
          [%read ni]
      ==
    ::
    ++  envelope
      %-  ot
      :~  [%uid seri]
          [%number ni]
          [%author (su ;~(pfix sig fed:ag))]
          [%when di]
          [%letter letter]
      ==
    ::
    ++  letter
      %-  of
      :~  [%text so]
          [%url so]
          [%code eval]
      ==
    ::
    --
  --
--
