/-  *groups
|_  act=group-action
++  grab
  |%
  ++  noun  group-action
  ++  json
    |=  jon=^json
    =<  (parse-group-action jon)
    |%
    ++  parse-group-action
      =,  dejs:format
      %-  of
      :~
        [%add add-action]
        [%remove remove-action]
        [%bundle bundle-action]
        [%unbundle unbundle-action]
      ==
    ::
    ++  add-action
      %-  ot
      :~  [%members (ar hall-action)]
          [%path+(su ;~(pfix net (more net urs:ab)))]
      ==
    ::
    ++  remove-action
      %-  ot
      :~  [%members (ar hall-action)]
          [%path (su ;~(pfix net (more net urs:ab)))]
      ==
    ::
    ++  bundle-action
      (ot path+(su ;~(pfix net (more net urs:ab))) ~)
    ::
    ++  unbundle-action
      (ot path+(su ;~(pfix net (more net urs:ab))) ~)
    ::
    --
  --
--
