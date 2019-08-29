/-  *groups
/+  inbox-json
=,  dejs:format

|_  act=group-action
++  grab
  |%
  ++  noun  group-action
  ++  json
    |=  jon=^json
    =<  (parse-group-action jon)
    |%
    ++  parse-group-action
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
      :~  [%members (as:inbox-json (su ;~(pfix sig fed:ag)))]
          [%path (su ;~(pfix net (more net urs:ab)))]
      ==
    ::
    ++  remove-action
      %-  ot
      :~  [%members (as:inbox-json (su ;~(pfix sig fed:ag)))]
          [%path (su ;~(pfix net (more net urs:ab)))]
      ==
    ::
    ++  bundle-action
      (su ;~(pfix net (more net urs:ab)))
    ::
    ++  unbundle-action
      (su ;~(pfix net (more net urs:ab)))
    ::
    --
  --
--
