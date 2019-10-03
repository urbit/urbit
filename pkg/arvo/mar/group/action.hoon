/+  *group-json
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
        [%bundle pa]
        [%unbundle pa]
      ==
    ::
    ++  add-action
      %-  ot
      :~  [%members (as (su ;~(pfix sig fed:ag)))]
          [%path pa]
      ==
    ::
    ++  remove-action
      %-  ot
      :~  [%members (as (su ;~(pfix sig fed:ag)))]
          [%path pa]
      ==
    --
  --
--
