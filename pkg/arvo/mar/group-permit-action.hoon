/-  *group-permit
/+  *group-json
=,  dejs:format
|_  act=group-permit-action
++  grab
  |%
  ++  noun  group-permit-action
  ++  json  
    |=  jon=^json
    =<  (parse-action jon)
    |%
    ++  parse-action
      %-  of
      :~  [%associate associate]
          [%dissociate dissociate]
      ==
    ::
    ++  associate
      %-  ot
      :~  [%group (su ;~(pfix net (more net urs:ab)))]
          [%permissions (as (su ;~(pfix net (more net urs:ab))))]
      ==
    ::
    ++  dissociate
      %-  ot
      :~  [%group (su ;~(pfix net (more net urs:ab)))]
          [%permissions (as (su ;~(pfix net (more net urs:ab))))]
      ==
    ::
    --
  --
--
