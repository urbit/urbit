/-  *permission-group-hook
/+  *group-json
=,  dejs:format
|_  act=permission-group-hook-action
++  grab
  |%
  ++  noun  permission-group-hook-action
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
