/-  *permission-store
/+  *permission-json
=,  dejs:format
|_  act=permission-action
++  grab
  |%
  ++  noun  permission-action
  ++  json  
    |=  jon=^json
    =<  (parse-permission-action jon)
    |%
    ++  parse-permission-action
      %-  of
      :~  [%create create]
          [%delete delete]
          [%add add]
          [%remove remove]
          [%allow allow]
          [%deny deny]
      ==
    ::
    ++  create
      %-  ot
      :~  [%path (su ;~(pfix net (more net urs:ab)))]
          [%kind ki]
          [%who (as (su ;~(pfix sig fed:ag)))]
      ==
    ::
    ++  delete
      (ot [%path (su ;~(pfix net (more net urs:ab)))]~)
    ::
    ++  add
      %-  ot
      :~  [%path (su ;~(pfix net (more net urs:ab)))]
          [%who (as (su ;~(pfix sig fed:ag)))]
      ==
    ::
    ++  remove
      %-  ot
      :~  [%path (su ;~(pfix net (more net urs:ab)))]
          [%who (as (su ;~(pfix sig fed:ag)))]
      ==
    ::
    ++  allow
      %-  ot
      :~  [%path (su ;~(pfix net (more net urs:ab)))]
          [%who (as (su ;~(pfix sig fed:ag)))]
      ==
    ::
    ++  deny
      %-  ot
      :~  [%path (su ;~(pfix net (more net urs:ab)))]
          [%who (as (su ;~(pfix sig fed:ag)))]
      ==
    ::
    --
  --
--
