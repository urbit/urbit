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
      :~  [%path pa]
          [%kind ki]
          [%who (as pa)]
      ==
    ::
    ++  delete
      (ot [%path pa]~)
    ::
    ++  add
      %-  ot
      :~  [%path pa]
          [%who (as (su ;~(pfix sig fed:ag)))]
      ==
    ::
    ++  remove
      %-  ot
      :~  [%path pa]
          [%who (as (su ;~(pfix sig fed:ag)))]
      ==
    ::
    ++  allow
      %-  ot
      :~  [%path pa]
          [%who (as (su ;~(pfix sig fed:ag)))]
      ==
    ::
    ++  deny
      %-  ot
      :~  [%path pa]
          [%who (as (su ;~(pfix sig fed:ag)))]
      ==
    ::
    --
  --
--
