/-  sur=graph-view
^?
=<  [sur .]
=,  sur
|%
++  dejs
  =,  dejs:format
  |%
  ++  action
    |=  jon=json
    ^-  ^action
    =<  (parse-json jon)
    |%
    ++  parse-json
      %-  of
      :~  [%fetch fetch]
      ==
    ::
    ++  fetch
      %-  ot
      :~  [%connection ni]
          [%type fetch-type]
      ==
    ::
    ++  fetch-type
      %-  of
      :~  [%all ul]
      ==
    --
  --

--
