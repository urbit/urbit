/-  hall, tiebout
=,  format
::
|_  act=action:tiebout
::
++  grow
  |%
  ++  tank  >act<
  --
::
++  grab
  |%
  ++  noun  action:tiebout
  ++  json
    |=  jon=^json
    ^-  action:tiebout
    =<  (action jon)
    |%
    ++  action
      %-  of:dejs
        :~  token+so:dejs
            add-circle+sa
            del-circle+sa
        ==
    ++  sa            :: string as ta
      |=  jon=^json
      ?>  ?=([%s *] jon)
      (scot %tas p.jon)
    --
  --
::
--

