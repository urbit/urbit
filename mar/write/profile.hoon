::
::::  /hoon/profile/write/mar
  ::
/?  310
/-  profile, hall
::
=,  dejs:format
::
|_  all=profile:profile
++  grab
  |%
  ++  noun  profile:profile
  ++  json
    |^
    |=  jon=^json
    ^-  profile:profile
    =/  out  %.  jon
    %-  ot
    :~  nickname+so
        location+so
        streams+(ar parse-circle)
        collections+(ar parse-circle)
    ==
    %=  out
      +>-  (silt +>-.out)
      +>+  (silt +>+.out)
    ==
    ::
    ++  parse-circle
      |=  jon=^json
      ^-  circle:hall
      %.  jon
      %-  ot
      :~  hos+(su fed:ag)
          nom+so
      ==
    --
  --
--
