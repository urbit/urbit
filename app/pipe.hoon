/+  talk
!:
=>  |%
    ++  move  (pair bone card)
    ++  card
      $%  {$peel wire dock mark path}
          {$poke wire dock $talk-command command:talk}
      ==
    --
::
|_  {hid/bowl connections/(set {app/term source/path station/knot})}
++  poke-noun
  |=  arg/*
  ^-  {(list move) _+>.$}
  ?:  ?=($list arg)
    %-  %-  slog
        %+  turn  (~(tap in connections))
        |=  {app/term source/path station/knot}
        leaf+"{(trip app)}{<`path`source>} ---> {(trip station)}"
    [~ +>.$]
  =+  ((soft {$cancel app/term source/path station/knot}) arg)
  ?^  -
    ?.  (~(has in connections) [app.u source.u station.u])
      %-  %-  slog  :~
            leaf+"no connection:"
            leaf+"{(trip app.u)}{<`path`source.u>} ---> {(trip station.u)}"
          ==
      [~ +>.$]
    %-  %-  slog  :~
          leaf+"canceling:"
          leaf+"{(trip app.u)}{<`path`source.u>} ---> {(trip station.u)}"
        ==
    [~ +>.$(connections (~(del in connections) [app.u source.u station.u]))]
  =+  ((hard {app/term source/path station/knot}) arg)
  (poke-pipe-arg app source station)
::
++  poke-pipe-arg
  |=  {app/term source/path station/knot}
  ^-  {(list move) _+>.$}
  :_  +>.$(connections (~(put in connections) [app source station]))
  :_  ~
  ~&  [%peeling app source station]
  :*  ost.hid  %peel  [%subscribe app station source]
      [our.hid app]  %talk-speeches  source
  ==
::
++  diff-talk-speeches
  |=  {way/wire speeches/(list speech:talk)}
  ^-  {(list move) _+>.$}
  ?>  ?=({$subscribe @ @ *} way)
  =+  app=(slav %tas i.t.way)
  =+  station=i.t.t.way
  =+  source=t.t.t.way
  ?.  (~(has in connections) [app source station])
    %-  %-  slog  :~
          leaf+"pipe dropping:"
          leaf+"{(trip app)}{<`path`source>} ---> {(trip station)}"
        ==
    [~ +>.$]
  :_  +>.$  :_  ~
  :*  ost.hid  %poke  [%relay app station source]
      [our.hid %talk]  %talk-command
      %publish
      |-  ^-  (list thought:talk)
      ?~  speeches
        ~
      :_  $(speeches t.speeches, eny.hid (shax (cat 3 %pipe eny.hid)))
      :*  `@uvH`(end (sub 'H' 'A') 1 eny.hid)
          [[[%& our.hid station] *envelope:talk %pending] ~ ~]
          now.hid  *(set flavor:talk)  i.speeches
      ==
  ==
::
++  coup-relay
  |=  {way/wire saw/(unit tang)}
  ^-  {(list move) _+>.$}
  ?>  ?=({@ @ @ *} way)
  ?~  saw
    [~ +>.$]
  %-  (slog leaf+"pipe relay failure in:" >way< u.saw)
  [~ +>.$]
--
