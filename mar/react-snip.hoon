::
::::  /hoon/core/react-snip/mar
  ::
/?  314
/+  react
!:
::::
  ::
[. react]
|_  {hed+marl tal+marl}
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/application/json (tact tape)]
  ++  tape  (pojo react-snips-json)
  ++  elem  ;div:(h1:"*{hed}" div:"*{tal}")
  ++  react-snip-js  (crip (react-to-tape elem))
  ++  react-snips-json
    ::?>  ?=([[%div ~] [[%h1 ~] *] [[%div ~] *] ~]] own)  :: xx mystery fish-loop
    %^  jobe
      head#react-head-json
      body#react-snip-json
    ~
  ::
  ++  react-head-json  (react-to-json ;h1:"*{hed}")
  ++  react-snip-json  (react-to-json ;div:"*{tal}")
  --
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
          ++  snip  |=(a+{marl marl} a)
--        --
