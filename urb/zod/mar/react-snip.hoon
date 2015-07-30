::
::::  /hoon/core/react-snip/mar
  ::
/?  314
/+  react
!:
::::
  ::
|_  [hed=marl tal=marl]
::
++  grow                                                ::  convert to
  |%
  ++  tape  (pojo react-snip-json)
  ++  elem  ;div:(h1:"*{hed}" div:"*{tal}")
  ++  react-snip-js  (crip (react-to-tape elem))
  ++  react-snips-json
    ::?>  ?=([[%div ~] [[%h1 ~] *] [[%div ~] *] ~]] own)  :: xx mystery fish-loop
    %^  jobe
      head/(react-to-json ;h1:"*{hed}")
      body/(react-to-json ;div:"*{tal}")
    ~
::   ++  js  react-snip-js
  ++  react-snip-json  (react-to-json elem)
  ++  mime  [/application/json (tact tape)]
  --
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
          ++  snip  |=(a=[marl marl] a)
--        --
