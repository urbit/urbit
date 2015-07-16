::
::::  /hoon/core/react-snip/mar
  ::
/?  314
/+  react
!:
::::
  ::
|_  own=manx
::
++  grow                                                ::  convert to
  |%
  ++  tape  (pojo react-snip-json)
  ++  react-snip-js  (crip (react-to-tape own))
  ++  react-snips-json
    ::?>  ?=([[%div ~] [[%h1 ~] *] [[%div ~] *] ~]] own)  :: xx mystery fish-loop
    %^  jobe
      head/react-snip-json(own &2.own)
      body/react-snip-json(own &3.own)
    ~
::   ++  js  react-snip-js
  ++  react-snip-json  (react-to-json own)
  ++  mime  [/application/json (tact tape)]
  --
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
          ++  snip  |=  a=manx  a
--        --
