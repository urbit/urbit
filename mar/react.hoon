::
::::  /hoon/core/react/mar
  ::
/?  314
/+  react
!:
::::
  ::
[. react]
|_  own+manx
::
++  grow                                                ::  convert to
  |%
  ++  tape  (pojo react-json)
  ++  react-js  (crip (react-to-tape own))
::   ++  js  react-js                                   ::  convert to %js
  ++  react-json  (react-to-json own)
  ++  mime  [/application/json (tact tape)] ::  convert to %mime
  --
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
          ++  elem  |=  a+manx  a
--        --
