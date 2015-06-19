::
::::  /hoon/core/react/mar
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
  ++  tape  (react-to-tape own)
  ++  react-js  (crip tape)
::   ++  js  react-js                                      ::  convert to %js
  ++  mime  [/text/javascript (taco react-js)]             ::  convert to %mime
  --
++  grab  |%                                            ::  convert from
          ++  noun  manx                                ::  clam from %noun
          ++  elem  |=  a=manx  a
--        --
