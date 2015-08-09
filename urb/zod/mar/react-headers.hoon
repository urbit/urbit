::
::::  /hoon/core/react-headers/mar
  ::
/?  314
/+  react
!:
::::
  ::
|_  hed=marl
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/application/json (tact tape)]
  ++  tape  (pojo react-headers-json)
  ++  react-headers-json  (react-to-json ;div:"*{hed}")
  --
++  grab  |%                                            ::  convert from
          ++  noun  marl
          ++  headers  |=(a=marl a)
--        --
