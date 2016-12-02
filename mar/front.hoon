::
::::  /hoon/front/mar
  ::
/?    310
::
::::
  ::
|_  all/(map knot cord)
::
++  grow                                                ::  convert to
  |%
  ++  json
    :-  %o
    %-  ~(run by all)
    |=(a/cord s+a)
  --
++  grab  |%                                            ::  convert from
          ++  noun  (map knot cord)                     ::  clam from %noun
          :: ++  elem  ,~
--        --
