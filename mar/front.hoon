::
::::  /hoon/front/mar
  ::
/?    314
!:
::::
  ::
|_  all=(map span cord)
::
++  grow                                                         ::  convert to
  |%
  ++  json
    :-  %o
    %-  ~(run by all)
    |=(a=cord s/a)
  --
++  grab  |%                                                     ::  convert from
          ++  noun  (map span cord)                              ::  clam from %noun
          :: ++  elem  ,~
--        --
