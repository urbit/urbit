::
::::  /hoon/stat-all/womb/mar
  ::
/?    310
/+    hood-womb
::
::::  ~fyr
  ::
|_  balance:hood-womb
::
++  grab                                                     ::  convert from
  |%
  ++  noun  balance:hood-womb                                ::  clam from %noun
  --
++  grow
  |%
  ++  json
    %-  pairs:enjs:format  :~
      owner+[%s owner]
      stars+(numb:enjs:format stars)
      planets+(numb:enjs:format planets)
      history+[%a (turn history |=(a/knot s+a))]
    ==
  --
--
