::
::::  /hoon/stat-all/womb/mar
  ::
/?    310
/+    hood-womb
::
::::  ~fyr
  ::
|_  all/(map ship stat:hood-womb)
::
++  grab                                                ::  convert from
  |%
  ++  noun  (map ship stat:hood-womb)                        ::  clam from %noun
  --
++  grow
  |%
  ++  json
    %-  pairs:enjs:format
    %+  turn  ~(tap by all)
    |=  {a/ship b/stat:hood-womb}  ^-  {cord ^json}
    :-  (crip +:<a>)
    (pairs:enjs:format live+[%s p.b] dist+(json-dist q.b) ~)
  ::
  ++  json-dist
    |=  a/dist:hood-womb
    %+  frond:enjs:format  -.a
    ?-  -.a
      $free  b+&
      $owned  s+p.a
      $split  json(all p.a)
    ==
  --
--
