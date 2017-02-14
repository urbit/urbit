::
::::  /hoon/stat-all/womb/mar
  ::
/?    310
/+    womb, old-zuse
::
::::  ~fyr
  ::
=,  old-zuse
|_  all/(map ship stat:womb)
::
++  grab                                                ::  convert from
  |%
  ++  noun  (map ship stat:womb)                        ::  clam from %noun
  --
++  grow
  |%
  ++  json
    %-  jobe
    %+  turn  (~(tap by all))
    |=  {a/ship b/stat:womb}  ^-  {cord ^json}
    :-  (crip +:<a>)
    (jobe live+[%s p.b] dist+(json-dist q.b) ~)
  ::
  ++  json-dist
    |=  a/dist:womb
    %+  joba  -.a
    ?-  -.a
      $free  b+&
      $owned  s+p.a
      $split  json(all p.a)
    ==
  --
--
