::
::::  /hoon/stat-all/womb/mar
  ::
/?    310
/+    womb
::
::::  ~fyr
  ::
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
    %+  joba  -.b
    ?-  -.b
      $free  b+&
      $owned  s+p.b
      $split  json(all p.b)
    ==
  --
--
