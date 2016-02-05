::
::::  /hoon/subdir/ls/gen
  ::
|%
++  subdir
  |=  [pax=path des=(map ,@t ,~)]
  ^-  tank
  :+  %rose  [" " `~]
  %+  turn  (sort (~(tap by des)) aor)
  |=  [kid=@ta ~]
  =+  paf=`path`/[kid]
  =-  :+  %rose  ["/" ~ ?:(dir "/" ~)]
      (turn paf |=(a=span leaf/(trip a)))
  |-  ^-  [dir=? paf=path]
  =+  arf=;;(arch .^(%cy (weld pax paf)))
  ?^  fil.arf  
    [| paf]
  ?~  dir.arf
    [& paf]  ::  !!
  ?.  ?=([^ ~ ~] dir.arf)
    [& paf]
  $(paf (welp paf /[p.n.dir.arf]))
--
