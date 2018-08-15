::  Display directory contents
::
::::  /hoon/show-dir/lib
  ::
/?    310
|=  {vane/?($g $c) pax/path des/(map @t ~)}
^-  tank
:+  %rose  [" " `~]
%+  turn  (sort ~(tap by des) aor)
|=  {kid/@ta ~}
=+  paf=`path`/[kid]
=-  :+  %rose  ["/" ~ ?:(dir "/" ~)]
    (turn paf |=(a/knot leaf+(trip a)))
|-  ^-  {dir/? paf/path}
=+  arf=.^(arch (cat 3 vane %y) (weld pax paf))
?^  fil.arf
  [| paf]
?~  dir.arf
  [& paf]  ::  !!
?.  ?=({^ ~ ~} dir.arf)
  [& paf]
$(paf (welp paf /[p.n.dir.arf]))
