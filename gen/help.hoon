::  List available comands
::  
::::  /hoon/help/gen
  ::
/?    310
!:
::::
  ::
|%
++  path-heps
  |=  a/path  ^-  tape
  ?~  a  ""
  |-  ^-  tape
  %+  welp  (trip i.a)
  ?~  t.a  ""
  ['-' $(a t.a)]
::
++  rend
  |=  {a/@thoon b/path}  ^-  tank
  =;  c/(pair tape tape)
    =/  tab  (sub 10 (mod (lent "{p.c}  ") 10))
    [%palm ["  {(reap tab ' ')}" ``~] leaf+p.c leaf+q.c ~]
  :-  ?-  b
        $~  "/"  :: XX !! maybe?
        {$hood ^}  "|{(path-heps t.b)}"
        ^          "+{(path-heps b)}"  :: XX deal with :talk|foo
      ==
  =/  c  (lore a)
  ?~  c  "~"
  ?.  =('::  ' (end 3 4 i.c))
    "<undocumented>"
  (trip i.c)
--
::
:-  %say
|=  {{now/time @ our/ship ^} $~ $~}
=/  pax/path  /(scot %p our)/home/(scot %da now)/gen  :: XX hardcoded
=+  len=(lent pax)
:-  %tang  %-  flop
=+  ark=.^(arch cy+pax)
|-  ^-  tang
=+  =<  arl=(~(tap by (~(urn by dir.ark) .)))
    |=({a/@t $~} .^(arch cy+(welp pax /[a])))
=-  %+  welp  -
    %-  zing  ^-  (list tang)
    %+  turn  (sort arl aor)
    |=  {a/@t b/arch}
    ^$(pax (welp pax /[a]), ark b)
=;  res/(list {cord path})
  (turn (sort res aor) rend)
%+  murn  arl
|=  {a/@t b/arch}
?.  (~(has by dir.b) %hoon)  ~
%+  bind  (file (welp pax /[a]/hoon))
|=  c/*  ^-  {cord path}
[;;(@t c) (welp (slag len pax) /[a])]
