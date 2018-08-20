::  Describe available comands: +help, +help %tree, +help %hood
::
::::  /hoon/help/gen
  ::
/?    310
::
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
        ~  "/"  :: XX !! maybe?
        {$hood ^}  "|{(path-heps t.b)}"
        ^          "+{(path-heps b)}"  :: XX deal with :hall|foo
      ==
  =/  c  (to-wain:format a)
  ?~  c  "~"
  ?.  =('::  ' (end 3 4 i.c))
    "<undocumented>"
  (trip i.c)
::
++  read-at
  |=  {len/@u pax/path}
  |=  {nam/@t ark/arch}  ^-  (unit {@t path})
  ?.  (~(has by dir.ark) %hoon)  ~
  %+  bind  (file:space:userlib (welp pax /[nam]/hoon))
  |=  a/*  ^-  {cord path}
  [;;(@t a) (welp (slag len pax) /[nam])]
--
::
:-  %say
|=  {{now/time @ our/ship ^} typ/$@(~ {p/term ~}) ~}
=/  pax/path  /(scot %p our)/home/(scot %da now)/gen  :: XX hardcoded
=+  len=(lent pax)
=.  pax  ?~(typ pax (welp pax /[p.typ]))
:-  %tang  %-  flop  ^-  tang
=+  ark=.^(arch cy+pax)
%+  welp
  ?~  typ  ~
  =/  red  ((read-at len (scag len pax)) p.typ ark) :: XX ugly
  (drop (bind red rend))
|-  ^-  tang
=+  =<  arl=~(tap by (~(urn by dir.ark) .))
    |=({a/@t ~} .^(arch cy+(welp pax /[a])))
%+  welp
  =/  dir/(list {@ path})
    (murn arl (read-at len pax))
  `tang`(turn (sort dir aor) rend)
%-  zing  ^-  (list tang)
%+  turn  (sort arl aor)
|=  {a/@t b/arch}
^$(pax (welp pax /[a]), ark b)
