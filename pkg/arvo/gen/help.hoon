::  Describe available comands: +help, +help %tree, +help /hood
::
::
::
::  The first line of the generator is expected to be a comment
::  describing the expected behavior.
::
::  If a generator should be used with an agent, the description
::  line should be prefixed with the call, e.g.:
::    :spider|kill  Terminate a running thread
::  (This does not apply to Hood commands, which should be
::  treated normally.)
|%
++  path2tape
  |=  a=path  ^-  tape
  ?~  a  ""
  |-  ^-  tape
  %+  welp  (trip i.a)
  ?~  t.a  ""
  ['/' $(a t.a)]
::  render generator
++  rend
  |=  [a=@thoon b=path]  ^-  tank
  =;  c=(pair tape tape)
    =/  tab  (sub 10 (mod (lent "{p.c}  ") 10))
    [%palm ["  {(reap tab ' ')}" ``~] leaf+p.c leaf+q.c ~]
  =/  c  (to-wain:format a)
  ?~  c  !!
  =/  ct  (trip i.c)
  :-  ?:  =(~ (find "|" ct))
        ?-  b
          ~          ~
          [%hood ^]  "|{(path2tape t.b)}"
          ^          "+{(path2tape b)}"
        ==
      ?:  ?=([%hood ^] b)  "|{(path2tape t.b)}"
      ::  :spider|kill style directed generators must be named in the first line
      ::  trim out the /app command
      =/  beg  (find "  " ct)
      ?~  beg  ~
      =/  bg=@  (add 2 u.beg)
      =/  off  (find " " (slag bg ct))
      =/  of=@
        ?~  off  (lent (slag bg ct))
        u.off
      (slag bg (scag :(add bg of 1) ct))
  ?.  |(=('::  ' (end [3 4] i.c)) =('  ::  ' (end [3 4] i.c)))
    "<undocumented>"
  =?    ct
      !=(~ (find "|" ct))
    =/  beg  (find "  " ct)
    ?~  beg  ~
    =/  bg=@  (add 2 u.beg)
    =/  off  (find " " (slag bg ct))
    =/  of=@
      ?~  off  (lent (slag bg ct))
      u.off
    (welp ":: " (slag :(add bg of 1) ct))
  ct
::  render thread
++  rent
  |=  [a=@thoon b=path]  ^-  tank
  =;  c=(pair tape tape)
    =/  tab  (sub 10 (mod (lent "{p.c}  ") 10))
    [%palm ["  {(reap tab ' ')}" ``~] leaf+p.c leaf+q.c ~]
  :-  ?-  b
        ~  ~
        ^  "-{(path2tape b)}"
      ==
  =/  c  (to-wain:format a)
  ?~  c  "~"
  ?.  |(=('::  ' (end [3 4] i.c)) =('  ::  ' (end [3 4] i.c)))
    "<undocumented>"
  (trip i.c)
::
++  read-at
  |=  [len=@u pax=path]
  |=  [nam=@t ark=arch]  ^-  (unit [@t path])
  ?.  (~(has by dir.ark) %hoon)  ~
  %+  bind  (file:space:userlib (welp pax /[nam]/hoon))
  |=  a=*  ^-  [cord path]
  [;;(@t a) (welp (slag len pax) /[nam])]
::
--
::
::TODO: make this work with doccords
:-  %say
::|=  [[now=time @ our=ship ^] typ=$@(~ p=path) ~]
|=  [[now=time @ our=ship ^] typ=$@(~ [p=term ~]) ~]
=/  pax=path  /(scot %p our)/base/(scot %da now)/gen  :: XX hardcoded
=/  pat=path  /(scot %p our)/base/(scot %da now)/ted  :: XX hardcoded
=+  len=(lent pax)
=.  pax  ?~(typ pax (welp pax /[p.typ]))
=.  pat  ?~(typ pat (welp pat /[p.typ]))
:-  %tang  %-  flop  ^-  tang
%+  welp
  =+  ark=.^(arch cy+pax)
  %+  welp
    ?~  typ  ~
    =/  red  ((read-at len (scag len pax)) p.typ ark) :: XX ugly
    (drop (bind red rend))
  |-  ^-  tang
  =+  =<  arl=~(tap by (~(urn by dir.ark) .))
      |=([a=@t ~] .^(arch cy+(welp pax /[a])))
  %+  welp
    =/  dir=(list [@ path])
      (murn arl (read-at len pax))
    `tang`(turn (sort dir aor) rend)
  %-  zing  ^-  (list tang)
  %+  turn  (sort arl aor)
  |=  [a=@t b=arch]
  ^$(pax (welp pax /[a]), ark b)
=+  art=.^(arch cy+pat)
%+  welp
  ?~  typ  ~
  =/  ret  ((read-at len (scag len pat)) p.typ art) :: XX ugly
  (drop (bind ret rent))
|-  ^-  tang
=+  =<  arl=~(tap by (~(urn by dir.art) .))
    |=([a=@t ~] .^(arch cy+(welp pat /[a])))
%+  welp
  =/  dir=(list [@ path])
    (murn arl (read-at len pat))
  `tang`(turn (sort dir aor) rent)
%-  zing  ^-  (list tang)
%+  turn  (sort arl aor)
|=  [a=@t b=arch]
^$(pat (welp pat /[a]), art b)
