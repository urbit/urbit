::  Describe available comands: +help, +help %tree, +help /hood
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
  ?~  c  ["{<b>}" "~"]
  =/  ct  (trip i.c)
  :-  ?:  =(~ (find "|" ct))
        ?-  b
          ~          ~
          [%hood ^]  "|{(path2tape t.b)}"
          [%gen %hood ^]  "|{(path2tape t.t.b)}"
          ^          "+{(path2tape b)}"
        ==
      ?:  ?=([%hood ^] b)  "|{(path2tape t.b)}"
      ?:  ?=([%gen %hood ^] b)  "|{(path2tape t.t.b)}"
      ::  :spider|kill style directed generators must be named in the first line
      ::  trim out the /app command
      ::  TODO trim out the /hoon suffix
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
  |=  [nam=$@(@t p=path) ark=arch]  ^-  (unit [@t path])
  ?.  (~(has by dir.ark) %hoon)  ~
  %+  bind  (file:space:userlib ;;(path ?@(nam (welp pax /[nam]/hoon) :(welp pax nam /hoon))))
  |=  a=*  ^-  [cord path]
  [;;(@t a) ;;(path (welp (slag len pax) ;;(path ?@(nam /[nam] nam))))]
::  Locate a file within a path (if it exists).
++  search-down
  |=  [tgt=@tas pax=path]
  ^-  (unit (list path))
  =/  dir  .^((list path) %ct pax)
  =/  res
    %+  skim  dir
    |=  [a=path]
    !=(~ (find ~[tgt] a))
  ?~(res ~ `res)
::
--
::
::TODO: make this work with doccords
:-  %say
|=  [[now=time @ our=ship ^] typ=$@(~ ?([p=term ~] [p=path ~])) ~]
:: |=  [[now=time @ our=ship ^] typ=$@(~ [p=term ~]) ~]
=/  pax=path  /(scot %p our)/base/(scot %da now)/gen  :: XX hardcoded
=/  pat=path  /(scot %p our)/base/(scot %da now)/ted  :: XX hardcoded
=+  len=(lent pax)
::  Three cases:
::  1. A single search term is provided.  Show matches.
::  2. A path is provided.  Show nested.
::  3. No search term or path is provided.  Show all.
::
::  Branch 1, Single search term, show matches.
?:  ?=([p=@ ~] typ)
  :-  %tang  %-  flop  ^-  tang
  =/  out
    %+  welp
      =/  res  (search-down p.typ pax)
      ?~  res  ~
      ::  strip /gen from head
      =/  reg  (turn u.res |=(pax=path ?~(pax pax t.pax)))
      %-  sort  :_  aor  ^-  tang
      =|  out=tang
      |-  ^-  tang
      ?~  reg  out
      ::  Search in /gen
      =/  gen  i.reg
      ::  Trim off trailing /hoon
      =?  gen  !=(~ gen)  (snip `path`gen)
      =+  ark=.^(arch cy+(welp pax gen))
      %=  $
          reg
        t.reg
        ::
          out
        %+  welp
          =/  red  ((read-at len pax) gen ark) :: XX ugly
          (drop (bind red rend))
        out
      ==
    =/  res  (search-down p.typ pat)
    ?~  res  ~
    ::  strip /ted from head
    =/  reg  (turn u.res |=(pax=path ?~(pax pax t.pax)))
    %-  sort  :_  aor  ^-  tang
    =|  out=tang
    |-  ^-  tang
    ?~  reg  out
    ::  Search in /ted
    =/  gen  i.reg
    ::  Trim off trailing /hoon
    =?  gen  !=(~ gen)  (snip `path`gen)
    =+  ark=.^(arch cy+(welp pat gen))
    %=  $
        reg
      t.reg
      ::
        out
      %+  welp
        =/  red  ((read-at len pat) gen ark) :: XX ugly
        (drop (bind red rent))
      out
    ==
  ?~(out `tang`~[[%leaf "{<`@tas`p.typ>} not found"]] out)
::  Branch 3, No search term or path so grab all
?~  typ
  :-  %tang  %-  flop  ^-  tang
  %+  welp
    ::  Search in /gen
    =+  ark=.^(arch cy+pax)
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
  ::  Search in /ted
  =+  art=.^(arch cy+pat)
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
::  Branch 2, Path is provided, search under path
?>  ?=([^ ~] typ)
=.  pax  (welp pax p.typ)
=.  pat  (welp pat p.typ)
:-  %tang  %-  flop  ^-  tang
=/  out
  %+  welp
    ::  Search in /gen
    =+  ark=.^(arch cy+pax)
    %+  welp
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
  ::  Search in /ted
  =+  art=.^(arch cy+pat)
  %+  welp
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
?~(out `tang`~[[%leaf "{<`path`p.typ>} not found"]] out)
