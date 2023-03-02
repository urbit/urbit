::  minesweeper: it's minesweeper
::
/+  *etui, dbug, verb, default-agent
::
|%
+$  state-0
  $:  %0
      watch=@ta
      scope=size
      alive=?
      times=$@(@da [since=@da until=@da])
      clean=@ud
      mines=@ud
      flags=@ud
      field=(list @)
    ::TODO  need to know when we've won. mb count "clean" tiles during setup?
  ==
::
+$  card  card:agent:gall
::
++  start
  |=  [eny=@ siz=size hit=spot now=@da]
  ^-  state-0
  =;  [f=(list @) m=@ud]
    (click [%0 ~. siz & now (sub (mul h.siz w.siz) m) m 0 f] [| hit] now)
  %^  spin  (gulf 1 h.siz)  0
  |=  [y=@ud m=@ud]
  =;  [r=(list @) m=@ud]
    :-  (rep size:ti r)
    (add ^m m)
  %^  spin  (gulf 1 w.siz)  0
  |=  [x=@ud m=@ud]
  ?:  =([x y] hit)  [easy:ti m]
  ?:  (gth (~(rad og (mug x y eny)) 100) 10)
    [easy:ti m]
  [mine:ti +(m)]
::
++  click
  |=  [state=state-0 [alt=? hit=spot] now=@da]
  ^+  state
  ~&  [%click alt hit]
  ?.  alive.state
    ~&  %revive
    state(alive &, field ~)
  =/  t=@  (~(get fi field.state) hit)
  ?:  (is-open:ti t)
    ?:  alt  state
    ::TODO  open all surrounding non-flagged tiles
    =^  new=@ud  field.state  (open hit [scope field]:state)
    (claim state(clean (sub clean.state new)))
  ?:  alt
    =.  flags.state
      ?:((is-flag:ti t) (dec flags.state) +(flags.state))
    state(field (~(put fi field.state) hit (do-flag:ti t)))
  ?:  (is-flag:ti t)
    state
  ?:  (is-mine:ti t)
    ?>  ?=(@ times.state)  ::TODO  somewhat unsafe
    state(alive |, times [times.state now], field +:(open hit [scope field]:state))
  =^  new=@ud  field.state  (open hit [scope field]:state)
  (claim state(clean (sub clean.state new)))
::
++  claim  same
::
++  open
  |=  [spot size field=(list @)]
  =|  new=@ud
  ^+  [new field]
  =/  t=@  (~(get fi field) x y)
  ?:  (is-open:ti t)
    [0 field]
  =.  field  (~(put fi field) [x y] (do-open:ti t))
  =.  new    +(new)
  =/  v=@ud  (value [x y] [w h] field)
  ?:  !=(0 v)  [new field]
  |-
  =/  cx  ?:(=(0 x) 1 0)
  =/  dx  ?:(=((dec w) x) 1 2)
  |-
  ?:  (gth cx dx)  [new field]
  =/  cy  ?:(=(0 y) 1 0)
  =/  dy  ?:(=((dec h) y) 1 2)
  |-
  ?:  (gth cy dy)  ^$(cx +(cx))
  =^  new=@ud  field
    ?:  =([1 1] [cx cy])  [new field]
    =.  x  ?:(=(0 cx) (dec x) (add x (dec cx)))
    =.  y  ?:(=(0 cy) (dec y) (add y (dec cy)))
    =/  t=@  (~(get fi field) x y)
    ?:  (is-open:ti t)  ::TODO  could also do set cache on exact coord?
      [new field]
    =.  field  (~(put fi field) [x y] (do-open:ti t))
    =.  new  +(new)
    =/  v=@ud  (value [x y] [w h] field)
    ?:  !=(0 v)  [new field]
    ^^$(x x, y y, new new, field field)
  $(cy +(cy), new new)
::
++  value
  ::TODO  what if coordinates were 1-based instead? safer, less edge checks...
  |=  [spot size field=(list @)]  ~+
  ^-  @ud
  =/  cx  ?:(=(0 x) 1 0)
  =/  dx  ?:(=((dec w) x) 1 ?:(=(w x) 0 2))
  |-
  ?:  (gth cx dx)  0
  =/  cy  ?:(=(0 y) 1 0)
  =/  dy  ?:(=((dec h) y) 1 ?:(=(h y) 0 2))
  |-
  ?:  (gth cy dy)  ^$(cx +(cx))
  %+  add
    ?:  =([1 1] [cx cy])  0
    =.  x  ?:(=(0 cx) (dec x) (add x (dec cx)))
    =.  y  ?:(=(0 cy) (dec y) (add y (dec cy)))
    ~|  [%get-field scope=[w h] [x y] d=[dx dy]]
    ?:((is-mine:ti (~(get fi field) x y)) 1 0)
  $(cy +(cy))
::
++  paint
  |=  [state-0 now=@da]
  ^-  card
  =;  [* * z=_zo]
    =/  score=(list @)
      =/  diff=@sd  (dif:si (sun:si mines) (sun:si flags))
      =/  lead=@c   ?:((syn:si diff) ~-0 ~--)
      [lead ((d-co:^co 3) (abs:si diff))]
    =/  timer=(list @)
      =/  diff=@dr
        ?@  times  (sub now times)
        (sub [until since]:times)
      ((d-co:^co 4) (div diff ~s1))
    =/  out=blit:dill
      :-  %mor
      =?  w.scope  (lth w.scope 5)  5
      :~  [%hop 1 1]                        [%put score]
          :: [%hop (sub (mul w.scope 2) 5) 1]  [%put timer]
          zo-abet:z
      ==
    [%give %fact [/dill/[watch]]~ %dill-blit !>(out)]
  =?  field  =(~ field)  (reap h.scope (fil size:ti w.scope easy:ti))
  %^  spin  field  [0 (zo-apex:zo [[1 3] [(mul 2 w.scope) h.scope]])]
  |=  [r=@ y=@ud z=_zo]
  =-  [~ +(y) (zo-stub:z [0 y] (pact:klr:format -<))]
  %^  spin  (rip size:ti r)  0
  |=  [t=@ x=@ud]
  :_  +(x)
  ^-  [stye tour]
  =*  bg  [. . .]:0xb9
  =*  plain  [bg [0xd5 0xd9 0xda]]
  ^-  [stye (list @)]
  ?:  &((is-mine:ti t) (is-open:ti t))
    [[~ [0xff . .]:0x77 [. . .]:0x0] (tuba "><")]
  ?:  &(!alive (is-mine:ti t))
    :_  (tuba "><")
    ?:  (is-flag:ti t)
      [~ bg [0x0 0x77 0x0]]
    [~ bg [. . .]:0x0]
  ?:  (is-open:ti t)
    ~|  [%paint x=x y=y scope=scope]
    =/  v  (value [x y] scope field)
    ?:  =(0 v)  [`plain "  "]
    :_  ((d-co:^co 2) v)
    :+  ~  bg
    ?+  v  [0xff 0x0 0x0]
      %1  [0x0 0x0 0xff]
      %2  [0x0 0xff 0x0]
    ==
  ?:  (is-flag:ti t)
    [[[%br ~ ~] [. . .]:0xc9 [0xff . .]:0x55] "!!"]
  [`plain "[]"]
::
:: ++  fi
::   |_  field=(list (list @))
::   ++  get  |=(spot (snag x (snag y field)))
::   ++  try  |=([spot g=$-(@ ?)] ?:((g (get x y)) 1 0))
::   ++  put  |=([spot t=@] (jab [x y] |=(@ t)))
::   ++  jab  |=([spot g=$-(@ @)] (prod field y |=(r=(list @) (prod r x g))))
::   --
::
++  fi
  |_  field=(list @)
  ++  get  |=(spot (cut size:ti [x 1] (snag y field)))
  ++  try  |=([spot g=$-(@ ?)] ?:((g (get x y)) 1 0))
  ++  put  |=([spot t=@] (prod field y |=(r=@ (sew size:ti [x 1 t] r))))
  ++  jab  |=([spot g=$-(@ @)] (prod field y |=(r=@ (cop size:ti [x 1] r g))))
  --
::
:: =/  row  0b100
:: =/  x    width  ::  4
:: |-
:: ?:  =(0 x)  ~
:: =/  t  (end 0^1 row)
:: =.  this  (show x^y t)
:: $(row (rsh 0^1 row))

::
++  ti
  |%
  ++  size  `bloq`2  ::  (bex 2)  =>  4
  ++  easy     0b1
  ++  open    0b10  ::  (and mask number)  =>  >0
  ++  flag   0b100
  ++  mine  0b1000
  ++  is-open  |=(t=@ !=(0 (dis t open)))
  ++  is-flag  |=(t=@ !=(0 (dis t flag)))
  ++  is-mine  |=(t=@ !=(0 (dis t mine)))
  ++  do-open  |=(t=@ (con t open))
  ++  do-flag  |=(t=@ (mix t flag))
  ++  do-mine  |=(t=@ (con t mine))
  --
::
++  cop
  |=  [a=bloq [b=step c=step] d=@ e=$-(@ @)]
  (sew a [b c (e (cut a [b c] d))] d)
::
++  prod
  |*  [a=(list) b=@ud c=gate]
  ?~  a  !!
  ?:  =(0 b)  [(c i.a) t.a]
  [i.a $(a t.a, b (dec b))]
--
::
=|  state-0
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
::
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  [~ this]
++  on-save  !>(state)
::
++  on-load
  |=  ole=vase
  :: =.  state  !<(state-0 ole)
  [[(paint state now.bowl)]~ this]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?.  ?=(%dill-poke mark)  (on-poke:def mark vase)
  =+  !<([ses=@ta belt=dill-belt:dill] vase)
  ~&  [%ses-poke ses belt]
  =.  watch  ses
  ?:  ?=([%hey *] belt)
    [[(paint state now.bowl)]~ this]
  ?:  ?=([%rez *] belt)
    =.  scope  [(div (sub p.belt 2) 2) (sub q.belt 4)]
    =.  field  ~
    [[(paint state now.bowl)]~ this]
  =/  click=(unit [alt=? hit=spot])
    ?+  belt  ~
      [%hit *]            `[| +.belt]
      [%mod %met %hit *]  `[& +.key.belt]
    ==
  =?  click  ?=(^ click)
    =/  loc=(unit spot)
      (~(zo-spot zo [1 3] [(mul 2 w.scope) h.scope]) hit.u.click)
    ?~  loc  ~
    click(hit.u [(div x.u.loc 2) y.u.loc])
  ?~  click  [~ this]
  ?:  =(~ field)
    =.  state  (start eny.bowl scope +.u.click now.bowl)
    =.  watch  ses
    [[(paint state now.bowl)]~ this]
  =.  state  (^click state u.click now.bowl)
  [[(paint state now.bowl)]~ this]
::
++  on-watch
  |=  =path
  ?.  ?=([%dill @ ~] path)  (on-watch:def path)
  =.  watch  i.t.path
  [[(paint state now.bowl)]~ this]
::
++  on-leave
  |=  =path
  ?.  ?=([%dill @ ~] path)  (on-leave:def path)
  [~ this]
::
++  on-agent  on-agent:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
