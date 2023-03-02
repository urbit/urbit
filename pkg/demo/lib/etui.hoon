::  etui: everyday text ui toolkit
::
|%
::
::  +|  %generics
::
+$  spot  [x=@ud y=@ud]
+$  size  [w=@ud h=@ud]
+$  zone  [spot size]
::
::  +co: coordinate operations
::
++  co
  |%
  ++  map
    |=  f=$-([@ @] @)
    |=  [a=spot b=spot]
    ^-  spot
    [(f x.a x.b) (f y.a y.b)]
  ::
  ++  add  (map ^add)  ::  translate
  ++  sub  (map ^sub)  ::  translate
  ++  min  (map ^min)  ::  bound
  ++  max  (map ^max)  ::  expand
  ::
  ++  dif
    |=  [a=spot b=spot]
    ^-  spot
    :-  ?:((^gte x.a x.b) (^sub x.a x.b) (^sub x.b x.a))
    ?:((^gte y.a y.b) (^sub y.a y.b) (^sub y.b y.a))
  ::
  ++  cmp
    |=  f=$-([@ @] ?)
    |=  [a=size b=size]
    ^-  ?
    &((f w.a w.b) (f h.a h.b))
  ::
  ++  lte  (cmp ^lte)  ::  fits?
  ++  gte  (cmp ^gte)  ::  captures?
  --
::
++  za  ::  zone-to-zone transformations
  |%
  ++  crop  ::  zone inside inner stroke
    |=  zone
    ^-  zone
    [[+(x) +(y)] (sub w 2) (sub h 2)]
  ::
  ++  grow  ::  tightly wrapping zone
    |=  zone
    ^-  zone
    [[(dec x) (dec y)] (add w 2) (add h 2)]
  ::
  ++  join  ::  smallest zone that contains both
    |=  [a=zone b=zone]
    ^-  zone
    =+  o=(min:co -.a -.b)
    :-  o
    =+  aa=(add:co a)
    =+  bb=(add:co b)
    (sub:co (max:co aa bb) o)
  ::
  ++  center  ::  get relative spot to center zone of size b within a
    |=  [a=size b=size]
    ^-  spot  ::TODO  not zone-to-zone
    ?>  (lte:co b a)
    [(div (sub w.a w.b) 2) (div (sub h.a h.b) 2)]
  ::
  ++  side  ::  split in two, pick a side, %u and %l take the remainer
    |=  [z=zone s=?(%u %l %d %r)]
    ^+  z
    ?-  s
      %u  [-.z w.z (add (dvr h.z 2))]
      %l  [-.z (add (dvr w.z 2)) h.z]
      %d  =+  d=(dvr h.z 2)
          [[x.z (add y.z (add d))] w.z p.d]
      %r  =+  d=(dvr w.z 2)
          [[(add x.z (add d)) y.z] p.d h.z]
    ==
  ::
  ::TODO  given the difficulty of working with $lay here,
  ::      maybe we need an alternative system, where we have a
  ::      (list part) with $part being (list [?(%t %b %l %r)]).
  ::      doing a split there is easier: simply find the to-split entry,
  ::      duplicate it in-place, and pre/append the new split.
  ::TODO  actually, this is wrong because you can't do n-ary splits...
  ::      maybe [w=?(%h %v) i=@ud n=@ud] ? idk man...
  :: ++  splats
  ::   |^  |=  [z=zone p=part]
  ::       ^-  zone
  ::       %+  roll  p
  ::       |=  [s=side =_z]
  ::       ^+  z
  ::       :-  :-  ?+  s  x.z
  ::                 %r  (add x.z (add (dvr w.z 2)))
  ::               ==
  ::           ?+  s  y.z
  ::             %b  (add y.z (add (dvr h.z 2)))
  ::           ==
  ::       :-  ?+  s  w.z
  ::             %l  (add (dvr w.z 2))
  ::             %r  (div w.z 2)
  ::           ==
  ::       ?+  s  h.z
  ::         %t  (add (dvr h.z 2))
  ::         %b  (div h.z 2)
  ::       ==
  ::   ::
  ::   +$  part  (list side)
  ::   +$  side  ?(%t %b %l %r)
  ::   --
  ::
  ++  splits
    |^  |=  [z=zone l=lay]
        ^-  (list zone)  ~+  ::TODO  should this be a recursive list?
        ?~  l  [z]~
        ::  calculate the values to distribute across the split
        ::
        =/  [d=@ r=@]  (dvr ?-(w.l %h h.z, %v w.z) (lent l.l))
        ::  calculate zone(s) for each part of the split
        ::
        |-  ^-  (list zone)
        =^  s  r    ?:(=(0 r) [d r] [+(d) (dec r)])
        =/  h=zone  [-.z ?-(w.l %h [w.z s], %v [s h.z])]
        %+  weld  ::TODO  recursive list?
          ^$(z h, l i.l.l)
        ?~  t.l.l  ~
        =-  $(l.l t.l.l, z -)
        ?-  w.l
          %h  [[x.z (add y.z s)] w.z (sub h.z s)]
          %v  [[(add x.z s) y.z] (sub w.z s) h.z]
        ==
    ::
    ::  three h->2v    [%h [~ ~ [%v [~ ~]~]]~]
    ::TODO  maybe allow any @, for metadata passthrough. or sizes?
    +$  lay   $@(~ [w=way l=(lest lay)])
    +$  way   ?(%h %v)
    ::
    :: ++  jerk
    ::   |=  [f=$%([%oust ~] [%into way]) n=@ud l=lay]
    ::   ^+  l
    ::   =.  n  +(n)
    ::   ::  going forward, n has the following semantics:
    ::   ::  =(0 n)  f has happened, conclude
    ::   ::  =(1 n)  f needs to happen at the current cel
    ::   ::    else  f needs to happen later, traverse
    ::   =;  [n=@ud l=_lay]
    ::     ?:  !=(0 n)  ~&(%woah ~|(%woah !!))
    ::     l
    ::   |-  ^-  [@ud _l]
    ::   ?:  =(0 n)  [0 l]
    ::   ?@  l
    ::     ?.  =(1 n)
    ::       [(dec n) l]
    ::     ?-  -.f
    ::       %oust  [1 l]
    ::       %into  [0 +.f ~[l ~]]
    ::     ==
    ::   |-  ^-  [@ud _l]
    ::   ::TODO  handle %oust... where?
    ::   =^  m  i.l.l  ^$(l i.l.l)
    ::   =.  n  m
    ::   ?:  =(0 n)  [0 l]
    ::   ?.  =(1 n)
    ::     ?~  t.l.l  [n l]
    ::     !!
    ::   !!
    :: ::
    :: ++  oust
    ::   |=  [n=@ud l=lay]
    ::   (jerk [%oust ~] n l)
    :: ::
    :: ++  into
    ::   |=  [a=lay n=@ud l=lay]
    ::   (jerk [%into a] n l)
    --
  --
::
::  +zo: zone-bound accumulative rendering engine
::
++  zo
  =|  biz=(list blit:dill)
  |_  =zone
  +*  zo  .
  ::
  ++  zo-apex
    |=  z=^zone
    zo(zone z)
  ::
  ++  zo-abet
    ^-  blit:dill
    ::TODO  maybe flatten? probably at outermost level.
    ::      could also cat neighboring draw calls, but could be too slow/hard
    ?:  ?=([* ~] biz)  i.biz
    [%mor (flop biz)]
  ::
  ++  zo-sure  ::  draw, global coordinates, disregarding bounds
    |=  [pot=spot sut=stub]
    zo(biz [[%klr sut] [%hop pot] biz])
  ::
  ++  zo-stun  ::  draw, local coordinates, disregarding bounds
    |=  [pot=spot sut=stub]
    (zo-sure (add:co -.zone pot) sut)
  ::
  ++  zo-stub  ::  draw, local coordinates, within bounds
    |=  [pot=spot sut=stub]
    ?:  |((gth x.pot w.zone) (gth y.pot h.zone))  zo
    ::NOTE  performance
    =.  sut  (scag:klr:format (sub w.zone x.pot) sut)
    (zo-sure (add:co -.zone pot) sut)  ::TODO  evaluate wrt graph coords
  ::
  ++  zo-cord  ::  draw, local coordinates, within bounds
    |=  [pot=spot tex=cord]
    ?:  |((gth x.pot w.zone) (gth y.pot h.zone))  zo
    =.  tex  (end 3^(sub w.zone x.pot) tex)
    ::TODO  could be %put
    (zo-sure (add:co -.zone pot) [*stye (rip 5 (taft tex))]~)
  ::
  ++  zo-cure  ::  move cursor, local coordinates, within bounds
    |=  pot=spot
    =.  x.pot  (min x.pot (dec w.zone))
    =.  y.pot  (min y.pot (dec h.zone))
    zo(biz [[%hop (add:co -.zone pot)] biz])
  ::
  ++  zo-spot  ::  global to local coordinates, if within bounds
    |=  pot=spot
    ^-  (unit spot)
    ?:  ?|  (lth x.pot x.zone)  (gth +(x.pot) (add [x w]:zone))
            (lth y.pot y.zone)  (gth +(y.pot) (add [y h]:zone))
        ==
      ~
    `(sub:co pot -.zone)
  --
::
::  +zi: zone-bound immediate-mode rendering engine
::
++  zi
  |_  =zone  ::TODO  maybe faceless is tidier
  ++  none  [%mor ~]
  ++  sure  ::  draw, global coordinates, disregarding bounds
    |=  [pot=^spot sut=^stub]
    [%mor [%hop pot] [%klr sut] ~]
  ::
  ++  stun  ::  draw, local coordinates, disregarding bounds
    |=  [pot=^spot sut=^stub]
    (sure (add:co -.zone pot) sut)
  ::
  ++  stub  ::  draw, local coordinates, within bounds
    |=  [pot=^spot sut=^stub]
    ?:  |((gth x.pot w.zone) (gth y.pot h.zone))  none
    ::NOTE  performance
    =.  sut  (scag:klr:format (sub w.zone x.pot) sut)
    (sure (add:co -.zone pot) sut)  ::TODO  evaluate wrt graph coords
  ::
  ++  cord  ::  draw, local coordinates, within bounds
    |=  [pot=^spot tex=^cord]
    ?:  |((gth x.pot w.zone) (gth y.pot h.zone))  none
    =/  put  ::(end 3^(sub w.zone x.pot) tex)
             ::TODO  muh unicode
             (scag (sub w.zone x.pot) (rip 5 (taft tex)))
    ::TODO  could be %put
    (sure (add:co -.zone pot) [*stye put]~)
  ::
  ++  wain
    |=  [pot=^spot tex=^wain]
    :-  %mor
    |-
    ?~  tex  ~
    :-  (cord pot i.tex)
    $(tex t.tex, y.pot +(y.pot))
  ::
  ++  wipe
    (wain [0 0] (reap h.zone (fil 3 w.zone ' ')))
  ::
  ++  cure  ::  move cursor, local coordinates, within bounds
    |=  pot=^spot
    =.  x.pot  (min x.pot (dec w.zone))
    =.  y.pot  (min y.pot (dec h.zone))
    [%hop (add:co -.zone pot)]
  ::
  ++  line  ::  inner stroke
    |=  $:  klr=stye
            mis=(set ?(%t %b %l %r))
          ::
            $=  tyl
            $?  @c
                [h=@c v=@c c=@c]
                [[h=@c v=@c] [tl=@c tr=@c bl=@c br=@c]]
                [[t=@c b=@c l=@c r=@c] [tl=@c tr=@c bl=@c br=@c]]
            ==
        ==
    ^-  blit:dill
    =?  tyl  ?=(@ tyl)
      [tyl tyl tyl]
    =?  tyl  ?=(@ -.tyl)
      [[h v] [c c c c]]:tyl
    =?  tyl  ?=([@ @] -.tyl)
      [[h h v v] +]:tyl
    ?>  ?=([@ @ @ @] -.tyl)
    |^  :-  %mor
        :*  ?:  (~(has in mis) %t)  none
            (hori 0 [tl t tr]:tyl)
          ::
            ?:  (~(has in mis) %b)  none
            (hori (dec h.zone) [bl b br]:tyl)
          ::
            =+  y=(sub h.zone 2)
            =+  x=(dec w.zone)
            =+  s=[klr [l.tyl]~]~
            =+  t=[klr [r.tyl]~]~
            =+  l=!(~(has in mis) %l)
            =+  r=!(~(has in mis) %r)
            |-
            ?:  =(0 y)  ~
            :*  ?:(l (stun [0 y] s) none)
                ?:(r (stun [x y] t) none)
                $(y (dec y))
        ==  ==
    ::
    ++  hori
      |=  [y=@ud l=@c c=@c r=@c]
      =/  s  (reap (sub w.zone 2) c)
      =^  x  s
        ?:  (~(has in mis) %l)  [1 s]
        [0 [l s]]
      =?  s  !(~(has in mis) %r)
        (snoc s r)
      (stun [x y] klr^s ~)
    --
  ::
  ++  spot  ::  global to local coordinates, if within bounds
    |=  pot=^spot
    ^-  (unit ^spot)
    ?:  ?|  (lth x.pot x.zone)  (gth +(x.pot) (add [x w]:zone))
            (lth y.pot y.zone)  (gth +(y.pot) (add [y h]:zone))
        ==
      ~
    `(sub:co pot -.zone)
  --
::
++  generic-input  ::  assumes byte-sized inputs
  |=  [[buf=@t cur=@ud] put=belt:dill]
  ^+  [buf cur]
  =*  lef  (dec (max 1 cur))
  ?+  put  [buf cur]
    @          [(rap 3 (end 3^cur buf) put (rsh 3^cur buf) ~) +(cur)]
    [%txt *]   $(put `@`(crip (tufa p.put)))  ::NOTE  hacky
  ::
    [%bac ~]   [(cat 3 (end 3^lef buf) (rsh 3^cur buf)) lef]
    [%del ~]   [(cat 3 (end 3^cur buf) (rsh 3^+(cur) buf)) cur]
  ::
    [%aro %l]       [buf lef]
    [%aro %r]       [buf (min +(cur) (met 3 buf))]
    [%mod %ctl %a]  [buf 0]
    [%mod %ctl %e]  [buf (met 3 buf)]
    [%mod %ctl %f]  $(put [%aro %r])
    [%mod %ctl %b]  $(put [%aro %l])
  ::
    ::TODO  others. import +ta-pos and co from drum
  ==
::
++  generic-melt  ::  cached soft-wrap
  |*  $:  width=@ud
          [start=@ud count=(unit @ud)]
          [reset=gate split=gate glaze=gate]  ::  flatten, +trim, finalize
          lines=(list [wid=@ud lin=(list)])
      ==
  =/  y=@ud  0
  =/  d=@ud  start
  |-  ^+  lines
  ?:  (gte y (fall count +(y)))  lines
  ?~  lines  ~
  =/  l=@ud  (max 1 (lent lin.i.lines))
  ?:  (gte d l)
    [i.lines $(d (sub d l), lines t.lines)]
  =?  i.lines  !=(width wid.i.lines)
    :-  width
    =/  l  (reset lin.i.lines)
    %-  glaze
    |-  ^+  lin.i.lines
    ?:  =(~ l)  ~
    =^  h  l  (split width l)
    ?:(=(~ l) [h ~] [h $])
  :-  i.lines
  $(y (add y (max 1 (lent lin.i.lines))), lines t.lines)
::
++  wa  ::  wain ops
  |_  =wain
  ++  size
    ^-  ^size
    %+  roll  wain
    |=  [a=@t w=@ud h=@ud]
    [(max w (met 5 (taft a))) +(h)]
  --
::
++  wrap
  |%
  :: ++  lies
  ::   |$  [met val]
  ::   (list [met (list val)])
  ::
  ++  lent
    |=  a=(list [* b=(list)])
    %+  roll  a
    |=  [[* c=(list)] d=@ud]
    (add d (max 1 (^lent c)))
  ::
  ++  slap
    |*  [n=@ud a=(list [w=* l=(list)]) b=gate]
    !!
  ::
  ++  slag
    |*  [n=@ud a=(list [w=* l=(list)])]
    |-  ^+  a
    ?:  =(0 n)  a
    ?~  a  ~
    ~!  a
    ?:  =(~ l.i.a)  $(n (dec n), a t.a)
    |-
    ?~  l.i.a  ^$(a t.a)
    ?:  =(0 n)  a
    $(n (dec n), l.i.a t.l.i.a)
  ::
  ++  snag
    |*  [n=@ud l=(list [* l=(list)])]
    ?~  l  !!
    ?:  =(0 n)
      ?~(l.i.l ~ i.l.i.l)
    ?~  l.i.l  $(n (dec n), l t.l)
    |-
    ?:  =(0 n)  i.l.i.l
    =.  n  (dec n)
    ?~  t.l.i.l  ^$(l t.l)
    $(l.i.l t.l.i.l)
  --
::
++  generic-scroll
  |=  $:  sizes=(list @ud)
          point=@ud
          [start=@ud reach=@ud]  ::  viewport top & height
          swipe=?(~ %u %d %l %r)  ::  recalc, up, down, pageup, pagedown
      ==
  ^+  [point start]
  =/  items=@ud  (lent sizes)
  ::  first, adjust the pointer to the new list index
  ::
  =.  point
    ?+  swipe  point  ::TODO  pageup & pagedown
      ~   point
      %u  (dec (max 1 point))
      %d  (min (dec (max 1 items)) +(point))
    ==
  :-  point
  ::  then adjust the viewport as necessary
  ::
  ?:  (lte point start)  point
  ::TODO  performance
  =.  sizes  (slag start sizes)
  =/  above  *(qeu @ud)
  =/  count  0
  =/  i      start
  |-
  ?:  =(i point)  start
  =.  i  +(i)
  ?~  sizes  start
  =.  count  (add count i.sizes)
  =.  above  (~(put to above) i.sizes)
  ?:  (lth count reach)  $(sizes t.sizes)
  |-
  =^  hop=@ud  above  ~(get to above)  ::TODO  what if ~
  =.  count  (sub count hop)  ::TODO  what if hop > count?
  =.  start  +(start)
  ^$(sizes t.sizes)
::
++  generic-scag
  |*  $:  skimp=@ud
          lines=(list [wid=@ud lin=(list)])
      ==
  =/  d=@ud  skimp
  |-  ^+  lines
  ?~  lines  ~
  ?:  =(0 d)     lines
  =/  l=@ud      (max 1 (lent lin.i.lines))
  ?:  =(d l)     t.lines
  ?:  (gth d l)  $(d (sub d l), lines t.lines)
  [[wid.i.lines (slag d lin.i.lines)] lines]
::
::  +|  %opinions
::
++  xx  !!
--
