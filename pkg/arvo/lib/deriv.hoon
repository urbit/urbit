::  https://khoury.northeastern.edu/home/turon/re-deriv.pdf
::  https://crypto.stanford.edu/~blynn/haskell/re.html
:::::::::::::::::::::::::::::::::::::::::
::  character sets
:::::::::::::::::::::::::::::::::::::::::
|%
+$  mult  [asc=@ub big=(set @)]
+$  chaz  $~(*mult $@(@ mult))
+$  chap  [pos=? s=chaz]
--
|%
++  ascp   =>(l=lte |=(c=@ (l c 127)))
++  for-ascii
  |*  f=$-([@ *] *)
  =|  [i=@ out=_+<+.f]
  =+  a=ascp
  |-  ^+  out
  ?.  (a i)  out
  $(i +(i), out (f i out))
++  bs
  |_  a=@ub
  ++  has  |=(c=@ =(1 (cut 0 [c 1] a)))
  ++  sew  |=([c=@ v=?(%0 %1)] `@ub`(^sew 0 [c 1 v] a))
  ++  put  |=(c=@ (sew c %1))
  ++  del  |=(c=@ (sew c %0))
  ++  int  |=(b=@ub `@ub`(dis a b))
  ++  uni  |=(b=@ub `@ub`(con a b))
  ++  dif  |=(b=@ub `@ub`(dis a (not 0 (met 0 a) b)))
  ++  one  ^-  (unit @)
           ?:  =(0 a)  ~
           ?.  =(0 (dis a (dec a)))  ~
           `(dec (xeb a))
  ++  tao  |=  on=(list @)  ^+  on
           %-  for-ascii  =>  [h=has on=on]
           |=([c=@ o=_on] ?:((h c) [c o] o))
  ++  tap  (tao ~)
  --
++  bs-range
  |=  [lo=@ hi=@]
  ^-  @ub
  (lsh [0 lo] (dec (bex +((sub hi lo)))))
++  mu
  |_  a=mult
  ++  has  |=  c=@
           ?:  (ascp c)
             (~(has bs asc.a) c)
           (~(has in big.a) c)
  ++  put  |=  c=@
           ?:  (ascp c)
             a(asc (~(put bs asc.a) c))
           a(big (~(put in big.a) c))
  ++  del  |=  c=@
           ?:  (ascp c)
             a(asc (~(del bs asc.a) c))
           a(big (~(del in big.a) c))
  ++  gas  |=  cs=(list @)
           =/  p  put
           %+  roll  cs
           |=  [i=@ s=_a]
           $:p(a s, c i)
  ++  int  |=  b=mult   ^-  mult
           :-  (~(int bs asc.a) asc.b)
               (~(int in big.a) big.b)
  ++  uni  |=  b=mult   ^-  mult
           :-  (~(uni bs asc.a) asc.b)
               (~(uni in big.a) big.b)
  ++  dif  |=  b=mult   ^-  mult
           :-  (~(dif bs asc.a) asc.b)
               (~(dif in big.a) big.b)
  ++  tap  `(list @)`(~(tao bs asc.a) ~(tap in big.a))
  ++  nom  ^-  chaz
           ?@  big.a
             =/  got  ~(one bs asc.a)
             ?@(got a u.got)
           ?.  =(0 asc.a)  a
           ?.  ?=([* ~ ~] big.a)  a
           n.big.a
  ++  pel  :: pretty-printing elements
    ^-  (list tape)
    =/  clump
      %+  roll  tap
      |=  [c=@ out=(map @ [lo=@ hi=@])]
      =/  cup  +(c)
      =/  gup  (~(get by out) cup)
      =/  update
        |=  v=[lo=@ hi=@]
        =+  i=lo.v  |-
        =.  out  (~(put by out) i v)
        ?:  =(i hi.v)  out
        $(i +(i))
      ?^  gup  (update u.gup(lo c))
      =/  don  (dec c)
      =/  gon  (~(get by out) don)
      ?^  gon  (update u.gon(hi c))
      (~(put by out) c c c)
    =/  harp=(unit _clump) :: '-' appears by itself
      =/  got  (~(get by clump) '-')
      ?@  got  ~
      ?:  (gth (sub hi.u.got lo.u.got) 1)  ~
      :-  ~
      =+  [i=lo.u.got m=clump]
      |-
      =.  m  (~(del by m) i)
      ?:  =(i hi.u.got)  m
      $(i +(i))
    =?  clump  ?=(^ harp)  u.harp
    =-  ?@(harp - ["-" -])
    %+  turn
      %+  sort  ~(tap in (~(gas in *(set [lo=@ hi=@])) ~(val by clump)))
      |=([[a=@ @] b=@ @] (lth a b))
    |=  [lo=@ hi=@]
    =/  dif  (sub hi lo)
    ?:  =(0 dif)  (char-tape lo)
    ?:  =(1 dif)  (weld (char-tape lo) (char-tape hi))
    "{(char-tape lo)}-{(char-tape hi)}"
  --
++  two  |=  [a=@ b=@]  ^-  chaz
         ~(nom mu (~(put mu (~(put mu *mult) a)) b))
++  cup  |=  [a=@ b=@]  ^-  chaz
         ?:  =(a b)  a
         (two a b)
++  cz
  |_  a=chaz
  ++  has  |=(c=@ ?@(a =(c a) (~(has mu a) c)))
  ++  put  |=  c=@  ^-  chaz
           ?^  a  (~(put mu a) c)
           (cup a c)
  ++  del  |=  c=@  ^-  chaz
           ?@  a  ?:(=(a c) *mult a)
           ~(nom mu (~(del mu a) c))
  ++  gas  |=  cs=(list @)  ^-  chaz
           ~(nom mu (~(gas mu ?^(a a (~(put mu *mult) a))) cs))
  ++  int  |=  b=chaz  ^-  chaz
           ?@  a
             ?@  b  ?:(=(a b) a *mult)
             ?:((~(has mu b) a) a *mult)
           ?@  b  ?:((~(has mu a) b) b *mult)
           ~(nom mu (~(int mu a) b))
  ++  uni  |=  b=chaz  ^-  chaz
           ?@  a
             ?@  b  (cup a b)
             ~(nom mu (~(put mu b) a))
           ?@  b  (~(put mu a) b)
           ~(nom mu (~(uni mu a) b))
  ++  dif  |=  b=chaz  ^-  chaz
           ?@  a
             ?@  b  ?:(=(a b) *mult a)
             ?:((~(has mu b) a) *mult a)
           ?@  b  (~(del mu a) b)
           ~(nom mu (~(dif mu a) b))
  ++  pel  `(list tape)`?@(a ~[(char-tape a)] ~(pel mu a))
  ++  pak  `(list tank)`(turn pel |=(a=tape leaf+a))
  ++  tan  `tank`[%rose ["" "[" "]"] pak]
  --
++  mult-range
  |=  [lo=@ hi=@]
  ^-  mult
  ?:  (ascp hi)
    [(bs-range lo hi) ~]
  (~(gas mu *mult) (gulf lo hi))
++  chaz-range
  |=  [lo=@ hi=@]
  ^-  chaz
  ?:  =(lo hi)  lo
  (mult-range lo hi)
++  pl
  |_  a=chap
  ++  has  |=  c=@
           =/  hav=?  (~(has cz s.a) c)
           ?:(pos.a hav !hav)
  ++  put  |=  c=@  ^-  chap
           ?:  pos.a  a(s (~(put cz s.a) c))
           a(s (~(del cz s.a) c))
  ++  del  |=  c=@  ^-  chap
           ?:  pos.a  a(s (~(del cz s.a) c))
           a(s (~(put cz s.a) c))
  ++  int  |=  b=chap  ^-  chap
           ?:  pos.a
             ?:  pos.b  &+(~(int cz s.a) s.b)
             &+(~(dif cz s.a) s.b)
           ?:  pos.b  &+(~(dif cz s.b) s.a)
           |+(~(uni cz s.a) s.b)
  ++  uni  |=  b=chap  ^-  chap
           ?:  pos.a
             ?:  pos.b  &+(~(uni cz s.a) s.b)
             |+(~(dif cz s.b) s.a)
           ?:  pos.b  |+(~(dif cz s.a) s.b)
           |+(~(int cz s.a) s.b)
  ++  dif  |=  b=chap  ^-  chap
           ?.  pos.a  |+(~(uni cz s.a) s.b)
           ?:  pos.b  &+(~(dif cz s.a) s.b)
           &+(~(int cz s.a) s.b)
  ++  not  a(pos !pos.a)
  ++  tan  `tank`[%rose ["" ?:(pos.a "[" "[^") "]"] ~(pak cz s.a)]
  --
++  char-tape
  |=  c=@
  ?:  ?|  =(c '\\')
          =(c ']')
          =(c '^')
          (lth c 32)
          (gth c 126)
      ==
    "\\{<c>}/"
  [c ~]
++  char-tank
  |=  c=@
  leaf+(char-tape c)
:::::::::::::::::::::::::::::::::::::::::
::  regular expressions
:::::::::::::::::::::::::::::::::::::::::
+$  regx                          :: (list,set) for normalizaton
  $~  [%set | *mult]
  $%  [%set chap]
      [%star r=regx]
      [%cat nul=? l=(list regx)]  :: associativity
      [%and nul=? s=(set regx)]   :: order irrelevance
      [%or nul=? s=(set regx)]
      [%not nul=? r=regx]
  ==
--
=*  no-chars=chap  [& 0b0 ~]
=*  no-good=regx   [%set no-chars]
=*  sigma=chap     [| 0b0 ~]
=*  re-dot=regx    [%set sigma]
=*  all-good=regx  [%star re-dot]
=*  eps=regx       [%star no-good]
|%
++  re-nul  :: does this regex accept the empty string
  |=  r=regx
  ?+  -.r  nul.r
    %set   |
    %star  &
  ==
++  re-cat
  |=  l=(list regx)
  ^-  regx
  =.  l
    %+  reel  l
    |=  [r=regx l=(list regx)]
    ?:  =(r eps)  l
    ?:  ?=(%cat -.r)
      (weld l.r l)
    [r l]
  ?:  (lien l |=(r=regx =(r no-good)))  no-good
  ?+  l    [%cat (levy `(list regx)`l re-nul) l]
    ~      eps
    [* ~]  i.l
  ==
++  concat
  |*  [a=(lest) f=$-(^ *)]
  =/  out  i.a
  =/  bro  t.a
  |-  ^+  out
  ?~  bro  out
  $(out (f out i.bro), bro t.bro)
++  coalesce
  |=  [l=(list regx) f=$-([chap chap] chap)]
  ^-  (list regx)
  =/  grp
    %+  roll  l
    |=  [r=regx out=[sets=(list chap) regs=(list regx)]]
    ?:  ?=(%set -.r)
      out(sets [+.r sets.out])
    out(regs [r regs.out])
  ?~  sets.grp  regs.grp
  :_  regs.grp
  set+(concat sets.grp f)
++  re-or
  |=  l=(list regx)  ^-  regx
  =/  s=(set regx)
    %+  roll  (coalesce l |=([a=chap b=chap] (~(uni pl a) b)))
    |=  [r=regx s=(set regx)]
    ^+  s
    ?:  =(r no-good)  s
    ?:  ?=(%or -.r)
      (~(uni in s) s.r)
    (~(put in s) r)
  ?:  (~(has in s) all-good)  all-good
  ?+  s      [%or (~(any in `(set regx)`s) re-nul) s]
    ~        no-good
    [* ~ ~]  n.s
  ==
++  re-and
  |=  l=(list regx)  ^-  regx
  =/  s=(set regx)
    %+  roll  (coalesce l |=([a=chap b=chap] (~(int pl a) b)))
    |=  [r=regx s=(set regx)]
    ?:  =(r all-good)  s
    ?:  ?=([%and *] r)
      (~(uni in s) s.r)
    (~(put in s) r)
  ?:  (~(has in s) no-good)  no-good
  ?+  s      [%and (~(all in `(set regx)`s) re-nul) s]
    ~        all-good
    [* ~ ~]  n.s
  ==
++  re-star
  |=  r=regx  ^-  regx
  ?:  ?=(%star -.r)  r
  [%star r]
++  re-not
  |=  r=regx  ^-  regx
  ?:  =(no-good r)  all-good
  ?:  ?=(%not -.r)  r.r
  ::  at first i thought you would flip a charset, but no.
  ::  the empty string, for example, should be in the negation.
  [%not !(re-nul r) r]
++  re-plus
  |=  r=regx
  (re-and (re-star r) (re-not eps) ~)
++  re-class
  |=  [pos=? cs=(list @)]  ^-  regx
  [%set pos (~(gas cz *chaz) cs)]
++  re-sep
  |=  [sep=regx item=regx]
  (re-cat item (re-star (re-cat sep item ~)) ~)
++  re-range
  |=  [lo=@ hi=@]
  ^-  regx
  [%set & (chaz-range +<)]
++  re-opt     |=(r=regx (re-or r eps ~))
++  re-char    |=(c=@ (re-class & c ~))
++  re-tape    |=(=tape (re-cat (turn tape re-char)))
++  re-error   |=(r=regx =(r no-good))
++  re-final   |=(r=regx `(list @)`?:((re-nul r) ~[0] ~))
++  vec-error  |=(v=(list regx) (levy v re-error))
++  vec-final
  |=  v=(list regx)
  =|  [i=@ out=(list @)]
  |-  ^+  out
  ?~  v  out
  =?  out  (re-nul i.v)  [i out]
  $(v t.v, i +(i))
++  regx-tank
  |=  r=regx
  ^-  tank
  ?-  -.r
    %star  [%rose ["" "" ""] $(r r.r) '*' ~]
    %not   [%rose ["" "" ""] '!' $(r r.r) ~]
    %set   ~(tan pl +.r)
    %cat   [%rose [" " "(" ")"] (turn l.r ..$)]
    %and   [%rose [" " "&(" ")"] (turn ~(tap in s.r) ..$)]
    %or    [%rose [" " "|(" ")"] (turn ~(tap in s.r) ..$)]
  ==
:::::::::::::::::::::::::::::::::::::::::
::  derivation
:::::::::::::::::::::::::::::::::::::::::
++  cart
  |*  [a=(list) b=(list)]
  ?~  a  ~
  ?~  b  ~
  =*  at  _i.a
  =*  bt  _i.b
  %+  roll  `(list at)`a
  |=  [a=at out=(list [at bt])]
  %+  roll  `(list bt)`b
  |=  [b=bt out=_out]
  [[a b] out]
++  meet
  |=  [a=(set chap) b=(set chap)]
  ^-  (set chap)
  %+  roll  (cart ~(tap in a) ~(tap in b))
  |=  [[x=chap y=chap] out=(set chap)]
  (~(put in out) (~(int pl x) y))
++  re-approx
  |=  r=regx
  ^-  (set chap)
  ?-  -.r
    %set   (sy +.r ~(not pl +.r) ~)
    %cat   ?~  l.r  [[| ~] ~ ~]
           =/  out  $(r i.l.r)
           ?.  (re-nul i.l.r)  out
           =/  mor  t.l.r
           |-  ^+  out
           ?~  mor  out
           =.  out  (meet out ^$(r i.mor))
           ?.  (re-nul i.mor)  out
           $(mor t.mor)
    %or    =/  sets  (turn ~(tap in s.r) ..$)
           ?~(sets !! (concat sets meet))
    %and   =/  sets  (turn ~(tap in s.r) ..$)
           ?~(sets !! (concat sets meet))
    %star  $(r r.r)
    %not   $(r r.r)
  ==
++  vec-approx
  |=  v=(list regx)
  =*  s  (set chap)
  ^-  s
  =/  els=(list s)  (turn v re-approx)
  ?~  els  ~  (concat els meet)
++  gderive
  |*  has=$-([chap *] ?)
  ::  r should be normalized (made with the smart constructors)
  |=  [c=_+<+.has r=regx]
  |^  ^-  regx
    ?-  -.r
      %set   ?:((has +.r c) eps no-good)
      %star  (re-cat $(r r.r) (re-star r.r) ~)
      %and   (re-and (turn ~(tap in s.r) down))
      %or    (re-or (turn ~(tap in s.r) down))
      %not   (re-not $(r r.r))
      %cat   ?~  l.r  !!
             =/  def=regx  (re-cat $(r i.l.r) t.l.r)
             ?.  (re-nul i.l.r)  def
             (re-or def $(r (re-cat t.l.r)) ~)
    ==
  ++  down  |=(r=regx ^$(r r))
  --
++  char-derive  (gderive |=([s=chap c=@] (~(has pl s) c)))
++  re-derive  %-  gderive
  |=  [s=chap c=chap]
  ::  inefficient but simple and correct
  ?!  =(no-chars (~(int pl s) c))
++  vec-derive
  |=  [s=chap v=(list regx)]
  (turn v |=(r=regx (re-derive s r)))
++  deriv-match  :: very slow, but useful for testing
  |=  [r=regx txt=tape]
  =|  i=@
  |-  ^-  (unit @)
  ?~  txt  ?:((re-nul r) `i ~)
  =/  d=regx  (char-derive i.txt r)
  ?:  =(no-good d)  ~
  $(txt t.txt, r d, i +(i))
++  deriv-lex  :: slow like deriv-match
  |=  [v=(list regx) txt=tape]
  =+  [`i=@`0 `last=tork`~]
  |-  ^+  last
  ::~>  %slog.[0 [%rose [" <v> " "" ""] (turn v regx-tank)]]
  ?~  txt  last
  ::~&  char=i.txt
  =/  fin  (vec-final v)
  =?  last  ?=(^ fin)  [i fin]
  =/  d  (vec-derive [& i.txt] v)
  ?:  (vec-error d)  last
  $(txt t.txt, v d, i +(i))
:::::::::::::::::::::::::::::::::::::::::
::  machines
:::::::::::::::::::::::::::::::::::::::::
+$  aros  :: transitions from mast
  $@  ~  ::  no out transitions
  $:  chex=(list (pair @ chap))
      else=(unit @)
  ==
+$  mast  :: machine state
  $:  out=aros
      vim=(list @) :: vector indices of match
  ==
+$  mach  (map @ mast)
+$  mart  [i=@ tags=(list @)]
+$  tork  $@(~ mart)
--
|%
++  measure-sort
  |*  [l=(list) cmp=$-(^ ?) met=$-(* *)]
  ?~  l  ~
  =/  t  _i.l
  =/  m  _(met)
  %-  turn  :_  |=([m a=t] a)
  %-  sort  :_  |=([[a=m t] b=m t] (cmp a b))
  %+  turn  l
  |=  a=t
  ^-  [m t]
  [(met a) a]
++  make-dfa
  |*  $:  approx=$-(* (set chap))
          derive=$-(^ *)
          error=$-(* ?)
          final=$-(* (list @))
      ==
  =*  lbl  _+<.approx
  |=  r=lbl
  =<  mac
  =|  st=[nid=@ hav=(map lbl @) mac=mach]
  |-  ^+  st  :: each loop adds completed state to the machine
  =/  sid     nid.st
  =.  hav.st  (~(put by hav.st) r sid)
  =+  :*  ^-  done=chap           no-chars
          ^-  chex=(map @ chap)  ~
          ^-  parts=(list chap)  ~(tap in (approx r))
      ==
  |-  ^+  st  :: each loop adds a partition to the state
  ?^  parts
    =/  nex  i.parts
    ::  weed out transitions that go to error states
    ?:  =(no-chars nex)  $(parts t.parts)
    =/  d  (derive nex r)
    ?:  (error d)  $(parts t.parts)
    =>  %=  .
        done   (~(uni pl done) nex)
        parts  t.parts
      ==
    =^  tgt=@  st
      =/  got  (~(get by hav.st) d)
      ?^  got  [u.got st]
      =/  him  +(nid.st)
      :-  him  ^$(r d, nid.st him)
    =/  got  (~(get by chex) tgt)
    %=  $
      chex  %+  ~(put by chex)  tgt
            ?@  got  nex
            ::  combine charsets with the same target
            (~(uni pl u.got) nex)
    ==
  =/  chx
    =-  (flop o)
    %+  roll
      %^  measure-sort  ~(tap by chex)  lth
      |=  [* s=chap]  ^-  @
      ?@  s.s  0
      +(~(wyt in big.s.s))
    ::  having now fixed a testing order, we can shrink some of the
    ::  tests by propagating failures to later tests in two sets
    |=  [i=(pair @ chap) pos=chaz neg=chaz o=(list (pair @ chap))]
    ^+  +<+
    ?:  pos.q.i
      :+  pos=pos  neg=(~(uni cz s.q.i) neg)
      :_  o  i(s.q (~(dif cz s.q.i) pos))
    :+  (~(uni cz s.q.i) pos)  neg
    :_  o  i(s.q (~(dif cz s.q.i) neg))
  %=  st
    mac  %+  ~(put by mac.st)  sid
         :_  vim=(final r)
         ^-  out=aros
         ?~  chx  ~
         ?.  =(done sigma)  [chex=chx else=~]
         ::  skip the last (most expensive) test
         =/  rev  (flop chx)
         ?~  rev  !!
         [chex=(flop t.rev) else=`p.i.rev]
  ==
++  re-dfa
  (make-dfa re-approx re-derive re-error re-final)
++  vector-dfa
  (make-dfa vec-approx vec-derive vec-error vec-final)
++  mach-trans
  |=  [c=@ tan=aros]
  ^-  (unit @)
  ?@  tan  ~
  ?~  chex.tan  else.tan
  ?:  (~(has pl q.i.chex.tan) c)
    `p.i.chex.tan
  $(chex.tan t.chex.tan)
++  mach-run
  |=  [m=mach txt=tape]
  =/  [i=@ s=@ gud=tork]  [0 0 ~]
  |-  ^-  tork
  =/  sov  (~(got by m) s)
  =?  gud  ?=(^ vim.sov)  [i vim.sov]
  ?@  txt  gud
  =/  nxt  (mach-trans i.txt out.sov)
  ?@  nxt  gud
  $(i +(i), txt t.txt, s u.nxt)
++  mach-match  :: same api as deriv-match
  |=  [mac=mach txt=tape]
  ^-  (unit @)
  =/  r  (mach-run mac txt)
  ?~  r  ~
  [~ i.r]
:::::::::::::::::::::::::::::::::::::::::
::  nock generation
:::::::::::::::::::::::::::::::::::::::::
++  battery-axes
  |*  [nam=(list) len=@]
  ?@  nam  ~
  =<  out
  =+  :-  `her=@`2
      ^=  st
      :-  `xs=$@(~ _nam)`nam
      `out=(map _i.nam @)`~
  |-  ^+  st
  ?:  =(0 len)  st
  ?:  =(1 len)
    ?~  xs.st  !!
    st(xs t.xs.st, out (~(put by out.st) i.xs.st her))
  =/  haf  (rsh [0 1] len)
  =/  kid  (lsh [0 1] her)
  $(st $(len haf, her kid), len (sub len haf), her +(kid))
++  mach-formula
  |=  $:  eof=nock
          next=nock
          char=nock
          bhas=nock
          shas=nock
          mhas=nock
      ==
  |=  [mac=mach pick=$-((lest @) *)]
  ^-  nock
  =+  =|  [st=[ks=(list @) len=@]]
      |-  ^+  st
      ?@  mac  st
      =.  st  [[p.n.mac ks.st] +(len.st)]
      $(st $(mac l.mac), mac r.mac)
  =/  sid-to-axis  (battery-axes ks len)
  =/  axis-to-sid
    %+  roll  ks
    |=  [sid=@ out=(map @ @)]
    (~(put by out) (~(got by sid-to-axis) sid) sid)
  ::  subject starts as cursor, make [bat cur ~] core and pull 0 axis
  =-  [%9 (~(got by sid-to-axis) 0) [%1 bat] [%0 1] %1 0]
  =-  ^=  bat
      =/  her=@  2
      |-  ^-  nock
      =/  got  (~(get by axis-to-sid) her)
      ?^  got  (ma (~(got by mac) u.got))
      =/  kid  (lsh [0 1] her)
      [$(her kid) $(her +(kid))]
  |%
  ++  test
    |=  a=chaz
    ^-  nock
    ?@  a  [%5 [%0 2] %1 a]
    =-  [%7 [[%1 lit] %0 2] t]
    ^=  [t lit]
    ?@  big.a  [bhas asc.a]
    ?:  =(0 asc.a)  [shas big.a]
    [mhas a]
  ++  branch
    |=  [p=chap yes=nock no=nock]
    :+  %6  (test s.p)
    ?:(pos.p [yes no] [no yes])
  ++  goto  ::  [c bat cur gud]
    |=  [vim=(list @) sid=@]
    ^-  nock
    ::  uncomment to get a print every time we change states
    :: :+  %11  [%slog [%1 0] %1 (sell [%atom %ud ~] sid)]
    :+  %9  (~(got by sid-to-axis) sid)
    =/  nef  [%7 [%0 14] next]
    :-  %10  :_  [%0 3]
    ?@  vim  [6 nef]
    [3 nef [%1 (pick vim)] %0 14]
  ++  ma
    |_  s=mast  ::  [bat cur gud]
    ++  $
      ^-  nock
      ?@  out.s  (exit 3)
      :^  %6  [%7 [%0 6] eof]  (exit 3)
      :+  %8  [%7 [%0 6] char]  ::  [c bat cur gud]
      ::  uncomment to print every character as we get it
      :: :+  %11  [%slog [%1 0] %0 2]
      =/  l  chex.out.s
      ?@  else.out.s
        |-  ^-  nock
        ?@  l  (exit 7)
        (branch q.i.l (goto vim.s p.i.l) $(l t.l))
      |-  ^-  nock
      ?@  l  (goto vim.s u.else.out.s)
      (branch q.i.l (goto vim.s p.i.l) $(l t.l))
    ++  exit
      |=  pay=@
      ^-  nock
      ?@  vim.s  [%0 (peg pay 3)]
      [[%1 (pick vim.s)] %0 (peg pay 2)]
    --
  --
++  cord-nock
  %-  mach-formula
  :*  eof=[%5 [%0 2] %0 6]
      next=[%10 [2 %4 %0 2] %0 1]
      char=[%9 2 %10 [6 [%1 3] [[%0 2] %1 1] %0 7] %1 cut]
      ^=  bhas
      =-  [%9 2 %10 [6 %0 1] %1 -]
          =/  lhas  ~(has bs 0b0)
          |=  [s=@ c=@]
          $:lhas(a s, c c)
      ^=  shas
      =-  [%9 2 %10 [6 %0 1] %1 -]
          =/  lhas  ~(has in *(set @))
          |=  [s=(set @) c=@]
          $:lhas(a s, b c)
      ^=  mhas
      =-  [%9 2 %10 [6 %0 1] %1 -]
          =/  lhas  ~(has mu *mult)
          |=  [s=mult c=@]
          $:lhas(a s, c c)
  ==
:::::::::::::::::::::::::::::::::::::::::
::  machine pretty-printing (debug)
:::::::::::::::::::::::::::::::::::::::::
++  mump
  |%
  ++  arrows
    |=  t=aros
    ^-  tank
    ?@  t  leaf+"."
    =/  chx=tank
      =-  [%rose [", " "" ""] -]
      ^-  (list tank)
      %+  turn  chex.t
      |=  (pair @ chap)
      ^-  tank
      [%rose [" -> " "" ""] ~(tan pl q) leaf+(scow %ud p) ~]
    ?~  else.t  chx
    [%rose [" else " "" ""] chx leaf+(scow %ud u.else.t) ~]
  ++  state
    |=  s=mast
    ^-  tank
    :+  %rose  [" " "" ""]  :~
      [%rose [" " "(" ")"] (turn vim.s |=(a=@ leaf+(scow %ud a)))]
      (arrows out.s)
    ==
  ++  machine
    |=  mac=mach
    ^-  tank
    =-  [%rose [", " "states: " ""] -]
    ^-  (list tank)
    %+  turn
      %+  sort  ~(tap by mac)
      |=([[a=@ *] b=@ *] (lth a b))
    |=  [id=@ s=mast]
    ^-  tank
    :+  %palm  [" => " "" "" ""]
    ^-  (list tank)
    ~[leaf+(scow %ud id) (state s)]
  --
++  digits
  |%
  ++  fold
    |=  b=@
    |=  [dig=@ m=_1 a=@]
    :-  (mul m b)
    (add a (mul dig m))
  ++  little
    |=  [bas=@ d=(list @)]
    a:(roll d (fold bas))
  ++  big
    |=  [bas=@ d=(list @)]
    a:(reel d (fold bas))
  ++  decimal
    |=  t=tape
    (turn t |=(c=@ (sub c '0')))
  --
--
:::::::::::::::::::::::::::::::::::::::::
::  lexer construction helpers
:::::::::::::::::::::::::::::::::::::::::
|%
++  tokenize-gate
  |$  cursor
  $-(cursor $@(~ [tag=* cur=cursor]))
+$  cord-cursor  [i=@ len=@ txt=@t]
+$  tape-cursor  [i=@ t=tape]
+$  cunk         [txt=@ from=@ to=@]
++  nullable-c  :: nullable grammar arm with certainly cellular product
  |$([c st] [$@(~ u=c) st])
++  nullable-a  :: with a possibly atomic product
  |$([a st] [(unit a) st])
  ::  call either one like this:
  ::    =^  r  st  nullable-arm
  ::    ?@  r  ::  r was null
  ::    u.r is bound
++  mandatory   :: return type for non-nullable grammar arms
  |$([r st] $@(~ [u=r s=st]))
  ::  for a mandatory caller:
  ::    =+  mandatory-arm
  ::    ?@  -  ~
  ::    =>  [x=u +(st s)]
  ::  because the mandatory arm may peek and then fail, it takes some
  ::  care to ensure we don't drop a peek.
  ::    =.  st  fill  :: unless already preserved
  ::    =+  mandatory-arm
  ::    ?@  -  `st
  ::    =>  [x=u +(st s)]
--
|%
++  init-cord-cursor
  |=  txt=cord
  ^-  cord-cursor
  [0 (met 3 txt) txt]
++  token-map
  |=  (list (pair * regx))
  ^-  [vec=(list regx) tags=(map @ *)]
  =-  [(flop vec) tags]
  %+  roll  +<
  |=  [(pair * regx) i=@ vec=(list regx) tags=(map @ *)]
  :+  +(i)
    [q vec]
  (~(put by tags) i p)
++  pick-first
  |=  ice=(lest @)
  (roll t.ice =+(min -(+<+ i.ice)))
++  pick-first-mapped
  |=  m=(map @ *)
  |=  ice=(lest @)
  (~(got by m) (pick-first ice))
::  easier on tapes than cords, but could be optimized.
++  bsfs :: \123/ or \(special)
  |=  [t=tape spec=(map @ @)]
  =|  out=tape
  |-  ^-  tape
  ?~  t  (flop out)
  ?.  =(i.t '\\')  $(out [i.t out], t t.t)
  ?~  t.t  !!
  =/  got  (~(get by spec) i.t.t)
  ?^  got  $(out [u.got out], t t.t.t)
  =|  [digs=(list @) l=_t.t]
  |-  ^-  tape
  ?:  =('/' i.l)
    ^$(out [(little:digits 10 digs) out], t t.l)
  ?~  t.l  !!
  $(l t.l, digs [(sub i.l '0') digs])
::  fold-bytes vs trip+roll: relies on the runtime optimizing edits to
::  produce less ephemeral garbage, but always avoids allocating the
::  entire chunk of src as a list.
++  fold-bytes
  |*  [cunk f=$-([@ *] *)]
  =>  :+  [from=from to=to]  f=f  ^=  c
      =>  [txt=txt cut=cut]
      cut(+< [3 [0 1] txt])
  |-  ^+  +<+.f
  ?:  =(from to)  +<+.f
  %=  $
    from  +(from)
    +<+.f  $:f(+<- $:c(+<+<- from))
  ==
--
:::::::::::::::::::::::::::::::::::::::::
::  regex mini-language (tore)
:::::::::::::::::::::::::::::::::::::::::
|%
+$  tort                :: tore token
  $~  [%cc & '']
  $%  [%one c=@]        :: one of the one-character tokens
      [%cc p=chap]      :: character class
      [%id p=term]      :: identifier
      [%num p=@]        :: code point
      [%rep lo=@ hi=@]  :: repetition suffix
      [%quot txt=tape]  :: literal string
  ==
+$  torx                ::  tore expression
  $~  [%cc & '']
  $%  [%cc p=chap]
      [%quot txt=tape]
      [%id p=term]
      [%zap e=torx]
      [%tar e=torx]
      [%lus e=torx]
      [%wut e=torx]
      [%rep lo=@ hi=@ e=torx]
      [%seq p=torx q=torx]
      [%pam p=torx q=torx]
      [%bar p=torx q=torx]
  ==
--
|%
++  tore
  |%
  ++  gate
    =+  [vec tag]=def
    ::~&  'tore'
    :: ~>  %slog.[0 (machine:mump (vector-dfa vec))]
    =/  fol=nock
      (cord-nock (vector-dfa vec) (pick-first-mapped tag))
    =/  levi  |=(* &)
    !<  (tokenize-gate cord-cursor)
    `|=(bus=cord-cursor .*(bus fol))
  ++  def
    =/  lc-alpha    (re-range 'a' 'z')
    =/  uc-alpha    (re-range 'A' 'Z')
    =/  decimal     (re-range '0' '9')
    =/  number      (re-plus decimal)
    =/  alpha       (re-or lc-alpha uc-alpha ~)
    =/  alnum       (re-or alpha decimal ~)
    =/  soq         (re-char '\'')
    =/  bas         (re-char '\\')
    =/  white       (re-class & ' ' 9 10 13 ~)
    =/  qlike
      |=  [close=@ open=regx]
      %-  re-cat
      :~  open
          %-  re-star
          %-  re-and
          :~  (re-not (re-char close))
              %-  re-or
              :~  (re-class | "\\")
                  %-  re-cat
                  :~  (re-char '\\')
                      %-  re-or
                      :~  (re-class & [close "\\rnt"])
                          %-  re-cat
                          :~  (re-plus (re-range '0' '9'))
                              (re-char '/')
                          ==
                      ==
                  ==
              ==
          ==
          (re-char close)
      ==
    %-  token-map  :~
      quot+(qlike '\'' (re-char '\''))
      clas+(qlike ']' (re-cat (re-char '[') (re-opt (re-char '^')) ~))
      id+(re-cat alpha (re-star alnum) ~)
      num+number
      ws+(re-plus white)
      one+(re-class & "()*+|&!?")
      :-  %rep  %-  re-cat  :~
        (re-char '{')
        number
        %-  re-opt  %-  re-cat  :~
          (re-char ',')
          number
        ==
        (re-char '}')
      ==
    ==
  --
--
::  some unresolved questions of how to put this into the jet-stack
::  like, we are generating a core here, that could be hinted, right?
::  because we do want to generate the lexer as part of loading hoon.hoon
::  (don't we?) grousing: if jet registration weren't so fracking fragile,
::  this wouldn't be an issue. could just punt on that question until
::  jets are fixed lol.
=/  tore-lex  gate:tore
|%
++  torp
  |%
  ++  rep
    |=  t=tape
    =+  digits
    =/  num  |=(dig=tape (little 10 (decimal dig)))
    =|  lod=tape
    |-  ^-  [lo=@ hi=@]
    ?@  t  [(num lod) 0]
    ?.  =(',' i.t)
      $(t t.t, lod [i.t lod])
    =/  [hid=tape lot=tape]  [~ t.t]
    |-  ^-  [@ @]
    ?^  lot
      $(lot t.lot, hid [i.lot hid])
    [(num lod) (num hid)]
  ++  clas
    |=  t=tape
    ^-  chaz
    =.  t  ((unescape ']') t)
    =|  out=mult
    =-  ~(nom mu -)
    |-  ^-  mult
    ?-  t
      ~
        out
      [* %'-' ^]
        $(t t.t.t.t, out (~(uni mu out) (mult-range &1.t &3.t)))
      *
        $(t t.t, out (~(put mu out) i.t))
    ==
  ++  cord-to-torx
    |=  txt=cord
    ^-  torx
    =+  ~(expr te (init-cord-cursor txt) ~)
    ?@  -  ~_  txt  !!
    ?.  ?|  =(i.cur.s len.cur.s) :: fully consumed
            ::  or trailing whitespace only
            =/  lux  (tore-lex cur.s)
            &(?=(^ lux) =(%ws tag.lux) =(i.cur.lux len.cur.lux))
        ==
      ~&  parsed=u
      ~_  (rsh [3 i.cur.s] txt.cur.s)  !!
    u
  ++  unescape
    |=  ext=@
    =/  m  %-  ~(gas by *(map @ @))
      :~  :-  %r    13
          :-  %n    10
          :-  %t    9
          :-  '\\'  '\\'
          :-  ext   ext
      ==
    |=(t=tape (bsfs t m))
  ++  te
    |_  st=[cur=cord-cursor buf=$@(~ tort)]
    ++  chunk
      |=  [beg=@ len=@]
      (cut 3 +< txt.cur.st)
    ++  read  :: read a token (if possible) from the stream
      ^-  $@(~ [m=tort cur=_cur.st])
      =/  r  (tore-lex cur.st)
      ?@  r  ~
      =/  len  (sub i.cur.r i.cur.st)
      ?+  tag.r  !!
        %one   :_  cur.r  one+(chunk i.cur.st 1)
        %id    :_  cur.r  id+(chunk i.cur.st len)
        %ws    read(cur.st cur.r)  :: skip whitespace
        %rep   :_  cur.r
               rep+(rep (trip (chunk +(i.cur.st) (sub len 2))))
        %num   :_  cur.r  :-  %num  %+  big:digits  10
               (decimal:digits (trip (chunk i.cur.st len)))
        %quot  :_  cur.r
               :-  %quot
               %-  (unescape '\'')
               (trip (chunk +(i.cur.st) (sub len 2)))
        %clas  =+  :-  `t=tape`(trip (chunk +(i.cur.st) (sub len 2)))
                   `pos=?`&
               =?  -  ?=([%'^' *] t)  [t.t |]
               :_  cur.r  [%cc pos (clas t)]
      ==
    ++  fill  :: fill the buffer, if possible
      ^+  st
      ?^  buf.st  st
      =/  r  read
      ?@  r  st
      st(cur cur.r, buf m.r)
    ++  move :: unconditionally drop the current token
      ^+  st
      =.  st  fill
      st(buf ~)
    ++  peek :: view the current token
      ^-  (mandatory tort _st)
      =.  st  fill
      ?@  buf.st  ~
      [buf.st st]
    :: Expr -> Mod Tail
    :: Mod  -> '!' Prim | Prim Suf
    :: Prim -> cc | id | quot | ( Expr )
    :: Suf  -> '*' | '+' | '?' | Empty
    :: Tail -> '&' Expr | '|' Expr | Expr | Empty
    ++  expr
      ^-  (mandatory torx _st)
      =+  mod  ?@  -  ~  =>  [e=u +(st s)]
      =^  f  st  tail
      :_  st  ?@(f e (u.f e))
    ++  mod
      ^-  (mandatory torx _st)
      =+  peek  ?@  -  ~  =>  [t=u +(st s)]
      ?:  =(one+'!' t)
        =+  prim(st move)  ?@  -  ~
        [zap+u s]
      =+  prim  ?@  -  ~  =>  [e=u +(st s)]
      =^  f  st  suf
      :_  st  ?@(f e (u.f e))
    ++  prim
      ^-  (mandatory torx _st)
      =+  peek  ?@  -  ~  =>  [t=u +(st move(st s))]
      ?+  t  ~
        [%cc *]    [cc+p.t st]
        [%id *]    [id+p.t st]
        [%quot *]  [quot+txt.t st]
        [%one %'(']
          =+  expr  ?@  -  ~  =>  [e=u +(st s)]
          =+  peek  ?@  -  ~  =>  [t=u +(st s)]
          ?.  =(t [%one ')'])  ~
          [e move]
      ==
    ++  suf
      ^-  (nullable-c $-(torx torx) _st)
      =+  peek  ?@  -  `st  =>  [t=u +(st s)]
      ?+  t  `st
        [%rep *]     :_  move  |=(m=torx [%rep lo.t hi.t m])
        [%one %'*']  :_  move  |=(m=torx tar+m)
        [%one %'+']  :_  move  |=(m=torx lus+m)
        [%one %'?']  :_  move  |=(m=torx wut+m)
      ==
    ++  tail
      ^-  (nullable-c $-(torx torx) _st)
      =+  peek  ?@  -  `st  =>  [t=u +(st s)]
      ?+  t          =+  expr           ?@  -  `st  =>  [e=u +(st s)]
                     :_  st  |=(m=torx [%seq m e])
        [%one %'&']  =+  expr(st move)  ?@  -  `st  =>  [e=u +(st s)]
                     :_  st  |=(m=torx [%pam m e])
        [%one %'|']  =+  expr(st move)  ?@  -  `st  =>  [e=u +(st s)]
                     :_  st  |=(m=torx [%bar m e])
      ==
    --
  ++  de
    |_  d=(map term regx)
    ++  torx-to-regx
      |=  m=torx
      ^-  regx
      ?-  -.m
        %cc    [%set p.m]
        %quot  (re-cat (turn txt.m |=(c=@ [%set & c])))
        %id    ~|('torx-id' (~(got by d) p.m))
        %zap   (re-not $(m e.m))
        %tar   (re-star $(m e.m))
        %lus   (re-plus $(m e.m))
        %wut   (re-opt $(m e.m))
        %rep   =/  in  $(m e.m)
               =/  out
                 =|  [n=@ out=(list regx)]
                 |-  ^+  out
                 ?:  =(n lo.m)  out
                 $(n +(n), out [in out])
               ?:  (lte hi.m lo.m)  (re-cat out)
               =/  oin=regx  (re-opt in)
               |-  ^-  regx
               ?:  =(lo.m hi.m)  (re-cat (flop out))
               $(lo.m +(lo.m), out [oin out])
        %seq   =|  out=(list regx)
               |-  ^-  regx
               ?:  ?=(%seq -.q.m)
                 $(m q.m, out [^$(m p.m) out])
               ::  order matters for cat (but not &/|)
               (re-cat (flop ^$(m q.m) ^$(m p.m) out))
        %pam   =|  out=(list regx)
               |-  ^-  regx
               ?:  ?=(%pam -.q.m)
                 $(m q.m, out [^$(m p.m) out])
               (re-and ^$(m q.m) ^$(m p.m) out)
        %bar   =|  out=(list regx)
               |-  ^-  regx
               ?:  ?=(%bar -.q.m)
                 $(m q.m, out [^$(m p.m) out])
               (re-or ^$(m q.m) ^$(m p.m) out)
      ==
    ++  cord-to-regx
      |=  c=cord
      ^-  regx
      (torx-to-regx (cord-to-torx c))
    ++  def-rule
      |=  [n=term m=torx]
      ^+  d
      (~(put by d) n (torx-to-regx m))
    ++  def-cord
      |=  [n=term c=cord]
      ^+  d
      (def-rule n (cord-to-torx c))
    ++  def-cords
      |=  [ruls=(list (pair term cord))]
      ^+  d
      ?@  ruls  d
      $(ruls t.ruls, d (def-cord i.ruls))
    ++  select-rules
      |=  ns=(list $@(term [nam=term tag=*]))
      %-  token-map
      %+  turn  ns
      |=  n=$@(term [nam=term tag=*])
      ?@  n  [n (~(got by d) n)]
      [tag.n (~(got by d) nam.n)]
    --
  --
--
:::::::::::::::::::::::::::::::::::::::::
::  prototype runic language (runt)
:::::::::::::::::::::::::::::::::::::::::
|%
++  runt
  ::  untyped, runic thin-skin for nock
  =>  |%
      +$  reg
        $?  %cltr  %dtfs  %dtsq  %dttr  %dtwt  %dtls  %dtts
            %wtcl  %tsgr  %tsls  %dtcn  %dtbr  %dthx  %sgpt  %sgkt
        ==
      +$  toke
        ::  note that tall form atoms are in the representation
        ::  even though we don't have such syntax yet
        ::  "real" hoon does have them, though it doesn't enforce
        ::  tall/wide for them as it should.
        $@  ?(%pel %per %sel %ser %gap %ace %fas %stet %ilus %itis)
        $?  [%atom tol=? a=@]
            [reg tol=?]
        ==
      +$  post  [row=@ col=@]
      +$  expr
        $~  [%dtfs 0]
        $%  [%cltr p=expr q=expr rev=(list expr)]
            [%dtfs axe=@]
            [%dtsq val=*]
            [%dtbr p=expr] :: compile and quote
            [%dttr bus=expr fol=expr]
            [%dtwt p=expr]
            [%dtls p=expr]
            [%dtts p=expr q=expr]
            [%wtcl t=expr y=expr n=expr]
            [%tsgr p=expr q=expr]
            [%tsls p=expr q=expr]
            [%dtcn axe=@ cor=expr]
            [%dthx axe=@ val=expr big=expr]
            [%sgpt tag=@ p=expr]
            [%sgkt tag=@ clu=expr p=expr]
        ==
      --
  |%
  ++  gate
    =+  [vec tag]=def
    =/  fol=nock
      (cord-nock (vector-dfa vec) (pick-first-mapped tag))
    =/  levi  |=(* &)
    !<  (tokenize-gate cord-cursor)
    `|=(bus=cord-cursor .*(bus fol))
  ++  unescape
    =/  m  %-  ~(gas by *(map @ @))
      :~  :-  '\''  '\''
          :-  '\\'  '\\'
      ==
    |=(t=tape (bsfs t m))
  ++  def
    =/  rul  de:torp
    %.  ~[%lark %ud %cord %gap %one %ipfx %wide %tall %stet]
    %~  select-rules  rul
    %-  ~(def-cords rul ~)  :~
      :-  %one
      '''
      [ /()[\]]
      '''
      :-  %lark  '[+-] ([<>] [+-])* [<>]?'
      :-  %ud
          '''
          '0' | ( [1-9] [0-9]{0,3} ( '.' [1-9] [0-9]{3} )* )
          '''
      :-  %cord
          '''
          '\''
          ( !'\'' &
            ( [^\\] |
              ( '\\' ( ( [0-9]+ '/' ) |
                       [\\'] ) ) ) )*
          '\''
          '''
      :-  %comment
          '''
          '::' [^\n]* '\n'
          '''
      :-  %gap
          '''
          ( comment | [\r\n\t] | '  ' )
          ( comment | [\r\n\t ] )*
          '''
      :-  %rune  :: ./ for %0
                 :: .' for %1
                 :: .% for %9
                 :: .| for battery
          '''
          ':*' | './'  | '.\'' | '.*' | '.?' | '.+' | '.=' |
          '?:' | '=>'  | '=+'  | '.%' | '.|' | '.#' | '~@' | '~^'
          '''
      :-  %ipfx
          '''
          '=(' | '+('
          '''
      :-  %wide  'rune [(]'
      :-  %tall  'rune gap'
      :-  %stet
          '''
          '=='
          '''
    ==
  ++  pe  :: modeled after te, but different (hard to abstract)
    :: differently structured: build the lexer just-in-time
    :: you really don't want to call this arm repeatedly!
    ::~&  'runt'
    ::~>  %slog.[2 (machine:mump (vector-dfa -:def))]
    =/  lex  gate
    |_  $:  tol=?  :: tall-mode
            $=  st
            $:  cur=cord-cursor
                rap=_`post`[1 1] :: raw position (ignoring buffering)
                buf=$@(~ [p=post t=toke])
        ==  ==
    ++  chunk
      |=  [beg=@ len=@]
      (cut 3 +< txt.cur.st)
    ++  fill
      ^+  st
      ?^  buf.st  st
      =/  r  read
      ?@  r  st
      st(cur cur.r, rap pos.r, buf [rap.st t.r])
    ++  move
      ^+  st
      =.  st  fill
      st(buf ~)
    ++  peek
      ^-  (mandatory toke _st)
      =.  st  fill
      ?@  buf.st  ~
      [t.buf.st st]
    ++  gulp
      ^-  (mandatory toke _st)
      =+  peek  ?@  -  ~
      [u move(st s)]
    ++  read
      ^-  $@(~ [t=toke pos=post cur=cord-cursor])
      =/  r  (lex cur.st)
      ?@  r  ~
      =/  p  (proc tag.r i.cur.st i.cur.r)
      ?@  p  ~  ::  for example, tall token in wide form
      [t.p p.p cur.r]
    ++  here  `post`?@(buf.st rap.st p.buf.st)
    ++  gape
      |=  [beg=@ end=@ p=post]
      %+  fold-bytes  [txt.cur.st beg end]
      |=  [c=@ p=_p]  ^+  p
      ?:  =(10 c)
        p(row +(row.p), col 1)
      p(col +(col.p))
    ++  runk
      |=  [tol=? tag=@]
      ^-  toke
      :_  tol
      ?+  tag   !!
        %':*'   %cltr
        %'./'   %dtfs
        %'.\''  %dtsq
        %'.|'   %dtbr
        %'.*'   %dttr
        %'.?'   %dtwt
        %'.+'   %dtls
        %'.='   %dtts
        %'?:'   %wtcl
        %'=>'   %tsgr
        %'=+'   %tsls
        %'.%'   %dtcn
        %'.#'   %dthx
        %'~@'   %sgpt
        %'~^'   %sgkt
      ==
    ++  proc
      |=  [tag=* beg=@ end=@]
      ^-  $@(~ [t=toke p=post])
      ?:  =(%gap tag)
        ?.  tol  ~
        gap+(gape beg end rap.st)
      ?:  =(%stet tag)
        ?.  tol  ~
        stet+rap.st(col +(+(col.rap.st)))
      ?:  =(%tall tag)
        ?.  tol  ~
        :-  (runk & (chunk beg 2))
        (gape +(+(beg)) end rap.st(col +(+(col.rap.st))))
      ?:  =(%wide tag)
        :-  (runk | (chunk beg 2))
        rap.st(col +(+(+(col.rap.st))))
      ?:  =(%ipfx tag)
        :_  rap.st(col +(+(col.rap.st)))
        ?+  (chunk beg 1)  !!
          %'+'  %ilus
          %'='  %itis
        ==
      ::  if there were any tall form atoms, we would have to
      ::  have to deal with their internal gaps here
      =/  len  (sub end beg)
      :_  rap.st(col (add len col.rap.st))
      ^-  toke
      ?+  tag  !!
        %lark  :+  %atom  |
               %+  fold-bytes  [txt.cur.st beg end]
               |=  [c=@ acc=_1]
               %+  peg  acc
               ?+  c  !!
                 %'-'  %2
                 %'+'  %3
                 %'<'  %2
                 %'>'  %3
               ==
        %ud    :+  %atom  |  %+  big:digits  10
               %-  decimal:digits
               (skip (trip (chunk i.cur.st len)) |=(c=@ =('.' c)))
        %cord  :+  %atom  |
               (crip (unescape (trip (chunk +(i.cur.st) (sub len 2)))))
        %one   ?+  (chunk beg 1)  !!
                 %' '  %ace
                 %'('  %pel
                 %')'  %per
                 %'['  %sel
                 %']'  %ser
                 %'/'  %fas
      ==       ==
    ++  expect
      |=  t=toke
      ^-  $@(~ s=_st)
      =+  gulp  ?@  -  ~
      ?:(=(u t) s ~)
    ++  expect-atom
      ^-  (mandatory @ _st)
      =+  peek  ?@  -  ~
      ?.  ?=([%atom *] u)  ~
      [a.u move(st s)]
    ++  two-plus
      |*  m=mold
      |=  [sep=toke one=$-(_st (mandatory m _st))]
      ^-  (mandatory [p=m q=m rev=(list m)] _st)
      =+  (one st)      ?@  -  ~  =>  [p=u +(st s)]
      =+  (expect sep)  ?@  -  ~  =>  +(st s)
      =+  (one st)      ?@  -  ~  =>  [q=u +(st s)]
      =^  rev  st
        =|  a=(list m)
        |-  ^+  [a st]
        =+  peek  ?@  -  [a st]  ?.  =(sep u)  [a s]
        =+  (one move(st s))  ?@  -  [a s]
        $(a [u a], st s)
      [[p q rev] st]
    ++  expr2p
      |=  [sep=toke exp=$-(_st (mandatory expr _st))]
      ^-  (mandatory [p=expr q=expr mor=(list expr)] _st)
      =+  ((two-plus expr) +<)  ?@  -  ~  =>  [u +(st s)]
      [[p q rev] st]
    ++  expr2p-w  (expr2p %ace |=(s=_st wide(st s)))
    ++  wide-close
      ^-  $@(~ s=_st)
      =+  (expect %per)
      ?@(- ~ s)
    ++  tall-close
      ^-  $@(~ s=_st)
      =+  (expect %gap)   ?@  -  ~  =>  +(st s)
      =+  (expect %stet)  ?@  -  ~  s
    ++  wide-1
      ^-  (mandatory expr _st)
      =+  wide        ?@  -  ~  =>  [one=u +(st s)]
      =+  wide-close  ?@  -  ~  [one s]
    ++  wide-2
      ^-  (mandatory [expr expr] _st)
      =+  wide           ?@  -  ~  =>  [one=u +(st s)]
      =+  (expect %ace)  ?@  -  ~  =>  +(st s)
      =+  wide-1         ?@  -  ~  [[one u] s]
    ++  tall-2
      ^-  (mandatory [expr expr] _st)
      =+  tall           ?@  -  ~  =>  [one=u +(st s)]
      =+  (expect %gap)  ?@  -  ~  =>  +(st s)
      =+  tall           ?@  -  ~  [[one u] s]
    ++  dtls-w
      ^-  (mandatory expr _st)
      =+  wide-1  ?@  -  ~  [[%dtls u] s]
    ++  dtts-w
      ^-  (mandatory expr _st)
      =+  wide-2  ?@  -  ~  [[%dtts u] s]
    ++  wide  ::  pel is part of the opening token
      ^-  (mandatory expr _st)
      =+  gulp  ?@  -  ~  =>  [t=u +(st s)]
      ?+  t  ~
        %sel          =+  expr2p-w       ?@  -  ~  =>  [es=u +(st s)]
                      =+  (expect %ser)  ?@  -  ~  [[%cltr es] s]
                      ::  awkwardly post-process es to make a big %dtsq?
        %fas          =+  expect-atom    ?@  -  ~  [[%dtfs u] s]
        %ilus         dtls-w
        %itis         dtts-w
        [%cltr %|]    =+  expr2p-w       ?@  -  ~  =>  [es=u +(st s)]
                      =+  wide-close     ?@  -  ~  [[%cltr es] s]
        [%atom %| *]  [%dtsq a.t]^st
        [%dtfs %|]    =+  expect-atom    ?@  -  ~  =>  [a=u +(st s)]
                      =+  wide-close     ?@  -  ~  [[%dtfs a] s]
        [%dtsq %|]    =+  lit-w          ?@  -  ~  =>  [v=u +(st s)]
                      =+  wide-close     ?@  -  ~  [[%dtsq v] s]
        [%dttr %|]    =+  wide-2         ?@  -  ~  [[%dttr u] s]
        [%dtwt %|]    dtls-w
        [%dtls %|]    =+  wide-1         ?@  -  ~  [[%dtls u] s]
        [%dtts %|]    dtts-w
        [%wtcl %|]    =+  wide           ?@  -  ~  =>  [t=u +(st s)]
                      =+  (expect %ace)  ?@  -  ~  =>  +(st s)
                      =+  wide-2         ?@  -  ~  [[%wtcl t u] s]
        [%tsgr %|]    =+  wide-2         ?@  -  ~  [[%tsgr u] s]
        [%tsls %|]    =+  wide-2         ?@  -  ~  [[%tsls u] s]
        [%dtcn %|]    =+  expect-atom    ?@  -  ~  =>  [a=u +(st s)]
                      =+  (expect %ace)  ?@  -  ~  =>  +(st s)
                      =+  wide-1         ?@  -  ~  [[%dtcn a u] s]
        [%dtbr %|]    =+  wide-1         ?@  -  ~  [[%dtbr u] s]
        [%dthx %|]    =+  expect-atom    ?@  -  ~  =>  [a=u +(st s)]
                      =+  (expect %ace)  ?@  -  ~  =>  +(st s)
                      =+  wide-2         ?@  -  ~  [[%dthx a u] s]
        [%sgpt %|]    =+  expect-atom    ?@  -  ~  =>  [a=u +(st s)]
                      =+  (expect %ace)  ?@  -  ~  =>  +(st s)
                      =+  wide-1         ?@  -  ~  [[%sgpt a u] s]
        [%sgkt %|]    =+  expect-atom    ?@  -  ~  =>  [a=u +(st s)]
                      =+  (expect %ace)  ?@  -  ~  =>  +(st s)
                      =+  wide-2         ?@  -  ~  [[%sgkt a u] s]
      ==
    ++  tall  ::  gap is part of the opening token
      ^-  (mandatory expr _st)
      =+  peek  ?@  -  ~  =>  [t=u +(st s)]
      ?+  t  wide(tol |)
        [%atom *]   [%dtsq a.t]^move
        [%cltr %&]  =+  (expr2p(st move) %gap |=(s=_st tall(st s)))
                    ?@  -  ~  =>  [in=u +(st s)]
                    =+  tall-close  ?@  -  ~  [[%cltr in] s]
        [%dtfs %&]  =+  expect-atom(st move)  ?@  -  ~  [[%dtfs u] s]
        [%dtsq %&]  =+  lit-t(st move)        ?@  -  ~  [[%dtsq u] s]
        [%dttr %&]  =+  tall-2(st move)       ?@  -  ~  [[%dttr u] s]
        [%dtwt %&]  =+  tall(st move)         ?@  -  ~  [[%dtwt u] s]
        [%dtls %&]  =+  tall(st move)         ?@  -  ~  [[%dtls u] s]
        [%dtts %&]  =+  tall-2(st move)       ?@  -  ~  [[%dtts u] s]
        [%wtcl %&]  =+  tall(st move)         ?@  -  ~  =>  [t=u +(st s)]
                    =+  (expect %gap)         ?@  -  ~  =>  +(st s)
                    =+  tall-2                ?@  -  ~  [[%wtcl t u] s]
        [%tsgr %&]  =+  tall-2(st move)       ?@  -  ~  [[%tsgr u] s]
        [%tsls %&]  =+  tall-2(st move)       ?@  -  ~  [[%tsls u] s]
        [%dtcn %&]  =+  expect-atom(st move)  ?@  -  ~  =>  [a=u +(st s)]
                    =+  (expect %gap)         ?@  -  ~  =>  +(st s)
                    =+  tall                  ?@  -  ~  [[%dtcn a u] s]
        [%dtbr %&]  =+  tall(st move)         ?@  -  ~  [[%dtbr u] s]
        [%dthx %&]  =+  expect-atom(st move)  ?@  -  ~  =>  [a=u +(st s)]
                    =+  (expect %gap)         ?@  -  ~  =>  +(st s)
                    =+  tall-2                ?@  -  ~  [[%dthx a u] s]
        [%sgpt %&]  =+  expect-atom(st move)  ?@  -  ~  =>  [a=u +(st s)]
                    =+  (expect %gap)         ?@  -  ~  =>  +(st s)
                    =+  tall                  ?@  -  ~  [[%sgpt a u] s]
        [%sgkt %&]  =+  expect-atom(st move)  ?@  -  ~  =>  [a=u +(st s)]
                    =+  (expect %gap)         ?@  -  ~  =>  +(st s)
                    =+  tall-2                ?@  -  ~  [[%sgkt a u] s]
      ==
    ++  lit2p
      |=  [sep=toke nun=$-(_st (mandatory * _st))]
      ^-  (mandatory ^ _st)
      =+  ((two-plus *) +<)  ?@  -  ~  =>  [u +(st s)]
      :_  st
      ?@  rev  [p q]
      [p q (roll t.rev |=([i=* a=_i.rev] +<))]
    ++  lit2p-w  (lit2p %ace |=(s=_st lit-w(st s)))
    ++  lit-w
      ^-  (mandatory * _st)
      =+  gulp  ?@  -  ~  =>  [t=u +(st s)]
      ?+  t  ~
        %sel          =+  lit2p-w        ?@  -  ~  =>  [cel=u +(st s)]
                      =+  (expect %ser)  ?@  -  ~  [cel -]
        [%atom %| *]  a.t^st
        [%cltr %|]    =+  lit2p-w        ?@  -  ~  =>  [cel=u +(st s)]
                      =+  (expect %per)  ?@  -  ~  [cel -]
      ==
    ++  lit-t
      ^-  (mandatory * _st)
      =+  peek  ?@  -  ~  =>  [t=u +(st s)]
      ?+  t  lit-w(tol |)
        [%atom *]   a.t^move  :: in theory, atoms can be tall
        [%cltr %&]  =+  (lit2p(st move) %gap |=(s=_st lit-t(st s)))
                    ?@  -  ~  =>  [cel=u +(st s)]
                    =+  tall-close  ?@  -  ~  [cel s]
      ==
    --
    ++  expr-to-nock
      |=  e=expr
      ^-  nock
      ?-  -.e
        %cltr  =/  p  $(e p.e)
               =/  q  $(e q.e)
               =/  r  (turn rev.e ..$)
               ?@  r  [p q]
               :+  p  q
               (roll t.r |=([i=nock a=_i.r] +<))
        %dtfs  [%0 axe.e]
        %dtsq  [%1 val.e]
        %dtbr  [%1 $(e p.e)]
        %dttr  [%2 $(e bus.e) $(e fol.e)]
        %dtwt  [%3 $(e p.e)]
        %dtls  [%4 $(e p.e)]
        %dtts  [%5 $(e p.e) $(e q.e)]
        %wtcl  [%6 $(e t.e) $(e y.e) $(e n.e)]
        %tsgr  [%7 $(e p.e) $(e q.e)]
        %tsls  [%8 $(e p.e) $(e q.e)]
        %dtcn  [%9 axe.e $(e cor.e)]
        %dthx  [%10 [axe.e $(e val.e)] $(e big.e)]
        %sgpt  [%11 tag.e $(e p.e)]
        %sgkt  [%11 [tag.e $(e clu.e)] $(e p.e)]
      ==
  --
--
