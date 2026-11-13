::  rd-vast: the hoon parser by recursive descent, first-order
::
::    entry points, shaped like the stock ones in hoon.hoon:
::
::      (ream txt)      a file's text to hoon, as ^ream
::      (rain bon txt)  the same with a path for % forms, as ^rain
::      (vice txt)      one wide-form hoon, as ^vice
::
::    each runs the recursive-descent parser in +rd first and hands the
::    text to the stock parser when that parser declines the input or
::    finds no parse, so a syntax error is reported by the stock parser.
::
::      (file bug wer txt)  as ream, raw: 0 no parse, 1 declined, or hoon
::      (line bug wer txt)  as (rush txt wide:vast), raw
::
::    +rd is a door over vast's sample (bug for !: spots, wer for % paths)
::    and the place of a tape in the text.  the text is taken line by
::    line: an arm on a +sat (tall forms) can cross lines, an arm on a
::    tape (wide forms) first gets the rest of one line.  every arm
::    returns [product rest] or an atom: 0 where the stock rule finds no
::    parse, 1 where this parser declines, 2 where a wide arm needs the
::    following lines or its place in the text; its caller on a +sat then
::    runs it again on a tape of more lines.  a choice goes on to its next
::    alternative on 0 only.  no rule is built and no gate is passed.
::    what is left to hoon.hoon: the rarer atom syntaxes (rules of +so),
::    path interpolation (posh:vast), tape lists (phax:vast), flay,
::    autoname, and the markdown of sail (cram:vast, for ;> and the text
::    children of a tall tag).  the stock rules run under mule; where
::    one crashes, this parser declines.
::
|%
++  ream                                                ::  parse cord to hoon
  |=  txt=@
  ^-  hoon
  =/  r  (file | ~ txt)
  ?@  r  (^ream txt)
  r
::
++  rain                                                ::  parse with % path
  |=  [bon=path txt=@]
  ^-  hoon
  =/  r  (file | bon txt)
  ?@  r  (^rain bon txt)
  r
::
++  vice                                                ::  parse a wide hoon
  |=  txt=@ta
  ^-  hoon
  =/  r  (line | ~ txt)
  ?@  r  (^vice txt)
  r
::
++  file                                                ::  0, 1 or a file's hoon
  |=  [bug=? wer=path txt=@]
  ^-  $@(@ hoon)
  (~(apex rd bug wer 0 0 ~ 0 %line &) (rowt (trip txt)))
::
++  line                                                ::  0, 1 or a wide hoon
  |=  [bug=? wer=path txt=@]
  ^-  $@(@ hoon)
  =/  tap  (trip txt)
  =/  r  (~(wide rd bug wer 1 (lent tap) tap 0 %eof &) tap)
  ?@  r  ?:(=(0 r) 0 1)
  ?^  q.r  0
  p.r
::
++  rowt                                                ::  a tape by lines,
  |=  tub=tape                                          ::  each with its length
  ^-  (lest [len=@ud tap=tape])
  =|  acc=(list [len=@ud tap=tape])
  |-  ^-  (lest [len=@ud tap=tape])
  =/  i  (find "\0a" tub)
  ?~  i
    =/  l  (flop `(list [len=@ud tap=tape])`[[(lent tub) tub] acc])
    ?>(?=(^ l) l)
  $(acc [[u.i (scag u.i tub)] acc], tub (slag +(u.i) tub))
::
++  rd
  |_  $:  bug=?                                         ::  spots are on
          wer=path                                      ::  path for % forms
          lin=@ud                                       ::  line of the tape's
          len=@ud                                       ::  start, 0 if unknown;
          all=tape                                      ::  %line: len of line.
          beg=@ud                                       ::  else: the joined
          end=?(%line %more %eof)                       ::  tape, its length, the
          fin=?                                         ::  column before it,
      ==                                                ::  and what its end is.
                                                        ::  fin: a sat ends the
                                                        ::  text
  +$  res  $@(@ [p=hoon q=tape])                    ::  hoon and rest
  +$  rez  $@(@ [p=spec q=tape])                    ::  spec and rest
  +$  rew  $@(@ [p=wing q=tape])                    ::  wing and rest
  +$  rea  $@(@ [p=@ q=tape])                       ::  atom and rest
  +$  rel  $@(@ [p=[i=hoon t=(list hoon)] q=tape])  ::  hoons and rest
  +$  rem  $@(@ [p=[i=spec t=(list spec)] q=tape])  ::  specs and rest
  +$  rek  $@(@ [p=(list (pair wing hoon)) q=tape]) ::  changes and rest
  ::                                                ::  ++wide
  ++  wide                                          ::  hoon, wide form
    |=  tub=tape
    ^-  res
    ?.  bug  (wode tub)
    ?:  =(0 lin)  2
    =/  r  (wode tub)
    ?@  r  r
    [[%dbug [wer (hair tub) (hair q.r)] p.r] q.r]
  ::                                                ::  ++wode
  ++  wode                                          ::  wide, no spot
    |=  tub=tape
    ^-  res
    ?~  tub  0
    =/  nex  t.tub
    =/  r=res
      ?.  ?=([@ %'(' *] nex)  0
      (rune i.tub i.nex t.t.nex)
    ?.  =(0 r)  r
    =/  r  (long tub)
    ?.  =(0 r)  r
    ?.  =(';' i.tub)  0
    (saiw nex)
  ::                                                ::  ++long
  ++  long                                          ::  scat and a suffix
    |=  tub=tape
    ^-  res
    =/  r  (scat tub)
    ?@  r  r
    =/  nex  q.r
    ?~  nex  r
    ?+    i.nex  r
        %'='
      =/  s  (wide t.nex)
      ?@  s  ?:(=(0 s) r s)
      =/  rud  ~(flay ap p.r)
      ?~  rud  r
      [[%ktts u.rud p.s] q.s]
    ::
        %':'
      =/  s  (wide t.nex)
      ?@  s  ?:(=(0 s) r s)
      ?:  =([%base %flag] p.r)  r
      [[%tsgl p.r p.s] q.s]
    ::
        %'^'
      =/  s  (wide t.nex)
      ?@  s  ?:(=(0 s) r s)
      [[p.r p.s] q.s]
    ::
        %'('
      =/  s  (rick ')' t.nex)
      ?@  s  ?:(=(0 s) r s)
      =/  hyp  (reek p.r)
      ?~  hyp  r
      [[%cnts u.hyp p.s] q.s]
    ==
  ::                                                ::  ++reek
  ++  reek                                          ::  hoon as a wing
    |=  gen=hoon
    ^-  (unit wing)
    ?+  gen  ~
      [~ *]        [~ [%& p.gen] ~]
      [%limb *]    [~ p.gen ~]
      [%wing *]    [~ p.gen]
      [%cnts * ~]  [~ p.gen]
      [%dbug *]    $(gen q.gen)
    ==
  ::                                                ::  ++scat
  ++  scat                                          ::  irregular forms
    |=  tub=tape
    ^-  res
    ?~  tub  0
    =/  c  i.tub
    =/  nex  t.tub
    ?:  &((gte c 'a') (lte c 'z'))  (rump tub)
    ?+    c  0
        %'('
      =/  r  (hank ')' nex)
      ?@  r  r
      [[%cncl p.r] q.r]
    ::
        %'['  (rupl | nex)
        %'%'
      ?~  nex  (pers 0 ~)
      =/  d  i.nex
      ?:  &((gte d 'a') (lte d 'z'))
        =/  s  (sym nex)
        [[%rock %tas p.s] q.s]
      ?:  &((gte d '0') (lte d '9'))
        =/  s  (bisk nex)
        ?@  s  s
        [[%rock p.s] q.s]
      ?+    d  (pers 0 nex)
          %'$'  [[%rock %tas %$] t.nex]
          %'&'  [[%rock %f &] t.nex]
          %'|'  [[%rock %f |] t.nex]
          %'\''
        =/  s  (qut t.nex)
        ?@  s  ?:(=(0 s) (pers 0 nex) s)
        [[%rock %t p.s] q.s]
      ::
          %'/'
        =/  r  (porc 0 t.nex)
        ?.  =(0 r)  r
        (pers 0 nex)
      ::
          %'%'
        =/  n=@  1
        =/  nix  t.nex
        |-  ^-  res
        ?:  ?=([%'%' *] nix)  $(n +(n), nix t.nix)
        =/  r=res  ?.(?=([%'/' *] nix) 0 (porc n t.nix))
        ?.  =(0 r)  r
        (pers n nix)
      ::
          ?(%'.' %'~' %'-')
        =/  c  (nuck nex)
        ?@  c  ?:(=(0 c) (pers 0 nex) c)
        [((jock &) p.c) q.c]
      ==
    ::
        %'~'
      ?~  nex  [[%bust %null] ~]
      =/  d  i.nex
      ?:  =('[' d)
        =/  r  (rupl & t.nex)
        ?.  =(0 r)  r
        [[%bust %null] nex]
      ?:  =('(' d)
        =/  r  (cnsg t.nex)
        ?.  =(0 r)  r
        [[%bust %null] nex]
      =/  c=$@(@ [p=coin q=tape])
        ?.  (twid nex)  0
        =/  mex  (mule |.((twid:so [[1 1] nex])))
        ?.  ?=(%& -.mex)  1
        =/  vex  p.mex
        ?~  q.vex  0
        ?:  (gapy q.q.u.q.vex)  2
        [p.u.q.vex q.q.u.q.vex]
      ?^  c  [((jock |) p.c) q.c]
      ?.  =(0 c)  c
      ?.  |(=('+' d) =('/' d))  [[%bust %null] nex]
      =/  r  (wide t.nex)
      ?@  r  ?:(=(0 r) [[%bust %null] nex] r)
      [[[%bust %null] p.r] q.r]
    ::
        %'\''
      =/  s  (qut nex)
      ?@  s  s
      [[%sand %t p.s] q.s]
    ::
        %'"'
      =/  s  (sols 0 nex)
      ?@  s  s
      [[%knit (knit p.s)] q.s]
    ::
        %'+'
      ?:  ?=([%'"' *] nex)
        =/  s  (sols '+' t.nex)
        ?^  s  [[%mcfs %knit (knit p.s)] q.s]
        ?.  =(0 s)  s
        (cnts tub)
      ?.  ?=([%'(' *] nex)  (cnts tub)
      =/  r  (expa t.nex)
      ?@  r  ?:(=(0 r) (cnts tub) r)
      [[%dtls p.r] q.r]
    ::
        %'-'
      ?~  nex  (cnts tub)
      =/  d  i.nex
      ?:  =('"' d)
        =/  s  (sols '-' t.nex)
        ?^  s  [[%clsg (phax:vast p.s)] q.s]
        ?.  =(0 s)  s
        (cnts tub)
      ?:  &((gte d '0') (lte d '9'))
        =/  s  (bisk nex)
        ?@  s  s
        :_  q.s
        [%sand (sign -.p.s) ?:(=(0 +.p.s) 0 +((mul 2 (dec +.p.s))))]
      ?.  ?=([%'-' @ *] nex)  (cnts tub)
      =/  e  i.t.nex
      ?.  &((gte e '0') (lte e '9'))  (cnts tub)
      =/  s  (bisk t.nex)
      ?@  s  s
      [[%sand (sign -.p.s) (mul 2 +.p.s)] q.s]
    ::
        %'='
      ?:  ?=([%'(' *] nex)
        =/  r  (expb t.nex)
        ?@  r  ?:(=(0 r) (tisk nex) r)
        [[%dtts p.r] q.r]
      (tisk nex)
    ::
        %'&'
      ?~  nex  [[%sand %f &] ~]
      =/  d  i.nex
      ?:  &((gte d '0') (lte d '9'))  (cnts tub)
      ?:  =('(' d)
        =/  r  (hank ')' t.nex)
        ?@  r  ?:(=(0 r) [[%sand %f &] nex] r)
        [[%wtpm p.r] q.r]
      ?.  |(=('+' d) =('/' d))  [[%sand %f &] nex]
      =/  r  (wide t.nex)
      ?@  r  ?:(=(0 r) [[%sand %f &] nex] r)
      [[[%rock %f &] p.r] q.r]
    ::
        %'|'
      ?~  nex  [[%sand %f |] ~]
      =/  d  i.nex
      ?:  &((gte d '0') (lte d '9'))  (cnts tub)
      ?:  =('(' d)
        =/  r  (hank ')' t.nex)
        ?@  r  ?:(=(0 r) [[%sand %f |] nex] r)
        [[%wtbr p.r] q.r]
      ?.  |(=('+' d) =('/' d))  [[%sand %f |] nex]
      =/  r  (wide t.nex)
      ?@  r  ?:(=(0 r) [[%sand %f |] nex] r)
      [[[%rock %f |] p.r] q.r]
    ::
        %'.'
      ?~  nex  (cnts tub)
      =/  d  i.nex
      =/  s  (flag nex)
      ?^  s  [[%sand %f p.s] q.s]
      ?.  ?|  &((gte d 'a') (lte d 'z'))
              &((gte d '0') (lte d '9'))
              =('~' d)  =('-' d)  =('_' d)
          ==
        (cnts tub)
      =/  mex  (mule |.((perd:so [[1 1] nex])))
      ?.  ?=(%& -.mex)  1
      =/  vex  p.mex
      ?~  q.vex  (cnts tub)
      ?:  (gapy q.q.u.q.vex)  2
      [((jock |) p.u.q.vex) q.q.u.q.vex]
    ::
        %'`'
      ?~  nex  0
      =/  d  i.nex
      =/  r=res
        ?.  =('@' d)  0
        =/  m  (mota t.nex)
        =/  s  (tick q.m)
        ?@  s  s
        [[%ktls [%sand p.m 0] [%ktls [%sand %$ 0] p.s]] q.s]
      ?.  =(0 r)  r
      =/  r=res
        ?.  =('*' d)  0
        =/  s  (tick t.nex)
        ?@  s  s
        [[%kthp [%base %noun] p.s] q.s]
      ?.  =(0 r)  r
      =/  r=res
        =/  m  (wyde nex)
        ?@  m  m
        =/  s  (tick q.m)
        ?@  s  s
        [[%kthp p.m p.s] q.s]
      ?.  =(0 r)  r
      =/  r=res
        ?.  =('+' d)  0
        =/  m  (wide t.nex)
        ?@  m  m
        =/  s  (tick q.m)
        ?@  s  s
        [[%ktls p.m p.s] q.s]
      ?.  =(0 r)  r
      =/  s  (wide nex)
      ?@  s  s
      [[[%rock %n ~] p.s] q.s]
    ::
        %'!'
      =/  r  (wide nex)
      ?^  r  [[%wtzp p.r] q.r]
      ?.  =(0 r)  r
      ?.  ?=([%'!' *] nex)  0
      [[%zpzp ~] t.nex]
    ::
        %'?'
      ?.  ?=([%'(' *] nex)  [[%base %flag] nex]
      =/  r  (hunk ')' t.nex)
      ?@  r  ?:(=(0 r) [[%base %flag] nex] r)
      [[%ktcl %bcwt p.r] q.r]
    ::
        %'^'
      =/  w  (rope tub)
      ?@  w  ?:(=(0 w) [[%base %cell] nex] w)
      [[%wing p.w] q.w]
    ::
        %'$'
      ?~  nex  (rump tub)
      =/  d  i.nex
      ?:  =('$' d)  [[%leaf %tas %$] t.nex]
      ?:  &((gte d 'a') (lte d 'z'))
        =/  s  (sym nex)
        [[%leaf %tas p.s] q.s]
      =/  r=res
        ?:  =('\'' d)
          =/  s  (qut t.nex)
          ?@  s  s
          [[%leaf %t p.s] q.s]
        ?.  ?|  &((gte d '0') (lte d '9'))
                =('-' d)  =('~' d)  =('.' d)
            ==
          0
        =/  c  (nuck nex)
        ?@  c  c
        ?.  ?=(%$ -.p.c)  0
        [[%leaf p.p.c] q.c]
      ?.  =(0 r)  r
      (rump tub)
    ::
        %'@'
      =/  m  (mota nex)
      [[%base %atom p.m] q.m]
    ::
        %'*'
      =/  s  (wyde nex)
      ?@  s  ?:(=(0 s) [[%base %noun] nex] s)
      [[%kttr p.s] q.s]
    ::
        %'_'
      =/  r  (wide nex)
      ?@  r  r
      [[%ktcl %bccb p.r] q.r]
    ::
        %':'
      ?~  nex  0
      ?:  =('/' i.nex)
        =/  r  (wide t.nex)
        ?@  r  r
        [[%mcfs p.r] q.r]
      ?.  =('(' i.nex)  0
      =/  r  (hank ')' t.nex)
      ?@  r  r
      [[%mccl p.r] q.r]
    ::
        %','
      =/  s  (wyde nex)
      ?^  s  [[%ktcl p.s] q.s]
      ?.  =(0 s)  s
      =/  w  (rope tub)
      ?@  w  w
      [[%wing p.w] q.w]
    ::
        %'<'
      =/  r  (hank '>' nex)
      ?@  r  r
      [[%tell p.r] q.r]
    ::
        %'>'
      =/  r  (hank '<' nex)
      ?@  r  r
      [[%yell p.r] q.r]
    ::
        ?(%'0' %'1' %'2' %'3' %'4' %'5' %'6' %'7' %'8' %'9')
      =/  s  (bisk tub)
      ?@  s  s
      =/  nix  q.s
      ?.  ?=([?(%'+' %'/') *] nix)  [[%sand p.s] nix]
      =/  r  (wide t.nix)
      ?@  r  ?:(=(0 r) [[%sand p.s] nix] r)
      [[[%rock p.s] p.r] q.r]
    ::
        %'/'
      =/  g  (gash nex)
      ?@  g  g
      =/  nix  q.g
      =/  pof=$@(@ [p=(unit [p=@ud q=tyke]) q=tape])
        ?.  ?=([%'%' *] nix)  [~ nix]
        =/  n=@  0
        =/  nox  t.nix
        |-  ^-  $@(@ [p=(unit [p=@ud q=tyke]) q=tape])
        ?:  ?=([%'%' *] nox)  $(n +(n), nox t.nox)
        ?.  ?=([%'/' *] nox)  [~ nix]
        =/  h  (gash t.nox)
        ?@  h  h
        [[~ n p.h] q.h]
      ?@  pof  pof
      =/  u  (pash p.g p.pof)
      ?~  u  0
      [[%clsg u.u] q.pof]
    ::
        %'#'  ?.(?=([%'/' *] nex) 0 (reed t.nex))
    ==
  ::                                                ::  ++pash
  ++  pash                                          ::  tyke to hoons
    |=  [a=tyke pof=(unit [p=@ud q=tyke])]
    ^-  (unit (list hoon))
    ?^  pof  (posh:vaz [~ a] pof)
    =/  r=(unit (list hoon))
      =/  b  a
      |-  ^-  (unit (list hoon))
      ?~  b  [~ ~]
      ?~  i.b  ~
      =/  m  $(b t.b)
      ?~  m  ~
      [~ u.i.b u.m]
    ?^  r  r
    (posh:vaz [~ a] ~)
  ::                                                ::  ++pers
  ++  pers                                          ::  % and %%: here, up
    |=  [n=@ tub=tape]
    ^-  res
    =/  u  (posh:vaz ~ ~ n ~)
    ?~  u  0
    [[%clsg u.u] tub]
  ::                                                ::  ++porc
  ++  porc                                          ::  %/a, after the fas
    |=  [n=@ tub=tape]
    ^-  res
    =/  g  (gash tub)
    ?@  g  g
    =/  u  (posh:vaz ~ ~ n p.g)
    ?~  u  0
    [[%clsg u.u] q.g]
  ::                                                ::  ++nuck
  ++  nuck                                          ::  an atom by nuck:so
    |=  tub=tape
    ^-  $@(@ [p=coin q=tape])
    =/  mex  (mule |.((nuck:so [[1 1] tub])))
    ?.  ?=(%& -.mex)  1
    =/  vex  p.mex
    ?~  q.vex  0
    ?:  (gapy q.q.u.q.vex)  2
    [p.u.q.vex q.q.u.q.vex]
  ::                                                ::  ++gapy
  ++  gapy                                          ::  a stock atom rule
    |=  tub=tape                                    ::  may want text past
    ^-  ?                                           ::  the end of the tape
    ?~  tub  |
    =/  c  i.tub
    ?.  |(=('.' c) =('-' c) =('\\' c))  |
    ?:  ?=(@ (tgay t.tub))  &
    ?.  &(=('-' c) ?=([%'-' *] t.tub))  |
    ?=(@ (tgay t.t.tub))
  ::                                                ::  ++hair
  ++  hair                                          ::  line and column of
    |=  tub=tape                                    ::  a rest of the tape
    ^-  [@ud @ud]
    ?:  =(%line end)  [lin +((sub len (lent tub)))]
    =/  n  (sub len (lent tub))
    =/  [l=@ud c=@ud t=tape]  [lin beg all]
    |-  ^-  [@ud @ud]
    ?:  =(0 n)  [l +(c)]
    ?~  t  [l +(c)]
    ?:  =(10 i.t)  $(n (dec n), l +(l), c 0, t t.t)
    $(n (dec n), c +(c), t t.t)
  ::                                                ::  ++tgay
  ++  tgay                                          ::  optional gap in a
    |=  tub=tape                                    ::  tape; 2 if it may
    ^-  $@(@ [~ q=tape])                            ::  run past the end
    ?~  tub  ?:(=(%eof end) [~ ~] 2)
    =/  c  i.tub
    ?:  =(10 c)  (tgaw t.tub)
    ?:  =(' ' c)
      =/  nex  t.tub
      ?~  nex  ?:(=(%eof end) [~ tub] 2)
      ?:  |(=(' ' i.nex) =(10 i.nex))  (tgaw t.nex)
      ?.  ?=([%':' %':' *] nex)  [~ tub]
      =/  v  (tvul t.t.nex)
      ?@  v  ?:(=(0 v) [~ tub] v)
      (tgaw q.v)
    ?.  ?=([%':' %':' *] tub)  [~ tub]
    =/  v  (tvul t.t.tub)
    ?@  v  ?:(=(0 v) [~ tub] v)
    (tgaw q.v)
  ::                                                ::  ++tvul
  ++  tvul                                          ::  comment, after ::
    |=  tub=tape
    ^-  $@(@ [~ q=tape])
    ?~  tub  ?:(=(%eof end) [~ ~] 2)
    =/  c  i.tub
    ?:  =(10 c)  [~ t.tub]
    ?:  |((lth c 32) =(127 c))  0
    $(tub t.tub)
  ::                                                ::  ++tgaw
  ++  tgaw                                          ::  spaces, line ends
    |=  tub=tape                                    ::  and comments
    ^-  $@(@ [~ q=tape])
    ?~  tub  ?:(=(%eof end) [~ ~] 2)
    =/  c  i.tub
    ?:  |(=(' ' c) =(10 c))  $(tub t.tub)
    ?.  ?=([%':' %':' *] tub)  [~ tub]
    =/  v  (tvul t.t.tub)
    ?@  v  ?:(=(0 v) [~ tub] v)
    $(tub q.v)
  ::                                                ::  ++sols
  ++  sols                                          ::  tapes by dots, after
    |=  [pre=@ tub=tape]                            ::  a doq; pre: 0, + or -
    ^-  $@(@ [p=(list (list woof)) q=tape])
    =/  s  (soil tub)
    ?@  s  s
    =/  nex  q.s
    ?.  ?=([%'.' *] nex)  [[p.s ~] nex]
    =/  g  (tgay t.nex)
    ?@  g  g
    =/  nix=tape
      ?:  =(0 pre)  q.g
      ?.  &(?=(^ q.g) =(pre i.q.g))  ~
      t.q.g
    ?.  ?=([%'"' *] nix)  [[p.s ~] nex]
    =/  m  $(tub t.nix)
    ?@  m  ?:(=(0 m) [[p.s ~] nex] m)
    [[p.s p.m] q.m]
  ::                                                ::  ++knit
  ++  knit                                          ::  join tapes
    |=  a=(list (list woof))
    ^-  (list woof)
    ?~  a  ~
    ?~  t.a  i.a
    (weld i.a $(a t.a))
  ::                                                ::  ++gash
  ++  gash                                          ::  path, after a fas
    |=  tub=tape
    ^-  $@(@ [p=tyke q=tape])
    =/  l  (limp tub)
    ?@  l  ?:(=(0 l) [~ tub] l)
    =/  nex  q.l
    ?.  ?=([%'/' *] nex)  l
    =/  m  $(tub t.nex)
    ?@  m  m
    ?~  p.m  [p.l nex]
    [(weld p.l p.m) q.m]
  ::                                                ::  ++limp
  ++  limp                                          ::  fases and an element
    |=  tub=tape
    ^-  $@(@ [p=tyke q=tape])
    ?~  tub  0
    ?.  =('/' i.tub)  (gasp tub)
    =/  m  $(tub t.tub)
    ?@  m  m
    [[[~ %sand %tas %$] p.m] q.m]
  ::                                                ::  ++tiss
  ++  tiss                                          ::  count tises
    |=  tub=tape
    ^-  [p=@ q=tape]
    =/  n=@  0
    |-  ^-  [p=@ q=tape]
    ?.  ?=([%'=' *] tub)  [n tub]
    $(n +(n), tub t.tub)
  ::                                                ::  ++gasp
  ++  gasp                                          ::  =element=, or tises
    |=  tub=tape
    ^-  $@(@ [p=tyke q=tape])
    =/  a  (tiss tub)
    =/  h  (hasp q.a)
    ?@  h
      ?.  =(0 h)  h
      ?:  =(0 p.a)  0
      [(reap p.a ~) q.a]
    =/  b  (tiss q.h)
    ?:  &(=(0 p.a) =(0 p.b))  [[[~ p.h] ~] q.h]
    [(weld (reap p.a ~) `tyke`[[~ p.h] (reap p.b ~)]) q.b]
  ::                                                ::  ++hasp
  ++  hasp                                          ::  path element
    |=  tub=tape
    ^-  res
    ?~  tub  0
    =/  c  i.tub
    ?:  &((gte c 'a') (lte c 'z'))
      =/  s  (sym tub)
      [[%sand %tas p.s] q.s]
    ?+    c  0
        %'$'  [[%sand %tas %$] t.tub]
        %'['
      =/  h  (wide t.tub)
      ?@  h  h
      ?.  ?=([%']' *] q.h)  0
      [p.h t.q.h]
    ::
        %'('
      =/  h  (hank ')' t.tub)
      ?@  h  h
      [[%cncl p.h] q.h]
    ::
        %'\''
      =/  s  (qut t.tub)
      ?@  s  s
      [[%sand %t p.s] q.s]
    ::
        ?(%'-' %'.' %'~' %'0' %'1' %'2' %'3' %'4' %'5' %'6' %'7' %'8' %'9')
      =/  n  (nuck tub)
      ?@  n  n
      [[%sand ?:(?=([~ %tas *] p.n) %tas %ta) ~(rent co p.n)] q.n]
    ==
  ::                                                ::  ++sign
  ++  sign                                          ::  aura of a signed
    |=  a=@tas
    ^-  @tas
    (cat 3 's' (rsh 3 a))
  ::                                                ::  ++flag
  ++  flag                                          ::  y or n, after dot
    |=  tub=tape
    ^-  $@(@ [p=? q=tape])
    ?~  tub  0
    ?:  =('y' i.tub)  [& t.tub]
    ?.  =('n' i.tub)  0
    ?:  ?=([%'a' %'n' *] t.tub)  0
    [| t.tub]
  ::                                                ::  ++twid
  ++  twid                                          ::  may be an atom after
    |=  tub=tape                                    ::  a sig: not mine
    ^-  ?
    ?~  tub  |
    =/  c  i.tub
    ?|  &((gte c 'a') (lte c 'z'))
        &((gte c '0') (lte c '9'))
        =('.' c)  =('~' c)  =('-' c)
    ==
  ::                                                ::  ++tick
  ++  tick                                          ::  tic and a hoon
    |=  tub=tape
    ^-  res
    ?.  ?=([%'`' *] tub)  0
    (wide t.tub)
  ::                                                ::  ++tisk
  ++  tisk                                          ::  =spec, after the tis
    |=  tub=tape
    ^-  res
    =/  s  (wyde tub)
    ?@  s  s
    =/  n  ~(autoname ax p.s)
    ?~  n  0
    [[%ktts u.n %kttr p.s] q.s]
  ::                                                ::  ++cnts
  ++  cnts                                          ::  wing as %cnts
    |=  tub=tape
    ^-  res
    =/  w  (rope tub)
    ?@  w  w
    [[%cnts p.w ~] q.w]
  ::                                                ::  ++cnsg
  ++  cnsg                                          ::  ~(a b c), after pal
    |=  tub=tape
    ^-  res
    =/  w  (rope tub)
    ?@  w  w
    ?.  ?=([%' ' *] q.w)  0
    =/  h  (wide t.q.w)
    ?@  h  h
    ?.  ?=([%' ' *] q.h)  0
    =/  r  (hank ')' t.q.h)
    ?@  r  r
    [[%cnsg p.w p.h p.r] q.r]
  ::                                                ::  ++rupl
  ++  rupl                                          ::  [a b], after sel
    |=  [sig=? tub=tape]
    ^-  res
    =/  r=rel
      ?.  ?=([%' ' *] tub)  (hank ']' tub)
      ?:  |(?=(~ t.tub) ?=([?(%' ' %10) *] t.tub))  0
      ?:  =(0 lin)  2
      =/  f=?  &(fin =(%eof end))
      =/  a  (tall(fin f) (stas t.tub))
      ?@  a  ?:(&(=(0 a) !f) 2 a)
      =/  l  (thak(fin f) ']' p.a q.a)
      ?@  l  ?:(&(=(0 l) !f) 2 l)
      [p.l (taps q.l)]
    ?@  r  r
    =/  nex  q.r
    ?.  ?=([%'~' *] nex)
      [?:(sig [%clsg p.r] [%cltr p.r]) nex]
    [[%clsg ?:(sig [%clsg p.r] [%cltr p.r]) ~] t.nex]
  ::                                                ::  ++rump
  ++  rump                                          ::  wing, or a+b
    |=  tub=tape
    ^-  res
    =/  w  (rope tub)
    ?@  w  w
    =/  nex  q.w
    ?.  ?=([?(%'+' %'/') *] nex)  [[%wing p.w] nex]
    =/  h  (wide t.nex)
    ?@  h  ?:(=(0 h) [[%wing p.w] nex] h)
    =/  a  p.w
    ?.  ?=([@ ~] a)  0
    [[[%rock %tas i.a] p.h] q.h]
  ::                                                ::  ++rope
  ++  rope                                          ::  wing
    |=  tub=tape
    ^-  rew
    =/  l  (limb tub)
    ?@  l  l
    =/  nex  q.l
    ?.  ?=([%'.' *] nex)  [[p.l ~] nex]
    =/  m  $(tub t.nex)
    ?@  m  ?:(=(0 m) [[p.l ~] nex] m)
    [[p.l p.m] q.m]
  ::                                                ::  ++limb
  ++  limb                                          ::  wing element
    |=  tub=tape
    ^-  $@(@ [p=^limb q=tape])
    ?~  tub  0
    =/  c  i.tub
    ?:  &((gte c 'a') (lte c 'z'))  (sym tub)
    =/  nex  t.tub
    ?+    c  0
        %'$'  [%$ nex]
        %'.'  [[%& 1] nex]
        %','  [[%| 0 ~] nex]
        %'-'  (ven 2 nex)
        %'+'
      ?~  nex  [[%& 3] ~]
      ?.  &((gte i.nex '0') (lte i.nex '9'))  (ven 3 nex)
      =/  d  (dim nex)
      [[%& p.d] q.d]
    ::
        %'&'
      ?~  nex  0
      ?.  &((gte i.nex '0') (lte i.nex '9'))  0
      =/  d  (dim nex)
      [[%& (sub (bex +(p.d)) 2)] q.d]
    ::
        %'|'
      ?~  nex  0
      ?.  &((gte i.nex '0') (lte i.nex '9'))  0
      =/  d  (dim nex)
      [[%& (dec (bex +(p.d)))] q.d]
    ::
        %'^'
      =/  n=@  1
      |-  ^-  $@(@ [p=^limb q=tape])
      ?~  nex  0
      =/  d  i.nex
      ?:  =('^' d)  $(n +(n), nex t.nex)
      ?:  =('$' d)  [[%| n ~ %$] t.nex]
      ?.  &((gte d 'a') (lte d 'z'))  0
      =/  s  (sym nex)
      [[%| n ~ p.s] q.s]
    ==
  ::                                                ::  ++ven
  ++  ven                                           ::  +>- axis, after
    |=  [axe=@ tub=tape]                            ::  the first step
    ^-  [p=[%& p=@] q=tape]
    ?~  tub  [[%& axe] ~]
    =/  c  i.tub
    ?:  =('<' c)  (vem (add axe axe) t.tub)
    ?:  =('>' c)  (vem +((add axe axe)) t.tub)
    [[%& axe] tub]
  ::                                                ::  ++vem
  ++  vem                                           ::  +>- axis, after
    |=  [axe=@ tub=tape]                            ::  a < or >
    ^-  [p=[%& p=@] q=tape]
    ?~  tub  [[%& axe] ~]
    =/  c  i.tub
    ?:  =('-' c)  (ven (add axe axe) t.tub)
    ?:  =('+' c)  (ven +((add axe axe)) t.tub)
    [[%& axe] tub]
  ::                                                ::  ++dim
  ++  dim                                           ::  0, or digits; the
    |=  tub=tape                                    ::  first is checked
    ^-  [p=@ q=tape]
    ?>  ?=(^ tub)
    ?:  =('0' i.tub)  [0 t.tub]
    =/  v=@  (sub i.tub '0')
    =/  tub=tape  t.tub
    |-  ^-  [p=@ q=tape]
    ?~  tub  [v ~]
    =/  c  i.tub
    ?.  &((gte c '0') (lte c '9'))  [v tub]
    $(v (add (sub c '0') (mul 10 v)), tub t.tub)
  ::                                                ::  ++sym
  ++  sym                                           ::  [a-z][a-z0-9-]*; the
    |=  tub=tape                                    ::  first is checked
    ^-  [p=@tas q=tape]
    ?>  ?=(^ tub)
    =/  acc=(list @)  [i.tub ~]
    =/  tub=tape  t.tub
    |-  ^-  [p=@tas q=tape]
    ?~  tub  [(rap 3 (flop acc)) ~]
    =/  c  i.tub
    ?:  ?|  &((gte c 'a') (lte c 'z'))
            =('-' c)
            &((gte c '0') (lte c '9'))
        ==
      $(acc [c acc], tub t.tub)
    [(rap 3 (flop acc)) tub]
  ::                                                ::  ++mota
  ++  mota                                          ::  [a-z]*[A-Z]*
    |=  tub=tape
    ^-  [p=@ta q=tape]
    =|  acc=(list @)
    |-  ^-  [p=@ta q=tape]
    ?~  tub  [(rap 3 (flop acc)) ~]
    =/  c  i.tub
    ?:  &((gte c 'a') (lte c 'z'))  $(acc [c acc], tub t.tub)
    (moth acc tub)
  ::                                                ::  ++moth
  ++  moth                                          ::  [A-Z]*, the end of
    |=  [acc=(list @) tub=tape]                     ::  an aura
    ^-  [p=@ta q=tape]
    ?~  tub  [(rap 3 (flop acc)) ~]
    =/  c  i.tub
    ?:  &((gte c 'A') (lte c 'Z'))  $(acc [c acc], tub t.tub)
    [(rap 3 (flop acc)) tub]
  ::                                                ::  ++hank
  ++  hank                                          ::  hoons by aces, then
    |=  [end=@ tub=tape]                            ::  the closing char
    ^-  rel
    =/  h  (wide tub)
    ?@  h  h
    =/  nex  q.h
    ?~  nex  0
    ?:  =(end i.nex)  [[p.h ~] t.nex]
    ?.  =(' ' i.nex)  0
    =/  m  $(tub t.nex)
    ?@  m  m
    [[p.h p.m] q.m]
  ::                                                ::  ++hunk
  ++  hunk                                          ::  specs by aces, then
    |=  [end=@ tub=tape]                            ::  the closing char
    ^-  rem
    =/  h  (wyde tub)
    ?@  h  h
    =/  nex  q.h
    ?~  nex  0
    ?:  =(end i.nex)  [[p.h ~] t.nex]
    ?.  =(' ' i.nex)  0
    =/  m  $(tub t.nex)
    ?@  m  m
    [[p.h p.m] q.m]
  ::                                                ::  ++rick
  ++  rick                                          ::  a b, c d and the
    |=  [end=@ tub=tape]                            ::  closing char
    ^-  rek
    =/  w  (rope tub)
    ?@  w  w
    ?.  ?=([%' ' *] q.w)  0
    =/  h  (wide t.q.w)
    ?@  h  h
    =/  nex  q.h
    ?~  nex  0
    ?:  ?=([%',' %' ' *] nex)
      =/  m  $(tub t.t.nex)
      ?@  m  m
      [[[p.w p.h] p.m] q.m]
    ?.  =(end i.nex)  0
    [[[p.w p.h] ~] t.nex]
  ::                                                ::  ++ruck
  ++  ruck                                          ::  spec hoon, .. and
    |=  tub=tape                                    ::  the par
    ^-  $@(@ [p=(list (pair spec hoon)) q=tape])
    =/  s  (wyde tub)
    ?@  s  s
    ?.  ?=([%' ' *] q.s)  0
    =/  h  (wide t.q.s)
    ?@  h  h
    =/  nex  q.h
    ?~  nex  0
    ?:  ?=([%',' %' ' *] nex)
      =/  m  $(tub t.t.nex)
      ?@  m  m
      [[[p.s p.h] p.m] q.m]
    ?.  =(')' i.nex)  0
    [[[p.s p.h] ~] t.nex]
  ::
  ::    rune bodies, from after the pal through the par
  ::
  ++  expa                                          ::  one hoon
    |=  tub=tape
    ^-  res
    =/  a  (wide tub)
    ?@  a  a
    ?.  ?=([%')' *] q.a)  0
    [p.a t.q.a]
  ::
  ++  expb                                          ::  two hoons
    |=  tub=tape
    ^-  $@(@ [p=[hoon hoon] q=tape])
    =/  a  (wide tub)
    ?@  a  a
    ?.  ?=([%' ' *] q.a)  0
    =/  b  (wide t.q.a)
    ?@  b  b
    ?.  ?=([%')' *] q.b)  0
    [[p.a p.b] t.q.b]
  ::
  ++  expc                                          ::  three hoons
    |=  tub=tape
    ^-  $@(@ [p=[hoon hoon hoon] q=tape])
    =/  a  (wide tub)
    ?@  a  a
    ?.  ?=([%' ' *] q.a)  0
    =/  b  (expb t.q.a)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  expd                                          ::  four hoons
    |=  tub=tape
    ^-  $@(@ [p=[hoon hoon hoon hoon] q=tape])
    =/  a  (wide tub)
    ?@  a  a
    ?.  ?=([%' ' *] q.a)  0
    =/  b  (expc t.q.a)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  expi                                          ::  two or more hoons
    |=  tub=tape
    ^-  rel
    =/  r  (hank ')' tub)
    ?@  r  r
    ?~  t.p.r  0
    r
  ::
  ++  exph                                          ::  wing and changes
    |=  tub=tape
    ^-  $@(@ [p=[wing (list (pair wing hoon))] q=tape])
    =/  w  (rope tub)
    ?@  w  w
    ?.  ?=([%' ' *] q.w)  0
    =/  k  (rick ')' t.q.w)
    ?@  k  k
    [[p.w p.k] q.k]
  ::
  ++  expq                                          ::  wing and two hoons
    |=  tub=tape
    ^-  $@(@ [p=[wing hoon hoon] q=tape])
    =/  w  (rope tub)
    ?@  w  w
    ?.  ?=([%' ' *] q.w)  0
    =/  b  (expb t.q.w)
    ?@  b  b
    [[p.w p.b] q.b]
  ::
  ++  expw                                          ::  wing and three hoons
    |=  tub=tape
    ^-  $@(@ [p=[wing hoon hoon hoon] q=tape])
    =/  w  (rope tub)
    ?@  w  w
    ?.  ?=([%' ' *] q.w)  0
    =/  b  (expc t.q.w)
    ?@  b  b
    [[p.w p.b] q.b]
  ::
  ++  expo                                          ::  skin and two hoons
    |=  tub=tape
    ^-  $@(@ [p=[skin hoon hoon] q=tape])
    =/  w  (wise tub)
    ?@  w  w
    ?.  ?=([%' ' *] q.w)  0
    =/  b  (expb t.q.w)
    ?@  b  b
    [[p.w p.b] q.b]
  ::
  ++  expt                                          ::  skin, wing, hoons
    |=  tub=tape
    ^-  $@(@ [p=[skin wing hoon hoon] q=tape])
    =/  w  (wise tub)
    ?@  w  w
    ?.  ?=([%' ' *] q.w)  0
    =/  b  (expq t.q.w)
    ?@  b  b
    [[p.w p.b] q.b]
  ::
  ++  expj                                          ::  skin and hoon
    |=  tub=tape
    ^-  $@(@ [p=[skin hoon] q=tape])
    =/  a  (wide tub)
    ?@  a  a
    =/  rud  ~(flay ap p.a)
    ?~  rud  0
    ?.  ?=([%' ' *] q.a)  0
    =/  b  (expa t.q.a)
    ?@  b  b
    [[u.rud p.b] q.b]
  ::
  ++  expp                                          ::  changes and hoon
    |=  tub=tape
    ^-  $@(@ [p=[(list (pair wing hoon)) hoon] q=tape])
    =/  k  (rick ' ' tub)
    ?@  k  k
    =/  b  (expa q.k)
    ?@  b  b
    [[p.k p.b] q.b]
  ::
  ++  expn                                          ::  wing, hoon, [hoon ~]
    |=  tub=tape
    ^-  $@(@ [p=[wing hoon hoon ~] q=tape])
    =/  w  (expq tub)
    ?@  w  w
    [[-.p.w +<.p.w +>.p.w ~] q.w]
  ::
  ++  expm                                          ::  wing, hoon, changes
    |=  tub=tape
    ^-  $@(@ [p=[wing hoon (list (pair wing hoon))] q=tape])
    =/  w  (rope tub)
    ?@  w  w
    ?.  ?=([%' ' *] q.w)  0
    =/  a  (wide t.q.w)
    ?@  a  a
    ?.  ?=([%' ' *] q.a)  0
    =/  k  (rick ')' t.q.a)
    ?@  k  k
    [[p.w p.a p.k] q.k]
  ::
  ++  expg                                          ::  name, two hoons
    |=  tub=tape
    ^-  $@(@ [p=[[term (unit spec)] hoon hoon] q=tape])
    ?~  tub  0
    ?.  &((gte i.tub 'a') (lte i.tub 'z'))  0
    =/  s  (sym tub)
    =/  nex  q.s
    =/  m=$@(@ [p=(unit spec) q=tape])
      ?.  ?=([%'=' *] nex)  [~ nex]
      =/  p  (wyde t.nex)
      ?@  p  ?:(=(0 p) [~ nex] p)
      [[~ p.p] q.p]
    ?@  m  m
    ?.  ?=([%' ' *] q.m)  0
    =/  b  (expb t.q.m)
    ?@  b  b
    [[[p.s p.m] p.b] q.b]
  ::
  ++  exqa                                          ::  one spec
    |=  tub=tape
    ^-  rez
    =/  a  (wyde tub)
    ?@  a  a
    ?.  ?=([%')' *] q.a)  0
    [p.a t.q.a]
  ::
  ++  exqb                                          ::  two specs
    |=  tub=tape
    ^-  $@(@ [p=[spec spec] q=tape])
    =/  a  (wyde tub)
    ?@  a  a
    ?.  ?=([%' ' *] q.a)  0
    =/  b  (exqa t.q.a)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  exqc                                          ::  spec then hoon
    |=  tub=tape
    ^-  $@(@ [p=[spec hoon] q=tape])
    =/  a  (wyde tub)
    ?@  a  a
    ?.  ?=([%' ' *] q.a)  0
    =/  b  (expa t.q.a)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  exqd                                          ::  hoon then spec
    |=  tub=tape
    ^-  $@(@ [p=[hoon spec] q=tape])
    =/  a  (wide tub)
    ?@  a  a
    ?.  ?=([%' ' *] q.a)  0
    =/  b  (exqa t.q.a)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  exqg                                          ::  term and spec
    |=  tub=tape
    ^-  $@(@ [p=[term spec] q=tape])
    ?~  tub  0
    ?.  &((gte i.tub 'a') (lte i.tub 'z'))  0
    =/  s  (sym tub)
    ?.  ?=([%' ' *] q.s)  0
    =/  b  (exqa t.q.s)
    ?@  b  b
    [[p.s p.b] q.b]
  ::
  ++  exqn                                          ::  spec, hoons as %cltr
    |=  tub=tape
    ^-  $@(@ [p=[spec %cltr (list hoon)] q=tape])
    =/  a  (wyde tub)
    ?@  a  a
    ?.  ?=([%' ' *] q.a)  0
    =/  r  (hank ')' t.q.a)
    ?@  r  r
    [[p.a %cltr p.r] q.r]
  ::
  ++  expz                                          ::  spec, three hoons
    |=  tub=tape
    ^-  $@(@ [p=[spec hoon hoon hoon] q=tape])
    =/  a  (wyde tub)
    ?@  a  a
    ?.  ?=([%' ' *] q.a)  0
    =/  b  (expc t.q.a)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  hinb                                          ::  %a or %a.b, hoon
    |=  tub=tape
    ^-  $@(@ [p=[$@(term [term hoon]) hoon] q=tape])
    ?.  ?=([%'%' @ *] tub)  0
    ?.  &((gte i.t.tub 'a') (lte i.t.tub 'z'))  0
    =/  s  (sym t.tub)
    =/  nex  q.s
    =/  m=$@(@ [p=$@(term [term hoon]) q=tape])
      ?.  ?=([%'.' *] nex)  s
      =/  h  (wide t.nex)
      ?^  h  [[p.s p.h] q.h]
      ?.  =(0 h)  h
      ?.  ?=([%' ' *] t.nex)  s
      =/  h  (wide t.t.nex)
      ?@  h  ?:(=(0 h) s h)
      [[p.s p.h] q.h]
    ?@  m  m
    ?.  ?=([%' ' *] q.m)  0
    =/  b  (expa t.q.m)
    ?@  b  b
    [[p.m p.b] q.b]
  ::
  ++  gars                                          ::  0-3 gars and an ace
    |=  tub=tape                                    ::  if any
    ^-  [p=@ q=tape]
    ?.  ?=([%'>' *] tub)  [0 tub]
    =/  n=@  1
    =/  tub=tape  t.tub
    |-  ^-  [p=@ q=tape]
    ?:  &((lth n 3) ?=([%'>' *] tub))  $(n +(n), tub t.tub)
    ?.  ?=([%' ' *] tub)  [4 tub]
    [n t.tub]
  ::
  ::    tiki
  ::
  ++  teak                                          ::  wing or hoon to test
    |=  tub=tape
    ^-  $@(@ [p=tiki q=tape])
    ?~  tub  0
    =/  r=$@(@ [p=tiki q=tape])
      ?.  &((gte i.tub 'a') (lte i.tub 'z'))  0
      =/  s  (sym tub)
      ?.  ?=([%'=' *] q.s)  0
      =/  w  (rope t.q.s)
      ?^  w  [[%& [~ p.s] p.w] q.w]
      ?.  =(0 w)  w
      =/  h  (wide t.q.s)
      ?@  h  h
      [[%| [~ p.s] p.h] q.h]
    ?.  =(0 r)  r
    =/  w  (rope tub)
    ?^  w  [[%& ~ p.w] q.w]
    ?.  =(0 w)  w
    =/  h  (wide tub)
    ?@  h  h
    [[%| ~ p.h] q.h]
  ::
  ++  blue                                          ::  hoon in a tiki
    |=  [tik=tiki gen=hoon]
    ^-  hoon
    ?.  &(?=(%| -.tik) ?=(~ p.tik))  gen
    [%tsgr [%$ 3] gen]
  ::
  ++  teal                                          ::  spec in a tiki
    |=  [tik=tiki mod=spec]
    ^-  spec
    ?:  ?=(%& -.tik)  mod
    [%over [%& 3]~ mod]
  ::
  ++  gray                                          ::  test in a tiki
    |=  [tik=tiki gen=hoon]
    ^-  hoon
    ?-  -.tik
      %&  ?~(p.tik gen [%tstr [u.p.tik ~] [%wing q.tik] gen])
      %|  [%tsls ?~(p.tik q.tik [%ktts u.p.tik q.tik]) gen]
    ==
  ::
  ++  puce                                          ::  wing of a tiki
    |=  tik=tiki
    ^-  wing
    ?-  -.tik
      %&  ?~(p.tik q.tik [u.p.tik ~])
      %|  [[%& 2] ~]
    ==
  ::
  ++  bleu                                          ::  blue each case
    |=  [tik=tiki opt=(list (pair spec hoon))]
    ^-  (list (pair spec hoon))
    ?.  &(?=(%| -.tik) ?=(~ p.tik))  opt
    |-  ^-  (list (pair spec hoon))
    ?~  opt  ~
    [[p.i.opt [%tsgr [%$ 3] q.i.opt]] $(opt t.opt)]
  ::
  ++  tik3                                          ::  test, two hoons
    |=  tub=tape
    ^-  $@(@ [p=[tiki wing hoon hoon] q=tape])
    =/  t  (teak tub)
    ?@  t  t
    ?.  ?=([%' ' *] q.t)  0
    =/  b  (expb t.q.t)
    ?@  b  b
    [[p.t (puce p.t) (blue p.t -.p.b) (blue p.t +.p.b)] q.b]
  ::
  ::    skins
  ::
  ++  wise                                          ::  skin of =/
    |=  tub=tape
    ^-  $@(@ [p=skin q=tape])
    ?~  tub  0
    =/  c  i.tub
    ?:  &((gte c 'a') (lte c 'z'))
      =/  s  (sym tub)
      =/  nex  q.s
      ?.  ?=([?(%'/' %'=') *] nex)  s
      =/  p  (wyde t.nex)
      ?@  p  ?:(=(0 p) s p)
      [[%name p.s %spec p.p %base %noun] q.p]
    =/  r=$@(@ [p=skin q=tape])
      ?.  =('=' c)  0
      =/  p  (wyde t.tub)
      ?@  p  p
      =/  n  ~(autoname ax p.p)
      ?~  n  0
      [[%name u.n %spec p.p %base %noun] q.p]
    ?.  =(0 r)  r
    =/  p  (wyde tub)
    ?@  p  p
    [[%spec p.p %base %noun] q.p]
  ::
  ::    specs
  ::
  ++  wyde                                          ::  spec, wide form
    |=  tub=tape
    ^-  rez
    ?.  bug  (wydo tub)
    ?:  =(0 lin)  2
    =/  r  (wydo tub)
    ?@  r  r
    [[%dbug [wer (hair tub) (hair q.r)] p.r] q.r]
  ::                                                ::  ++wydo
  ++  wydo                                          ::  wyde, no spot
    |=  tub=tape
    ^-  rez
    ?~  tub  0
    =/  c  i.tub
    ?:  &((gte c 'a') (lte c 'z'))  (scad tub)
    =/  nex  t.tub
    ?:  =('#' c)  (redy tub)
    ?.  ?=([@ %'(' *] nex)  (scad tub)
    ?:  =('$' c)
      =/  r  (stru i.nex t.t.nex)
      ?.  =(0 r)  r
      (scad tub)
    ?.  =('%' c)  (scad tub)
    =/  r  (strp i.nex t.t.nex)
    ?.  =(0 r)  r
    (scad tub)
  ::                                                ::  ++stru
  ++  stru                                          ::  $x(..), after the pal
    |=  [b=@ tub=tape]
    ^-  rez
    ?+  b  0
      %':'  =/(r (hunk ')' tub) ?@(r r [[%bccl p.r] q.r]))
      %'%'  =/(r (hunk ')' tub) ?@(r r [[%bccn p.r] q.r]))
      %'?'  =/(r (hunk ')' tub) ?@(r r [[%bcwt p.r] q.r]))
      %'<'  =/(r (exqb tub) ?@(r r [[%bcgl p.r] q.r]))
      %'>'  =/(r (exqb tub) ?@(r r [[%bcgr p.r] q.r]))
      %'^'  =/(r (exqb tub) ?@(r r [[%bckt p.r] q.r]))
      %'@'  =/(r (exqb tub) ?@(r r [[%bcpt p.r] q.r]))
      %'-'  =/(r (exqb tub) ?@(r r [[%bchp p.r] q.r]))
      %'~'  =/(r (exqd tub) ?@(r r [[%bcsg p.r] q.r]))
      %'|'  =/(r (exqc tub) ?@(r r [[%bcbr p.r] q.r]))
      %'&'  =/(r (exqc tub) ?@(r r [[%bcpm p.r] q.r]))
      %'_'  =/(r (expa tub) ?@(r r [[%bccb p.r] q.r]))
      %';'  =/(r (expa tub) ?@(r r [[%bcmc p.r] q.r]))
      %'='  =/(r (exqg tub) ?@(r r [[%bcts p.r] q.r]))
      %'+'  =/(r (exqg tub) ?@(r r [[%bcls p.r] q.r]))
    ==
  ::                                                ::  ++like
  ++  like                                          ::  a.b:c.d
    |=  tub=tape
    ^-  rez
    =/  w  (rope tub)
    ?@  w  w
    =/  m  (ropa q.w)
    ?@  m  m
    [[%like p.w p.m] q.m]
  ::                                                ::  ++ropa
  ++  ropa                                          ::  :wing, repeated
    |=  tub=tape
    ^-  $@(@ [p=(list wing) q=tape])
    ?.  ?=([%':' *] tub)  [~ tub]
    =/  w  (rope t.tub)
    ?@  w  ?:(=(0 w) [~ tub] w)
    =/  m  $(tub q.w)
    ?@  m  m
    [[p.w p.m] q.m]
  ::                                                ::  ++scad
  ++  scad                                          ::  irregular specs
    |=  tub=tape
    ^-  rez
    ?~  tub  0
    =/  c  i.tub
    =/  nex  t.tub
    ?:  &((gte c 'a') (lte c 'z'))
      =/  s  (sym tub)
      ?.  ?=([%'=' *] q.s)  (like tub)
      =/  p  (wyde t.q.s)
      ?@  p  ?:(=(0 p) (like tub) p)
      [[%bcts p.s p.p] q.p]
    ?+    c  0
        %'@'
      =/  m  (mota nex)
      [[%base %atom p.m] q.m]
    ::
        %'*'  [[%base %noun] nex]
        %'~'  [[%base %null] nex]
        %'$'  (like tub)
        %'['
      =/  r  (hunk ']' nex)
      ?@  r  r
      [[%bccl p.r] q.r]
    ::
        %'('
      =/  h  (wide nex)
      ?@  h  h
      =/  nix  q.h
      ?~  nix  0
      ?:  =(')' i.nix)  [[%make p.h ~] t.nix]
      ?.  =(' ' i.nix)  0
      =/  r  (hunk ')' t.nix)
      ?@  r  r
      [[%make p.h p.r] q.r]
    ::
        %'?'
      ?.  ?=([%'(' *] nex)  [[%base %flag] nex]
      =/  r  (hunk ')' t.nex)
      ?@  r  ?:(=(0 r) [[%base %flag] nex] r)
      [[%bcwt p.r] q.r]
    ::
        %'^'
      =/  r  (like tub)
      ?.  =(0 r)  r
      [[%base %cell] nex]
    ::
        %'='
      =/  a=$@(@ [p=[term spec] q=tape])
        ?~  nex  0
        ?.  &((gte i.nex 'a') (lte i.nex 'z'))  0
        =/  s  (sym nex)
        ?.  ?=([%'=' *] q.s)  0
        =/  p  (wyde t.q.s)
        ?@  p  p
        [[p.s p.p] q.p]
      ?^  a
        =/  n  ~(autoname ax +.p.a)
        ?~  n  0
        [[%bcts (cat 3 -.p.a (cat 3 '-' u.n)) +.p.a] q.a]
      ?.  =(0 a)  a
      =/  p  (wyde nex)
      ?@  p  p
      =/  n  ~(autoname ax p.p)
      ?~  n  0
      [[%bcts u.n p.p] q.p]
    ::
        %'%'
      ?~  nex  0
      =/  d  i.nex
      ?:  &((gte d 'a') (lte d 'z'))
        =/  s  (sym nex)
        [[%leaf %tas p.s] q.s]
      ?:  &((gte d '0') (lte d '9'))
        =/  s  (bisk nex)
        ?@  s  s
        [[%leaf p.s] q.s]
      ?+    d  0
          %'$'  [[%leaf %tas %$] t.nex]
          %'&'  [[%leaf %f &] t.nex]
          %'|'  [[%leaf %f |] t.nex]
          %'\''
        =/  s  (qut t.nex)
        ?@  s  s
        [[%leaf %t p.s] q.s]
      ::
          ?(%'.' %'~' %'-')
        =/  c  (nuck nex)
        ?@  c  c
        ?.  ?=(%$ -.p.c)  0
        [[%leaf p.p.c] q.c]
      ==
    ::
        %'_'
      =/  r  (wide nex)
      ?@  r  r
      [[%bccb p.r] q.r]
    ::
        %','
      =/  r  (wide nex)
      ?@  r  r
      [[%bcmc p.r] q.r]
    ::
        %'!'
      ?.  ?=([%'!' *] nex)  0
      [[%base %void] t.nex]
    ::
        %'/'
      ?~  nex  0
      ?:  =('$' i.nex)  [[%loop %$] t.nex]
      ?.  &((gte i.nex 'a') (lte i.nex 'z'))  0
      =/  s  (sym nex)
      [[%loop p.s] q.s]
    ==
  ::
  ::    atoms and text
  ::
  ++  bisk                                          ::  @ud, @ux or @ub; the
    |=  tub=tape                                    ::  first is a digit
    ^-  $@(@ [p=dime q=tape])
    ?>  ?=(^ tub)
    ?.  =('0' i.tub)
      =/  r  (numb 10 3 1.000 | tub)
      ?@  r  r
      [[%ud p.r] q.r]
    =/  nex  t.tub
    ?~  nex  [[%ud 0] ~]
    =/  c  i.nex
    ?:  =('x' c)
      =/  r  (numb 16 4 0x1.0000 & t.nex)
      ?@  r  ?:(=(0 r) [[%ud 0] nex] r)
      [[%ux p.r] q.r]
    ?:  =('b' c)
      =/  r  (numb 2 4 16 | t.nex)
      ?@  r  ?:(=(0 r) [[%ud 0] nex] r)
      [[%ub p.r] q.r]
    ?:  |(=('c' c) =('i' c) =('v' c) =('w' c))
      =/  mex  (mule |.((bisk:so [[1 1] tub])))
      ?.  ?=(%& -.mex)  1
      =/  vex  p.mex
      ?~  q.vex  0
      ?:  (gapy q.q.u.q.vex)  2
      [p.u.q.vex q.q.u.q.vex]
    [[%ud 0] nex]
  ::                                                ::  ++digv
  ++  digv                                          ::  [0-9a-f], or 16
    |=  c=@
    ^-  @
    ?:  &((gte c '0') (lte c '9'))  (sub c '0')
    ?:  &((gte c 'a') (lte c 'f'))  (sub c 87)
    16
  ::                                                ::  ++hitv
  ++  hitv                                          ::  [0-9a-fA-F], or 16
    |=  c=@
    ^-  @
    ?:  &((gte c 'A') (lte c 'F'))  (sub c 55)
    (digv c)
  ::                                                ::  ++numb
  ++  numb                                          ::  0, or groups of
    |=  [bas=@ n=@ wuc=@ up=? tub=tape]             ::  n digits, base bas
    ^-  rea
    ?~  tub  0
    ?:  =('0' i.tub)  [0 t.tub]
    =/  d  (digv i.tub)
    ?.  (lth d bas)  0
    (lead bas (dec n) wuc up d t.tub)
  ::                                                ::  ++lead
  ++  lead                                          ::  up to n more digits
    |=  [bas=@ n=@ wuc=@ up=? v=@ tub=tape]         ::  of the first group
    ^-  rea
    ?:  =(0 n)  (rest bas wuc v tub)
    ?~  tub  [v ~]
    =/  d  ?:(up (hitv i.tub) (digv i.tub))
    ?.  (lth d bas)  (rest bas wuc v tub)
    $(n (dec n), v (add d (mul bas v)), tub t.tub)
  ::                                                ::  ++rest
  ++  rest                                          ::  dot, gap and a full
    |=  [bas=@ wuc=@ v=@ tub=tape]                  ::  group, repeated
    ^-  rea
    ?.  ?=([%'.' *] tub)  [v tub]
    =/  nex  t.tub
    =/  g=$@(@ [~ q=tape])
      ?:  &(?=(^ nex) !=(' ' i.nex) !=(10 i.nex) !=(':' i.nex))  [~ nex]
      (tgay nex)
    ?@  g  g
    =/  g  (grup bas wuc q.g)
    ?@  g  [v tub]
    $(v (add p.g (mul wuc v)), tub q.g)
  ::                                                ::  ++grup
  ++  grup                                          ::  a full group: digits
    |=  [bas=@ wuc=@ tub=tape]                      ::  until bas^k is wuc
    ^-  rea
    =/  [v=@ m=@]  [0 1]
    |-  ^-  rea
    ?:  =(m wuc)  [v tub]
    ?~  tub  0
    =/  d  (digv i.tub)
    ?.  (lth d bas)  0
    $(v (add d (mul bas v)), m (mul bas m), tub t.tub)
  ::                                                ::  ++qut
  ++  qut                                           ::  cord, after the soq
    |=  tub=tape
    ^-  rea
    ?:  ?=([%'\'' %'\'' *] tub)  (bloc tub)
    =|  acc=(list @)
    |-  ^-  rea
    ?~  tub  0
    =/  c  i.tub
    ?:  =('\'' c)  [(rep 3 (flop acc)) t.tub]
    ?.  =('\\' c)
      ?:  |((lth c 32) =(127 c))  0
      $(acc [c acc], tub t.tub)
    =/  g=$@(@ [~ q=tape])  ?~(acc [~ ~] (tgay t.tub))
    ?@  g  g
    =/  r=rea
      ?.  &(?=(^ acc) ?=([%'/' *] q.g))  (qit tub)
      (qit t.q.g)
    ?@  r  r
    $(acc [p.r acc], tub q.r)
  ::                                                ::  ++qit
  ++  qit                                           ::  one char of a cord
    |=  tub=tape
    ^-  rea
    ?~  tub  0
    =/  c  i.tub
    ?:  =('\'' c)  0
    ?.  =('\\' c)  ?:(|((lth c 32) =(127 c)) 0 [c t.tub])
    =/  nex  t.tub
    ?~  nex  0
    =/  d  i.nex
    ?:  |(=('\\' d) =('\'' d))  [d t.nex]
    =/  nix  t.nex
    ?~  nix  0
    =/  [a=@ b=@]  [(hitv d) (hitv i.nix)]
    ?:  |(=(16 a) =(16 b))  0
    [(add b (mul 16 a)) t.nix]
  ::                                                ::  ++bloc
  ++  bloc                                          ::  ''' block cord, at
    |=  tub=tape                                    ::  its second soq
    ^-  rea
    ?.  ?=([%'\'' %'\'' *] tub)  0
    ?:  =(0 lin)  2
    =/  lev  (sub +:(hair tub) 2)
    =/  aft=$@(@ [~ q=tape])
      =/  t  t.t.tub
      ?~  t  ?:(=(%eof end) 0 2)
      ?:  =(10 i.t)  [~ t]
      ?.  =(' ' i.t)  0
      =/  u=tape  t.t
      |-  ^-  $@(@ [~ q=tape])
      ?:  ?=([%' ' *] u)  $(u t.u)
      ?.  ?=([%':' %':' *] u)  0
      =/  l  (prnt u)
      ?^  q.l  ?:(=(10 i.q.l) [~ q.l] 0)
      ?:(=(%eof end) 0 2)
    ?@  aft  aft
    =/  b  (inde lev '\'' q.aft)
    ?@  b  b
    [(crip p.b) q.b]
  ::                                                ::  ++inde
  ++  inde                                          ::  indented block, at
    |=  [lev=@ud quo=@ tub=tape]                    ::  the line end after
    ^-  $@(@ [p=tape q=tape])                       ::  its opener
    =|  acc=(list tape)
    =/  fir=?  &
    |-  ^-  $@(@ [p=tape q=tape])
    ?~  tub  ?:(=(%eof end) 0 2)
    ?.  =(10 i.tub)  0
    =/  row  t.tub
    =/  d  (dent lev row)
    ?~  d
      ?~  row  ?:(=(%eof end) 0 2)
      ?.  =(10 i.row)  0
      $(tub row, acc [~ acc], fir |)
    =/  t  u.d
    ?:  &(!fir ?=([@ @ @ *] t) =(quo i.t) =(quo i.t.t) =(quo i.t.t.t))
      [(zing (join `tape`[`@tD`10 ~] (flop acc))) t.t.t.t]
    =/  l  (prnt t)
    ?:  &(?=(~ q.l) !=(%eof end))  2
    $(tub q.l, acc [p.l acc], fir |)
  ::                                                ::  ++dent
  ++  dent                                          ::  drop lev spaces
    |=  [lev=@ud tub=tape]
    ^-  (unit tape)
    ?:  =(0 lev)  [~ tub]
    ?.  ?=([%' ' *] tub)  ~
    $(lev (dec lev), tub t.tub)
  ::                                                ::  ++prnt
  ++  prnt                                          ::  printable chars
    |=  tub=tape
    ^-  [p=tape q=tape]
    =|  acc=tape
    |-  ^-  [p=tape q=tape]
    ?~  tub  [(flop acc) ~]
    =/  c  i.tub
    ?:  |((lth c 32) =(127 c))  [(flop acc) tub]
    $(acc [c acc], tub t.tub)
  ::                                                ::  ++soil
  ++  soil                                          ::  tape, after the doq
    |=  tub=tape
    ^-  $@(@ [p=(list woof) q=tape])
    ?:  ?=([%'"' %'"' *] tub)
      ?:  =(0 lin)  2
      =/  h  (hair tub)
      =/  b  (inde (sub +.h 2) '"' t.t.tub)
      ?@  b  b
      =/  me  ..soil(lin -.h, len (lent p.b), all p.b, beg 0, end %eof)
      =/  c  (soic:me p.b)
      ?@  c  c
      [p.c q.b]
    =|  acc=(list woof)
    |-  ^-  $@(@ [p=(list woof) q=tape])
    ?~  tub  0
    =/  c  i.tub
    ?:  =('"' c)  [(flop acc) t.tub]
    ?:  =('{' c)
      =/  r  (hank '}' t.tub)
      ?@  r  r
      $(acc [[~ %cltr p.r] acc], tub q.r)
    ?.  =('\\' c)
      ?:  |((lth c 32) =(127 c))  0
      $(acc [c acc], tub t.tub)
    =/  nex  t.tub
    ?~  nex  0
    =/  d  i.nex
    ?:  |(=('\\' d) =('"' d) =('{' d))  $(acc [d acc], tub t.nex)
    =/  nix  t.nex
    ?~  nix  0
    =/  [a=@ b=@]  [(digv d) (digv i.nix)]
    ?:  |(=(16 a) =(16 b))  0
    $(acc [(add b (mul 16 a)) acc], tub t.nix)
  ::                                                ::  ++soic
  ++  soic                                          ::  text of a block tape
    |=  tub=tape
    ^-  $@(@ [p=(list woof) ~])
    =|  acc=(list woof)
    |-  ^-  $@(@ [p=(list woof) ~])
    ?~  tub  [(flop acc) ~]
    =/  c  i.tub
    ?:  =(10 c)  $(acc [c acc], tub t.tub)
    ?:  =('{' c)
      =/  r  (hank '}' t.tub)
      ?@  r  r
      $(acc [[~ %cltr p.r] acc], tub q.r)
    ?.  =('\\' c)
      ?:  |((lth c 32) =(127 c))  0
      $(acc [c acc], tub t.tub)
    =/  nex  t.tub
    ?~  nex  0
    =/  d  i.nex
    ?:  |(=('\\' d) =('{' d))  $(acc [d acc], tub t.nex)
    =/  nix  t.nex
    ?~  nix  0
    =/  [a=@ b=@]  [(digv d) (digv i.nix)]
    ?:  |(=(16 a) =(16 b))  0
    $(acc [(add b (mul 16 a)) acc], tub t.nix)
  ::                                                ::  ++rune
  ++  rune                                          ::  xy(..), after the pal
    |=  [a=@ b=@ tub=tape]
    ^-  res
    ?+    a  0
        %'?'
      ?+    b  0
        %':'  =/(r (expc tub) ?@(r r [[%wtcl p.r] q.r]))
        %'.'  =/(r (expc tub) ?@(r r [[%wtdt p.r] q.r]))
        %'>'  =/(r (expb tub) ?@(r r [[%wtgr p.r] q.r]))
        %'<'  =/(r (expb tub) ?@(r r [[%wtgl p.r] q.r]))
        %'&'  =/(r (hank ')' tub) ?@(r r [[%wtpm p.r] q.r]))
        %'|'  =/(r (hank ')' tub) ?@(r r [[%wtbr p.r] q.r]))
        %'!'  =/(r (expa tub) ?@(r r [[%wtzp p.r] q.r]))
          %'#'
        =/  a  (wide tub)
        ?@  a  a
        =/  rud  ~(flay ap p.a)
        ?~  rud  0
        ?.  ?=([%' ' *] q.a)  0
        =/  t  (teak t.q.a)
        ?@  t  t
        ?.  ?=([%')' *] q.t)  0
        [(gray p.t [%wthx (tele p.t u.rud) (puce p.t)]) t.q.t]
      ::
          %'='
        =/  s  (wyde tub)
        ?@  s  s
        ?.  ?=([%' ' *] q.s)  0
        =/  t  (teak t.q.s)
        ?@  t  t
        ?.  ?=([%')' *] q.t)  0
        [(gray p.t [%wtts (teal p.t p.s) (puce p.t)]) t.q.t]
      ::
          %'~'
        =/  r  (tik3 tub)
        ?@  r  r
        [(gray -.p.r [%wtsg +.p.r]) q.r]
      ::
          %'^'
        =/  r  (tik3 tub)
        ?@  r  r
        [(gray -.p.r [%wtkt +.p.r]) q.r]
      ::
          %'@'
        =/  r  (tik3 tub)
        ?@  r  r
        [(gray -.p.r [%wtpt +.p.r]) q.r]
      ::
          %'-'
        =/  t  (teak tub)
        ?@  t  t
        ?.  ?=([%' ' *] q.t)  0
        =/  k  (ruck t.q.t)
        ?@  k  k
        [(gray p.t [%wthp (puce p.t) (bleu p.t p.k)]) q.k]
      ::
          %'+'
        =/  t  (teak tub)
        ?@  t  t
        ?.  ?=([%' ' *] q.t)  0
        =/  h  (wide t.q.t)
        ?@  h  h
        ?.  ?=([%' ' *] q.h)  0
        =/  k  (ruck t.q.h)
        ?@  k  k
        :_  q.k
        (gray p.t [%wtls (puce p.t) (blue p.t p.h) (bleu p.t p.k)])
      ==
    ::
        %'='
      ?+    b  0
        %'>'  =/(r (expb tub) ?@(r r [[%tsgr p.r] q.r]))
        %'<'  =/(r (expb tub) ?@(r r [[%tsgl p.r] q.r]))
        %'+'  =/(r (expb tub) ?@(r r [[%tsls p.r] q.r]))
        %'-'  =/(r (expb tub) ?@(r r [[%tshp p.r] q.r]))
        %','  =/(r (expb tub) ?@(r r [[%tscm p.r] q.r]))
        %'|'  =/(r (exqc tub) ?@(r r [[%tsbr p.r] q.r]))
        %'.'  =/(r (expq tub) ?@(r r [[%tsdt p.r] q.r]))
        %'?'  =/(r (expw tub) ?@(r r [[%tswt p.r] q.r]))
        %'^'  =/(r (expt tub) ?@(r r [[%tskt p.r] q.r]))
        %':'  =/(r (expp tub) ?@(r r [[%tscl p.r] q.r]))
        %'/'  =/(r (expo tub) ?@(r r [[%tsfs p.r] q.r]))
        %';'  =/(r (expo tub) ?@(r r [[%tsmc p.r] q.r]))
        %'*'  =/(r (expg tub) ?@(r r [[%tstr p.r] q.r]))
        %'~'  =/(r (expi tub) ?@(r r [[%tssg p.r] q.r]))
      ==
    ::
        %'|'
      ?+    b  0
        %'='  =/(r (exqc tub) ?@(r r [[%brts p.r] q.r]))
        %'*'  =/(r (exqc tub) ?@(r r [[%brtr p.r] q.r]))
        %'~'  =/(r (exqc tub) ?@(r r [[%brsg p.r] q.r]))
        %'.'  =/(r (expa tub) ?@(r r [[%brdt p.r] q.r]))
        %'-'  =/(r (expa tub) ?@(r r [[%brhp p.r] q.r]))
        %'?'  =/(r (expa tub) ?@(r r [[%brwt p.r] q.r]))
        %':'  =/(r (expb tub) ?@(r r [[%brcl p.r] q.r]))
        %'$'  =/(r (exqe tub) ?@(r r [[%brbc p.r] q.r]))
      ==
    ::
        %';'
      =/  r=res
        ?+  b  0
          %'~'  =/(r (expi tub) ?@(r r [[%mcsg p.r] q.r]))
          %':'  =/(r (expi tub) ?@(r r [[%mccl p.r] q.r]))
          %';'  =/(r (exqc tub) ?@(r r [[%mcmc p.r] q.r]))
          %'/'  =/(r (expa tub) ?@(r r [[%mcfs p.r] q.r]))
          %'<'  =/(r (expz tub) ?@(r r [[%mcgl p.r] q.r]))
        ==
      r
    ::
        %'%'
      ?+    b  0
        %'-'  =/(r (expb tub) ?@(r r [[%cnhp p.r] q.r]))
        %'+'  =/(r (expc tub) ?@(r r [[%cnls p.r] q.r]))
        %'^'  =/(r (expd tub) ?@(r r [[%cnkt p.r] q.r]))
        %'.'  =/(r (expb tub) ?@(r r [[%cndt p.r] q.r]))
        %':'  =/(r (expi tub) ?@(r r [[%cncl p.r] q.r]))
        %'='  =/(r (exph tub) ?@(r r [[%cnts p.r] q.r]))
        %'_'  =/(r (exph tub) ?@(r r [[%cncb p.r] q.r]))
        %'~'  =/(r (expn tub) ?@(r r [[%cnsg p.r] q.r]))
        %'*'  =/(r (expm tub) ?@(r r [[%cntr p.r] q.r]))
      ==
    ::
        %':'
      ?+    b  0
        %'-'  =/(r (expb tub) ?@(r r [[%clhp p.r] q.r]))
        %'_'  =/(r (expb tub) ?@(r r [[%clcb p.r] q.r]))
        %'+'  =/(r (expc tub) ?@(r r [[%clls p.r] q.r]))
        %'^'  =/(r (expd tub) ?@(r r [[%clkt p.r] q.r]))
        %'~'  =/(r (hank ')' tub) ?@(r r [[%clsg p.r] q.r]))
        %'*'  =/(r (hank ')' tub) ?@(r r [[%cltr p.r] q.r]))
      ==
    ::
        %'^'
      ?+    b  0
        %'-'  =/(r (exqc tub) ?@(r r [[%kthp p.r] q.r]))
        %'+'  =/(r (expb tub) ?@(r r [[%ktls p.r] q.r]))
        %'.'  =/(r (expb tub) ?@(r r [[%ktdt p.r] q.r]))
        %'_'  =/(r (expb tub) ?@(r r [[%ktcb p.r] q.r]))
        %'='  =/(r (expj tub) ?@(r r [[%ktts p.r] q.r]))
        %'|'  =/(r (expa tub) ?@(r r [[%ktbr p.r] q.r]))
        %'&'  =/(r (expa tub) ?@(r r [[%ktpm p.r] q.r]))
        %'~'  =/(r (expa tub) ?@(r r [[%ktsg p.r] q.r]))
        %'?'  =/(r (expa tub) ?@(r r [[%ktwt p.r] q.r]))
        %'*'  =/(r (exqa tub) ?@(r r [[%kttr p.r] q.r]))
        %':'  =/(r (exqa tub) ?@(r r [[%ktcl p.r] q.r]))
      ==
    ::
        %'~'
      ?+    b  0
        %'|'  =/(r (expb tub) ?@(r r [[%sgbr p.r] q.r]))
        %'_'  =/(r (expb tub) ?@(r r [[%sgcb p.r] q.r]))
        %'='  =/(r (expb tub) ?@(r r [[%sgts p.r] q.r]))
        %'!'  =/(r (expb tub) ?@(r r [[%sgzp p.r] q.r]))
        %'>'  =/(r (hinb tub) ?@(r r [[%sggr p.r] q.r]))
        %'<'  =/(r (hinb tub) ?@(r r [[%sggl p.r] q.r]))
        %'$'  =/(r (expf tub) ?@(r r [[%sgbc p.r] q.r]))
        %'%'  =/(r (hind tub) ?@(r r [[%sgcn p.r] q.r]))
        %'/'  =/(r (hine tub) ?@(r r [[%sgfs p.r] q.r]))
          %'+'
        =/  n  (tiss tub)
        =/  r=res
          ?.  &(!=(0 p.n) ?=([%' ' *] q.n))  0
          =/  r  (expa t.q.n)
          ?@  r  r
          [[%sgls p.n p.r] q.r]
        ?.  =(0 r)  r
        =/  r  (expa tub)
        ?@  r  r
        [[%sgls 0 p.r] q.r]
      ::
          %'&'
        =/  g  (gars tub)
        =/  r=res
          ?:  |(=(0 p.g) =(4 p.g))  0
          =/  r  (expb q.g)
          ?@  r  r
          [[%sgpm p.g p.r] q.r]
        ?.  =(0 r)  r
        =/  r  (expb tub)
        ?@  r  r
        [[%sgpm 0 p.r] q.r]
      ::
          %'?'
        =/  g  (gars tub)
        =/  r=res
          ?:  |(=(0 p.g) =(4 p.g))  0
          =/  r  (expc q.g)
          ?@  r  r
          [[%sgwt p.g p.r] q.r]
        ?.  =(0 r)  r
        =/  r  (expc tub)
        ?@  r  r
        [[%sgwt 0 p.r] q.r]
      ==
    ::
        %'.'
      ?+    b  0
        %'+'  =/(r (expa tub) ?@(r r [[%dtls p.r] q.r]))
        %'?'  =/(r (expa tub) ?@(r r [[%dtwt p.r] q.r]))
        %'*'  =/(r (expb tub) ?@(r r [[%dttr p.r] q.r]))
        %'='  =/(r (expb tub) ?@(r r [[%dtts p.r] q.r]))
        %'^'  =/(r (exqn tub) ?@(r r [[%dtkt p.r] q.r]))
      ==
    ::
        %'!'
      ?+    b  0
        %'>'  =/(r (expa tub) ?@(r r [[%zpgr p.r] q.r]))
        %'='  =/(r (expa tub) ?@(r r [[%zpts p.r] q.r]))
        %','  =/(r (expb tub) ?@(r r [[%zpcm p.r] q.r]))
        %';'  =/(r (expb tub) ?@(r r [[%zpmc p.r] q.r]))
        %'<'  =/(r (exqc tub) ?@(r r [[%zpgl p.r] q.r]))
        %'@'  =/(r (expx tub) ?@(r r [[%zppt p.r] q.r]))
        %'?'  =/(r (hinh tub) ?@(r r [[%zpwt p.r] q.r]))
        %':'  ?:(=(0 lin) 2 (expa(bug &) tub))
        %'.'  ?:(=(0 lin) 2 (expa(bug |) tub))
      ==
    ::
        %'$'
      ?:  =('.' b)  =/(r (exqa tub) ?@(r r [[%kttr p.r] q.r]))
      ?:  =(',' b)  =/(r (exqa tub) ?@(r r [[%ktcl p.r] q.r]))
      ?:  =(';' b)  0
      =/  r  (stru b tub)
      ?@  r  r
      [[%ktcl p.r] q.r]
    ==
  ::
  ++  vaz  =+(v=vast v(wer wer, bug bug))               ::  vast as we are
  ::
  ::    more wide rune bodies
  ::
  ++  strp                                              ::  %x(..), after the pal
    |=  [b=@ tub=tape]
    ^-  rez
    ?+    b  0
        %'-'
      =/  r  (exqd tub)
      ?@  r  r
      [[%make -.p.r +.p.r ~] q.r]
    ::
        %'.'
      =/  r  (exqc tub)
      ?@  r  r
      [[%make +.p.r -.p.r ~] q.r]
    ::
        %'+'
      =/  a  (wide tub)
      ?@  a  a
      ?.  ?=([%' ' *] q.a)  0
      =/  r  (exqb t.q.a)
      ?@  r  r
      [[%make p.a -.p.r +.p.r ~] q.r]
    ::
        %'^'
      =/  a  (wide tub)
      ?@  a  a
      ?.  ?=([%' ' *] q.a)  0
      =/  b  (wyde t.q.a)
      ?@  b  b
      ?.  ?=([%' ' *] q.b)  0
      =/  r  (exqb t.q.b)
      ?@  r  r
      [[%make p.a p.b -.p.r +.p.r ~] q.r]
    ::
        %':'
      =/  a  (wide tub)
      ?@  a  a
      ?.  ?=([%' ' *] q.a)  0
      =/  r  (hunk ')' t.q.a)
      ?@  r  r
      [[%make p.a p.r] q.r]
    ==
  ::
  ++  expx                                              ::  wings and two hoons
    |=  tub=tape
    ^-  $@(@ [p=[(list wing) hoon hoon] q=tape])
    =/  w  (rope tub)
    ?@  w  w
    =/  m  (ropa q.w)
    ?@  m  m
    ?.  ?=([%' ' *] q.m)  0
    =/  b  (expb t.q.m)
    ?@  b  b
    [[[p.w p.m] p.b] q.b]
  ::
  ++  expf                                              ::  %term and hoon
    |=  tub=tape
    ^-  $@(@ [p=[term hoon] q=tape])
    =/  s  (cens tub)
    ?@  s  s
    ?.  ?=([%' ' *] q.s)  0
    =/  b  (expa t.q.s)
    ?@  b  b
    [[p.s p.b] q.b]
  ::
  ++  cens                                              ::  %term
    |=  tub=tape
    ^-  $@(@ [p=term q=tape])
    ?.  ?=([%'%' @ *] tub)  0
    ?.  &((gte i.t.tub 'a') (lte i.t.tub 'z'))  0
    (sym t.tub)
  ::
  ++  dem                                               ::  digits to an atom
    |=  tub=tape
    ^-  rea
    ?~  tub  0
    ?.  &((gte i.tub '0') (lte i.tub '9'))  0
    =/  v=@  (sub i.tub '0')
    =/  tub=tape  t.tub
    |-  ^-  rea
    ?~  tub  [v ~]
    =/  c  i.tub
    ?:  &((gte c '0') (lte c '9'))
      $(v (add (sub c '0') (mul 10 v)), tub t.tub)
    ?.  =('\\' c)  [v tub]
    =/  g  (tgay t.tub)
    ?@  g  g
    ?.  ?=([%'/' @ *] q.g)  [v tub]
    =/  d  i.t.q.g
    ?.  &((gte d '0') (lte d '9'))  [v tub]
    $(v (add (sub d '0') (mul 10 v)), tub t.t.q.g)
  ::
  ++  hinn                                              ::  n or [n m]
    |=  tub=tape
    ^-  $@(@ [p=$@(@ [@ @]) q=tape])
    ?.  ?=([%'[' *] tub)  (dem tub)
    =/  a  (dem t.tub)
    ?@  a  a
    ?.  ?=([%' ' *] q.a)  0
    =/  b  (dem t.q.a)
    ?@  b  b
    ?.  ?=([%']' *] q.b)  0
    [[p.a p.b] t.q.b]
  ::
  ++  hinh                                              ::  1/2 numbers, hoon
    |=  tub=tape
    ^-  $@(@ [p=[$@(@ [@ @]) hoon] q=tape])
    =/  n  (hinn tub)
    ?@  n  n
    ?.  ?=([%' ' *] q.n)  0
    =/  b  (expa t.q.n)
    ?@  b  b
    [[p.n p.b] q.b]
  ::
  ++  bonk                                              ::  jet signature
    |=  tub=tape
    ^-  $@(@ [p=chum q=tape])
    =/  s  (cens tub)
    ?@  s  s
    =/  nex  q.s
    =/  r=$@(@ [p=chum q=tape])
      ?.  ?=([%':' @ *] nex)  0
      ?.  &((gte i.t.nex 'a') (lte i.t.nex 'z'))  0
      =/  t  (sym t.nex)
      =/  nix  q.t
      ?.  ?=([%'.' *] nix)  0
      =/  d=rea  ?.(?=([%'.' *] t.nix) 0 (dem t.t.nix))
      ?^  d  [[p.s p.t p.d] q.d]
      ?.  =(0 d)  d
      =/  d  (dem t.nix)
      ?@  d  d
      [[p.s p.t p.d] q.d]
    ?.  =(0 r)  r
    ?.  ?=([%'.' *] nex)  s
    =/  d  (dem t.nex)
    ?@  d  ?:(=(0 d) s d)
    [[p.s p.d] q.d]
  ::
  ++  hine                                              ::  jet and hoon
    |=  tub=tape
    ^-  $@(@ [p=[chum hoon] q=tape])
    =/  k  (bonk tub)
    ?@  k  k
    ?.  ?=([%' ' *] q.k)  0
    =/  b  (expa t.q.k)
    ?@  b  b
    [[p.k p.b] q.b]
  ::
  ++  hind                                              ::  jet, hoon, hooks, hoon
    |=  tub=tape
    ^-  $@(@ [p=[chum hoon tyre hoon] q=tape])
    =/  k  (bonk tub)
    ?@  k  k
    ?.  ?=([%' ' *] q.k)  0
    =/  a  (wide t.q.k)
    ?@  a  a
    ?.  ?=([%' ' *] q.a)  0
    =/  z  (bonz t.q.a)
    ?@  z  z
    ?.  ?=([%' ' *] q.z)  0
    =/  b  (expa t.q.z)
    ?@  b  b
    [[p.k p.a p.z p.b] q.b]
  ::
  ++  bonz                                              ::  ~ or (%a b, %c d)
    |=  tub=tape
    ^-  $@(@ [p=tyre q=tape])
    ?~  tub  0
    ?:  =('~' i.tub)  [~ t.tub]
    ?.  =('(' i.tub)  0
    ?:  ?=([%')' *] t.tub)  [~ t.t.tub]
    =/  tub=tape  t.tub
    |-  ^-  $@(@ [p=tyre q=tape])
    =/  s  (cens tub)
    ?@  s  s
    ?.  ?=([%' ' *] q.s)  0
    =/  h  (wide t.q.s)
    ?@  h  h
    =/  nex  q.h
    ?~  nex  0
    ?:  ?=([%',' %' ' *] nex)
      =/  m  $(tub t.t.nex)
      ?@  m  m
      [[[p.s p.h] p.m] q.m]
    ?.  =(')' i.nex)  0
    [[[p.s p.h] ~] t.nex]
  ::
  ++  lynw                                              ::  a or [a b c]
    |=  tub=tape
    ^-  $@(@ [p=(lest term) q=tape])
    ?~  tub  0
    ?:  &((gte i.tub 'a') (lte i.tub 'z'))
      =/  s  (sym tub)
      [[p.s ~] q.s]
    ?.  =('[' i.tub)  0
    (lynq t.tub)
  ::
  ++  lynq                                              ::  a b c], after the sel
    |=  tub=tape
    ^-  $@(@ [p=(lest term) q=tape])
    ?~  tub  0
    ?.  &((gte i.tub 'a') (lte i.tub 'z'))  0
    =/  s  (sym tub)
    =/  nex  q.s
    ?~  nex  0
    ?:  =(']' i.nex)  [[p.s ~] t.nex]
    ?.  =(' ' i.nex)  0
    =/  m  $(tub t.nex)
    ?@  m  m
    [[p.s p.m] q.m]
  ::
  ++  exqe                                              ::  names and a spec
    |=  tub=tape
    ^-  $@(@ [p=[(lest term) spec] q=tape])
    =/  l  (lynw tub)
    ?@  l  l
    ?.  ?=([%' ' *] q.l)  0
    =/  b  (exqa t.q.l)
    ?@  b  b
    [[p.l p.b] q.b]
  ::
  ++  tele                                              ::  skin in a tiki
    |=  [tik=tiki syn=skin]
    ^-  skin
    ?:  ?=(%& -.tik)  syn
    [%over [%& 3]~ syn]
  ::
  ::    #/ paths
  ::
  ++  reed                                              ::  #/a/b, after the fas
    |=  tub=tape
    ^-  res
    =/  l  (stes tub)
    ?@  l  l
    [[%clsg p.l] q.l]
  ::
  ++  stes                                              ::  path elements by fases
    |=  tub=tape
    ^-  $@(@ [p=(list hoon) q=tape])
    =/  e  (stem tub)
    ?@  e  e
    ?.  ?=([%'/' *] q.e)  [[p.e ~] q.e]
    =/  m  $(tub t.q.e)
    ?@  m  m
    [[p.e p.m] q.m]
  ::
  ++  stem                                              ::  one typed element
    |=  tub=tape
    ^-  res
    =/  non=res  [[%rock %tas %$] tub]
    ?~  tub  non
    =/  c  i.tub
    ?:  &((gte c 'a') (lte c 'z'))
      =/  s  (sym tub)
      ?.  ?=([%'+' ?(%'(' %'[') *] q.s)  [[%rock %tas p.s] q.s]
      =/  h=res
        ?:  =('[' i.t.q.s)
          =/  w  (wide t.t.q.s)
          ?@  w  w
          ?.  ?=([%']' *] q.w)  0
          [p.w t.q.w]
        =/  w  (hank ')' t.t.q.s)
        ?@  w  w
        [[%cncl p.w] q.w]
      ?^  h  [[%clhp [%rock %tas p.s] p.h] q.h]
      ?.  =(0 h)  h
      [[%rock %tas p.s] q.s]
    =/  r=res
      ?+    c  0
          %'$'  [[%rock %tas %$] t.tub]
          %'['
        =/  w  (wide t.tub)
        ?@  w  w
        ?.  ?=([%']' *] q.w)  0
        [p.w t.q.w]
      ::
          %'('
        =/  w  (hank ')' t.tub)
        ?@  w  w
        [[%cncl p.w] q.w]
      ::
          %'\''
        =/  s  (qut t.tub)
        ?@  s  s
        [[%clhp [%rock %tas %t] %sand %t p.s] q.s]
      ::
          ?(%'-' %'.' %'~' %'0' %'1' %'2' %'3' %'4' %'5' %'6' %'7' %'8' %'9')
        =/  mex
          %-  mule  |.
          ^-  (unit [p=* q=tape])
          ?:  =('.' c)
            =/  wex  (zust:so [[1 1] t.tub])
            ?~  q.wex  ~
            [~ p.u.q.wex q.q.u.q.wex]
          =/  wex=(unit [p=* q=tape])
            ?:  =('-' c)
              =/  xex  (tash:so [[1 1] tub])
              ?~(q.xex ~ [~ p.u.q.xex q.q.u.q.xex])
            ?.  =('~' c)
              =/  xex  (bisk:so [[1 1] tub])
              ?~(q.xex ~ [~ p.u.q.xex q.q.u.q.xex])
            =/  xex  (crub:so [[1 1] t.tub])
            ?~(q.xex [~ [%n ~] t.tub] [~ p.u.q.xex q.q.u.q.xex])
          ?~  wex  ~
          ?~  ((soft iota) p.u.wex)  ~
          wex
        ?.  ?=(%& -.mex)  1
        =/  vex  p.mex
        ?~  vex  0
        ?:  (gapy q.u.vex)  2
        =/  d  p.u.vex
        ?@  d  0
        ?.  ?=(@ -.d)  0
        [[%clhp [%rock %tas `@tas`-.d] [%sand `@tas`-.d +.d]] q.u.vex]
      ==
    ?:  =(0 r)  non
    r
  ::
  ++  redy                                              ::  #/ path spec, at the hax
    |=  tub=tape
    ^-  rez
    ?.  ?=([%'#' %'/' *] tub)  0
    =/  l  (redi t.t.tub)
    ?@  l  l
    =/  nex  q.l
    ?:  ?=([%'/' %'*' *] nex)
      [[%bccl i.p.l (snoc t.p.l [%base %noun])] t.t.nex]
    [[%bccl i.p.l (snoc t.p.l [%base %null])] nex]
  ::
  ++  redi                                              ::  path spec items
    |=  tub=tape
    ^-  $@(@ [p=[i=spec t=(list spec)] q=tape])
    =/  e  (redo tub)
    =/  nex  q.e
    ?.  &(?=([%'/' *] nex) !?=([%'*' *] t.nex))  [[p.e ~] nex]
    =/  m  $(tub t.nex)
    ?@  m  m
    [[p.e p.m] q.m]
  ::
  ++  redo                                              ::  one path spec item
    |=  tub=tape
    ^-  [p=spec q=tape]
    =/  non=[p=spec q=tape]  [[%leaf %tas %$] tub]
    ?~  tub  non
    =/  c  i.tub
    ?:  =('$' c)  [[%leaf %tas %$] t.tub]
    ?:  =('?' c)  [[%bccl [%leaf %tas %f] [%base %flag] ~] t.tub]
    ?:  =('~' c)  [[%bccl [%leaf %tas %n] [%base %null] ~] t.tub]
    ?:  =('@' c)
      =/  m  (mota t.tub)
      [[%bccl [%leaf %tas p.m] [%base %atom p.m] ~] q.m]
    ?.  &((gte c 'a') (lte c 'z'))  non
    =/  s  (sym tub)
    ?.  ?=([%'=' %'@' *] q.s)  [[%leaf %tas p.s] q.s]
    =/  m  (mota t.t.q.s)
    =/  bas=spec
      ?+  p.m  [%base %atom p.m]
        %f  [%base %flag]
        %n  [%base %null]
      ==
    [[%bccl [%leaf %tas p.m] [%bcts p.s bas] ~] q.m]
  ::
  ::    tall hoons in a wide place
  ::
  ++  stas                                              ::  a tape as a sat
    |=  tub=tape
    ^-  sat
    =/  h  (hair tub)
    =/  lis  (rowt tub)
    [tap.i.lis -.h (add (dec +.h) len.i.lis) t.lis]
  ::
  ++  taps                                              ::  a sat as a tape
    |=  s=sat
    ^-  tape
    =/  cur=tape  tub.s
    =/  rem  nex.s
    |-  ^-  tape
    ?~  rem  cur
    (weld cur `tape`[`@tD`10 $(cur tap.i.rem, rem t.rem)])
  ::
  ::    sail, wide form
  ::
  ++  tnod  |=(a=tape `manx:hoot`;/(a))                 ::  text node
  ++  saiw                                              ::  wide sail, after mic
    |=  tub=tape
    ^-  res
    =/  t  (swit tub)
    ?@  t  t
    :_  q.t
    ?-  -.p.t
      %&  [%xray p.p.t]
      %|  [%mcts p.p.t]
    ==
  ::
  ++  swit                                              ::  quote, flow or tag
    |=  tub=tape
    ^-  $@(@ [p=(each manx:hoot marl:hoot) q=tape])
    ?~  tub  0
    ?:  =('"' i.tub)
      =/  q  (squo | tub)
      ?@  q  q
      [[%| p.q] q.q]
    ?:  =('(' i.tub)
      =/  q  (spar tub)
      ?@  q  q
      [[%| p.q] q.q]
    =/  h  (shed tub)
    ?@  h  h
    =/  t  (stai q.h)
    ?@  t  t
    [[%& p.h p.t] q.t]
  ::
  ++  drop                                              ::  one top as marl
    |=  a=(each manx:hoot marl:hoot)
    ^-  marl:hoot
    ?-  -.a
      %&  [p.a ~]
      %|  p.a
    ==
  ::
  ++  squo                                              ::  "text" as marl, at
    |=  [tal=? tub=tape]                                ::  the doq
    ^-  $@(@ [p=marl:hoot q=tape])
    ?.  ?=([%'"' *] tub)  0
    ?.  ?=([%'"' %'"' *] t.tub)
      =/  i  (sinn tal & t.tub)
      ?@  i  i
      ?.  ?=([%'"' *] q.i)  0
      [(coll tal p.i) t.q.i]
    ?:  =(0 lin)  2
    =/  h  (hair tub)
    =/  b  (inde (dec +.h) '"' t.t.t.tub)
    ?@  b  b
    =/  me  ..squo(lin -.h, len (lent p.b), all p.b, beg 0, end %eof)
    =/  i  (sinn:me tal | p.b)
    ?@  i  i
    ?^  q.i  0
    [(coll tal p.i) q.b]
  ::
  ++  sinn                                              ::  text and embeds
    |=  [tal=? lyn=? tub=tape]
    ^-  $@(@ [p=(list $@(@ tuna:hoot)) q=tape])
    =|  acc=(list $@(@ tuna:hoot))
    |-  ^-  $@(@ [p=(list $@(@ tuna:hoot)) q=tape])
    ?~  tub  [(flop acc) ~]
    =/  c  i.tub
    ?:  =('\\' c)
      =/  nex  t.tub
      ?~  nex  [(flop acc) tub]
      =/  d  i.nex
      ?:  ?|  =('-' d)  =('+' d)  =('*' d)  =('%' d)  =(';' d)  =('{' d)
              =('\\' d)  =('"' d)
          ==
        $(acc [d acc], tub t.nex)
      =/  nix  t.nex
      ?~  nix  [(flop acc) tub]
      =/  [a=@ b=@]  [(digv d) (digv i.nix)]
      ?:  |(=(16 a) =(16 b))  [(flop acc) tub]
      $(acc [(add b (mul 16 a)) acc], tub t.nix)
    ?:  |(=(';' c) =('{' c) =('-' c) =('+' c) =('*' c) =('%' c))
      =/  e  (sinl tub)
      ?^  e  $(acc [p.e acc], tub q.e)
      ?.  =(0 e)  e
      ?:  =('{' c)  [(flop acc) tub]
      $(acc [c acc], tub t.tub)
    ?:  =(10 c)
      ?:  lyn  [(flop acc) tub]
      $(acc [c acc], tub t.tub)
    ?:  &(=('"' c) !tal)  [(flop acc) tub]
    ?:  |((lth c 32) =(127 c))  [(flop acc) tub]
    $(acc [c acc], tub t.tub)
  ::
  ++  sinl                                              ::  embed in text
    |=  tub=tape
    ^-  $@(@ [p=tuna:hoot q=tape])
    ?~  tub  0
    =/  c  i.tub
    ?:  =(';' c)
      ?.  ?=([%'{' *] t.tub)  0
      =/  h  (shed t.t.tub)
      ?@  h  h
      =/  e  (sele q.h)
      ?@  e  e
      ?.  ?=([%'}' *] q.e)  0
      [[p.h p.e] t.q.e]
    ?:  =('{' c)
      =/  r  (hank '}' t.tub)
      ?@  r  r
      [[%tape %cltr p.r] q.r]
    ?.  ?=([%'{' *] t.tub)  0
    =/  r  (hank '}' t.t.tub)
    ?@  r  r
    =/  gen=hoon  [%cltr p.r]
    :_  q.r
    ?:  =('-' c)  [%tape gen]
    ?:  =('+' c)  [%manx gen]
    ?:  =('*' c)  [%marl gen]
    [%call gen]
  ::
  ++  sele                                              ::  aces and tops
    |=  tub=tape
    ^-  $@(@ [p=marl:hoot q=tape])
    ?.  ?=([%' ' *] tub)  [~ tub]
    =/  i  (sint t.tub)
    ?@  i  ?:(=(0 i) [~ tub] i)
    =/  m  $(tub q.i)
    ?@  m  m
    [(weld p.i p.m) q.m]
  ::
  ++  sint                                              ::  top or -hoon, as marl
    |=  tub=tape
    ^-  $@(@ [p=marl:hoot q=tape])
    =/  t  (swit tub)
    ?^  t  [(drop p.t) q.t]
    ?.  =(0 t)  t
    ?~  tub  0
    =/  c  i.tub
    ?.  |(=('-' c) =('+' c) =('*' c) =('%' c))  0
    =/  h  (wide t.tub)
    ?@  h  h
    :_  q.h
    :_  ~
    ?:  =('-' c)  [%tape p.h]
    ?:  =('+' c)  [%manx p.h]
    ?:  =('*' c)  [%marl p.h]
    [%call p.h]
  ::
  ++  spar                                              ::  (a b c) flow, at pal
    |=  tub=tape
    ^-  $@(@ [p=marl:hoot q=tape])
    ?.  ?=([%'(' *] tub)  0
    =/  i  (sint t.tub)
    ?@  i
      ?.  =(0 i)  i
      ?.  ?=([%')' *] t.tub)  0
      [~ t.t.tub]
    |-  ^-  $@(@ [p=marl:hoot q=tape])
    =/  nex  q.i
    ?~  nex  0
    ?:  =(')' i.nex)  [p.i t.nex]
    ?.  =(' ' i.nex)  0
    =/  j  (sint t.nex)
    ?@  j  j
    $(i [(weld p.i p.j) q.j])
  ::
  ++  stai                                              ::  wide tail of a tag
    |=  tub=tape
    ^-  $@(@ [p=marl:hoot q=tape])
    ?~  tub  [~ ~]
    ?:  =(';' i.tub)  [~ t.tub]
    ?.  =(':' i.tub)  [~ tub]
    =/  w  (swra t.tub)
    ?^  w  w
    ?.  =(0 w)  w
    [~ tub]
  ::
  ++  swra                                              ::  flow, cord or top
    |=  tub=tape
    ^-  $@(@ [p=marl:hoot q=tape])
    =/  p  (spar tub)
    ?.  =(0 p)  p
    =/  q=$@(@ [p=marl:hoot q=tape])
      ?.  ?=([%'\'' *] tub)  0
      =/  c  (qut t.tub)
      ?@  c  c
      [[(tnod (trip p.c)) ~] q.c]
    ?.  =(0 q)  q
    =/  t  (swit tub)
    ?@  t  t
    [(drop p.t) q.t]
  ::
  ++  smix                                              ::  mixed-case symbol
    |=  tub=tape
    ^-  $@(@ [p=@tas q=tape])
    ?~  tub  0
    =/  c  i.tub
    ?.  |(&((gte c 'a') (lte c 'z')) &((gte c 'A') (lte c 'Z')))  0
    =/  acc=(list @)  [c ~]
    =/  tub=tape  t.tub
    |-  ^-  [p=@tas q=tape]
    ?~  tub  [(rap 3 (flop acc)) ~]
    =/  c  i.tub
    ?:  ?|  &((gte c 'a') (lte c 'z'))
            &((gte c 'A') (lte c 'Z'))
            &((gte c '0') (lte c '9'))
            =('-' c)
        ==
      $(acc [c acc], tub t.tub)
    [(rap 3 (flop acc)) tub]
  ::
  ++  sman                                              ::  tag or attribute name
    |=  tub=tape
    ^-  $@(@ [p=mane:hoot q=tape])
    =/  a  (smix tub)
    ?@  a  a
    ?.  ?=([%'_' *] q.a)  a
    =/  b  (smix t.q.a)
    ?@  b  a
    [[p.a p.b] q.b]
  ::
  ++  shed                                              ::  tag head
    |=  tub=tape
    ^-  $@(@ [p=marx:hoot q=tape])
    =/  n  (sman tub)
    ?@  n  n
    =/  i=[p=mart:hoot q=tape]
      =/  t  q.n
      ?.  ?=([%'#' @ *] t)  [~ t]
      ?.  &((gte i.t.t 'a') (lte i.t.t 'z'))  [~ t]
      =/  s  (sym t.t)
      [[[%id (trip p.s)] ~] q.s]
    =/  c=[p=tape q=tape]
      =/  t  q.i
      =|  acc=tape
      |-  ^-  [p=tape q=tape]
      ?.  ?=([%'.' @ *] t)  [acc t]
      ?.  &((gte i.t.t 'a') (lte i.t.t 'z'))  [acc t]
      =/  s  (sym t.t)
      $(t q.s, acc ?~(acc (trip p.s) :(weld acc " " (trip p.s))))
    =/  h=$@(@ [p=mart:hoot q=tape])
      =/  t  q.c
      ?.  ?=([?(%'/' %'@') %'"' *] t)  [~ t]
      =/  s  (soil t.t.t)
      ?@  s  ?:(=(0 s) [~ t] s)
      [[[?:(=('/' i.t) %href %src) p.s] ~] q.s]
    ?@  h  h
    =/  a  (satt q.h)
    ?@  a  a
    =/  cla=mart:hoot  ?~(p.c ~ [[%class p.c] ~])
    [[p.n :(weld p.i cla p.h p.a)] q.a]
  ::
  ++  satt                                              ::  (a "b", c d)
    |=  tub=tape
    ^-  $@(@ [p=mart:hoot q=tape])
    ?.  ?=([%'(' *] tub)  [~ tub]
    ?:  ?=([%')' *] t.tub)  [~ t.t.tub]
    =/  r=$@(@ [p=mart:hoot q=tape])
      =/  tub=tape  t.tub
      |-  ^-  $@(@ [p=mart:hoot q=tape])
      =/  n  (sman tub)
      ?@  n  n
      ?.  ?=([%' ' *] q.n)  0
      =/  h  (shop t.q.n)
      ?@  h  h
      =/  nex  q.h
      ?~  nex  0
      ?:  ?=([%',' %' ' *] nex)
        =/  m  $(tub t.t.nex)
        ?@  m  m
        [[[p.n p.h] p.m] q.m]
      ?.  =(')' i.nex)  0
      [[[p.n p.h] ~] t.nex]
    ?:  =(0 r)  [~ tub]
    r
  ::
  ++  shop                                              ::  attribute value
    |=  tub=tape
    ^-  $@(@ [p=(list beer:hoot) q=tape])
    =/  h  (wide tub)
    ?@  h  h
    [?:(?=([%knit *] p.h) p.p.h [[~ p.h] ~]) q.h]
  ::
  ++  coll                                              ::  group chars to text
    |=  [tal=? reb=(list $@(@ tuna:hoot))]
    ^-  marl:hoot
    =|  [sim=(list @) tuz=marl:hoot]
    |-  ^-  marl:hoot
    ?~  reb
      =.  sim
        ?.  tal  sim
        [10 |-(?~(sim sim ?:(=(32 i.sim) $(sim t.sim) sim)))]
      ?~(sim tuz [(tnod (flop sim)) tuz])
    ?@  i.reb
      $(reb t.reb, sim [i.reb sim])
    ?~  sim  [i.reb $(reb t.reb, sim ~)]
    [(tnod (flop sim)) i.reb $(reb t.reb, sim ~)]
  ::
  ::    tall form: arms on a +sat, the rest of the text by lines
  ::
  +$  sat  [tub=tape lin=@ud len=@ud nex=(list [len=@ud tap=tape])]
  +$  tes  $@(@ [p=hoon q=sat])                         ::  hoon and rest
  +$  tez  $@(@ [p=spec q=sat])                         ::  spec and rest
  ::                                                    ::  ++apex
  ++  apex                                              ::  a file: gay tall gay
    |=  lis=(lest [len=@ud tap=tape])
    ^-  $@(@ hoon)
    =/  r  (tall (gay [tap.i.lis 1 len.i.lis t.lis]))
    ?@  r  ?:(=(0 r) 0 1)
    =/  e  (gay q.r)
    ?.  &(?=(~ tub.e) ?=(~ nex.e))  0
    p.r
  ::                                                    ::  ++self
  ++  self                                              ::  this core, on the line
    |=  s=sat                                           ::  of s if spots are on
    ?:  &(!bug =(0 lin) =(%line end))  ..self
    ..self(lin ?:(bug lin.s 0), len len.s, all ~, beg 0, end %line)
  ::                                                    ::  ++lump
  ++  lump                                              ::  the rest of the line
    |=  [k=@ud s=sat]                                   ::  of s and k more
    ^-  [all=tape end=?(%more %eof) out=?]              ::  lines, as one tape
    =/  cur=tape  tub.s
    =/  rem  nex.s
    |-  ^-  [all=tape end=?(%more %eof) out=?]
    ?~  rem  [cur ?:(fin %eof %more) &]
    ?:  =(0 k)  [cur %more |]
    =/  m  $(k (dec k), cur tap.i.rem, rem t.rem)
    [(weld cur `tape`[`@tD`10 all.m]) end.m out.m]
  ::                                                    ::  ++slow
  ++  slow                                              ::  this core on a tape
    |=  [s=sat all=tape end=?(%more %eof)]              ::  joined from s
    %=  ..slow
      lin  lin.s
      len  (lent all)
      all  all
      beg  (sub len.s (lent tub.s))
      end  end
    ==
  ::                                                    ::  ++seat
  ++  seat                                              ::  the sat at a rest
    |=  [s=sat all=tape r=tape]                         ::  of a joined tape
    ^-  sat
    =/  n  (sub (nels all) (nels r))
    =/  i  (find "\0a" r)
    =/  tub=tape  ?~(i r (scag u.i r))
    ?:  =(0 n)  s(tub tub)
    =/  rem  (slag (dec n) nex.s)
    ?>  ?=(^ rem)
    [tub (add n lin.s) len.i.rem t.rem]
  ::                                                    ::  ++hare
  ++  hare                                              ::  line and column
    |=  s=sat
    ^-  [@ud @ud]
    [lin.s +((sub len.s (lent tub.s)))]
  ::                                                    ::  ++prns
  ++  prns                                              ::  all printable
    |=  tub=tape
    ^-  ?
    ?~  tub  &
    =/  c  i.tub
    ?:  |((lth c 32) =(127 c))  |
    $(tub t.tub)
  ::                                                    ::  ++gap
  ++  gap                                               ::  two spaces, a line
    |=  s=sat                                           ::  end or a comment,
    ^-  $@(@ sat)                                       ::  and all that follow
    =/  tub  tub.s
    ?~  tub
      ?~  nex.s  0
      (gaw tap.i.nex.s +(lin.s) len.i.nex.s t.nex.s)
    ?:  =(' ' i.tub)
      =/  nix  t.tub
      ?~  nix
        ?~  nex.s  0
        (gaw tap.i.nex.s +(lin.s) len.i.nex.s t.nex.s)
      ?:  =(' ' i.nix)  (gaw s(tub t.nix))
      ?.  ?=([%':' %':' *] nix)  0
      ?.  (prns t.t.nix)  0
      ?~  nex.s  s(tub ~)
      (gaw tap.i.nex.s +(lin.s) len.i.nex.s t.nex.s)
    ?.  ?=([%':' %':' *] tub)  0
    ?.  (prns t.t.tub)  0
    ?~  nex.s  s(tub ~)
    (gaw tap.i.nex.s +(lin.s) len.i.nex.s t.nex.s)
  ::                                                    ::  ++gaw
  ++  gaw                                               ::  spaces, line ends
    |=  sat                                             ::  and comments
    ^-  sat
    ?~  tub
      ?~  nex  +<
      $(tub tap.i.nex, lin +(lin), len len.i.nex, nex t.nex)
    ?:  =(' ' i.tub)  $(tub t.tub)
    ?.  ?=([%':' %':' *] tub)  +<
    ?.  (prns t.t.tub)  +<
    ?~  nex  +<(tub ~)
    $(tub tap.i.nex, lin +(lin), len len.i.nex, nex t.nex)
  ::                                                    ::  ++gay
  ++  gay                                               ::  optional gap
    |=  s=sat
    ^-  sat
    =/  g  (gap s)
    ?@  g  s
    g
  ::                                                    ::  ++tall
  ++  tall                                              ::  hoon, tall form
    |=  s=sat
    ^-  tes
    ?.  bug  (tell s)
    =/  r  (tell s)
    ?@  r  r
    [[%dbug [wer (hare s) (hare q.r)] p.r] q.r]
  ::                                                    ::  ++tell
  ++  tell                                              ::  tall, no spot
    |=  s=sat
    ^-  tes
    =/  tub  tub.s
    ?~  tub  0
    =/  nex  t.tub
    =/  r=tes
      ?~  nex  0
      ?.  ?=([%'(' *] t.nex)  (tune i.tub i.nex s(tub t.nex))
      (wrun s)
    ?.  =(0 r)  r
    =/  w  (wlon s)
    ?.  =(0 w)  w
    ?:  =(';' i.tub)  (sait s(tub nex))
    ?.  =('[' i.tub)  0
    =/  g  (gap s(tub nex))
    ?@  g  0
    =/  a  (tall g)
    ?@  a  a
    =/  l  (thak ']' p.a q.a)
    ?@  l  l
    [[%cltr p.l] q.l]
  ::                                                    ::  ++nels
  ++  nels                                              ::  count line ends
    |=  tub=tape
    ^-  @ud
    =/  n=@ud  0
    |-  ^-  @ud
    ?~  tub  n
    ?:  =(10 i.tub)  $(n +(n), tub t.tub)
    $(tub t.tub)
  ::                                                    ::  ++till
  ++  till                                              ::  spec, tall form
    |=  s=sat
    ^-  tez
    ?.  bug  (tilo s)
    =/  r  (tilo s)
    ?@  r  r
    [[%dbug [wer (hare s) (hare q.r)] p.r] q.r]
  ::                                                    ::  ++tilo
  ++  tilo                                              ::  till, no spot
    |=  s=sat
    ^-  tez
    =/  tub  tub.s
    =/  r=tez
      ?.  ?=([?(%'$' %'%') @ *] tub)  0
      ?:  ?=([%'(' *] t.t.tub)  0
      ?:  =('$' i.tub)  (tstr i.t.tub s(tub t.t.tub))
      (tstp i.t.tub s(tub t.t.tub))
    ?.  =(0 r)  r
    (wwyo s)
  ::                                                    ::  ++thak
  ++  thak                                              ::  more hoons, then ==
    |=  [end=@ h=hoon s=sat]                            ::  or ]; s: after h
    ^-  $@(@ [p=[i=hoon t=(list hoon)] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  n  (tall g)
    ?@  n
      ?.  =(0 n)  n
      =/  e  (stop end g)
      ?@  e  0
      [[h ~] e]
    =/  m  $(h p.n, s q.n)
    ?@  m  m
    [[h p.m] q.m]
  ::                                                    ::  ++stop
  ++  stop                                              ::  the closing == or ]
    |=  [end=@ s=sat]
    ^-  $@(@ sat)
    =/  tub  tub.s
    ?~  tub  0
    ?.  =(end i.tub)  0
    ?.  =('=' end)  s(tub t.tub)
    ?.  ?=([%'=' *] t.tub)  0
    s(tub t.t.tub)
  ::                                                    ::  ++thuk
  ++  thuk                                              ::  more specs, then ==
    |=  [h=spec s=sat]
    ^-  $@(@ [p=[i=spec t=(list spec)] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  n  (till g)
    ?@  n
      ?.  =(0 n)  n
      =/  e  (stop '=' g)
      ?@  e  0
      [[h ~] e]
    =/  m  $(h p.n, s q.n)
    ?@  m  m
    [[h p.m] q.m]
  ::                                                    ::  ++trik
  ++  trik                                              ::  wing hoon .. ==
    |=  s=sat
    ^-  $@(@ [p=(list (pair wing hoon)) q=sat])
    =/  i  (tpaw s)
    ?@  i  i
    =/  s  q.i
    =/  i  p.i
    |-  ^-  $@(@ [p=(list (pair wing hoon)) q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  n  (tpaw g)
    ?@  n
      ?.  =(0 n)  n
      =/  e  (stop '=' g)
      ?@  e  0
      [[i ~] e]
    =/  m  $(i p.n, s q.n)
    ?@  m  m
    [[i p.m] q.m]
  ::                                                    ::  ++tpaw
  ++  tpaw                                              ::  wing and hoon
    |=  s=sat
    ^-  $@(@ [p=(pair wing hoon) q=sat])
    =/  w  (rope tub.s)
    ?@  w  w
    =/  g  (gap s(tub q.w))
    ?@  g  0
    =/  h  (tall g)
    ?@  h  h
    [[p.w p.h] q.h]
  ::                                                    ::  ++truk
  ++  truk                                              ::  spec hoon .. ==
    |=  s=sat
    ^-  $@(@ [p=(list (pair spec hoon)) q=sat])
    =/  i  (tpas s)
    ?@  i  i
    =/  s  q.i
    =/  i  p.i
    |-  ^-  $@(@ [p=(list (pair spec hoon)) q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  n  (tpas g)
    ?@  n
      ?.  =(0 n)  n
      =/  e  (stop '=' g)
      ?@  e  0
      [[i ~] e]
    =/  m  $(i p.n, s q.n)
    ?@  m  m
    [[i p.m] q.m]
  ::                                                    ::  ++tpas
  ++  tpas                                              ::  spec and hoon
    |=  s=sat
    ^-  $@(@ [p=(pair spec hoon) q=sat])
    =/  w  (till s)
    ?@  w  w
    =/  g  (gap q.w)
    ?@  g  0
    =/  h  (tall g)
    ?@  h  h
    [[p.w p.h] q.h]
  ::
  ::    items of tall rune bodies
  ::
  ++  trop                                              ::  wing
    |=  s=sat
    ^-  $@(@ [p=wing q=sat])
    =/  w  (rope tub.s)
    ?@  w  w
    [p.w s(tub q.w)]
  ::
  ++  trpa                                              ::  wings by cols
    |=  s=sat
    ^-  $@(@ [p=(list wing) q=sat])
    =/  w  (rope tub.s)
    ?@  w  w
    =/  m  (ropa q.w)
    ?@  m  m
    [[p.w p.m] s(tub q.m)]
  ::
  ++  tsym                                              ::  term
    |=  s=sat
    ^-  $@(@ [p=term q=sat])
    =/  tub  tub.s
    ?~  tub  0
    ?.  &((gte i.tub 'a') (lte i.tub 'z'))  0
    =/  y  (sym tub)
    [p.y s(tub q.y)]
  ::
  ++  tbuc                                              ::  term or $
    |=  s=sat
    ^-  $@(@ [p=term q=sat])
    =/  tub  tub.s
    ?~  tub  0
    ?:  =('$' i.tub)  [%$ s(tub t.tub)]
    (tsym s)
  ::
  ++  tcen                                              ::  %term
    |=  s=sat
    ^-  $@(@ [p=term q=sat])
    =/  y  (cens tub.s)
    ?@  y  y
    [p.y s(tub q.y)]
  ::
  ++  twis                                              ::  skin of =/
    |=  s=sat
    ^-  $@(@ [p=skin q=sat])
    (wwis s)
  ::
  ++  tlor                                              ::  hoon as a skin
    |=  s=sat
    ^-  $@(@ [p=skin q=sat])
    =/  a  (tall s)
    ?@  a  a
    =/  rud  ~(flay ap p.a)
    ?~  rud  0
    [u.rud q.a]
  ::
  ++  tlom                                              ::  name, name=spec
    |=  s=sat
    ^-  $@(@ [p=[term (unit spec)] q=sat])
    =/  tub  tub.s
    ?~  tub  0
    ?.  &((gte i.tub 'a') (lte i.tub 'z'))  0
    =/  y  (sym tub)
    =/  nex  q.y
    ?.  ?=([%'=' *] nex)  [[p.y ~] s(tub nex)]
    =/  p  (wwye s(tub t.nex))
    ?@  p  ?:(=(0 p) [[p.y ~] s(tub nex)] p)
    [[p.y ~ p.p] q.p]
  ::
  ++  tbok                                              ::  jet signature
    |=  s=sat
    ^-  $@(@ [p=chum q=sat])
    (wbon s)
  ::
  ++  tbon                                              ::  %a, %a.b or %a.  b
    |=  s=sat
    ^-  $@(@ [p=$@(term [term hoon]) q=sat])
    =/  y  (cens tub.s)
    ?@  y  y
    =/  nex  q.y
    ?.  ?=([%'.' *] nex)  [p.y s(tub nex)]
    =/  w  (wwid s(tub t.nex))
    ?^  w  [[p.y p.w] q.w]
    ?.  =(0 w)  w
    =/  g  (gap s(tub t.nex))
    ?@  g  [p.y s(tub nex)]
    =/  h  (tall g)
    ?@  h  ?:(=(0 h) [p.y s(tub nex)] h)
    [[p.y p.h] q.h]
  ::
  ++  tbnz                                              ::  ~ or ==  %a  b  ==
    |=  s=sat
    ^-  $@(@ [p=tyre q=sat])
    =/  tub  tub.s
    ?~  tub  0
    ?:  =('~' i.tub)  [~ s(tub t.tub)]
    ?.  ?=([%'=' %'=' *] tub)  0
    =/  g  (gap s(tub t.t.tub))
    ?@  g  0
    |-  ^-  $@(@ [p=tyre q=sat])
    =/  y  (cens tub.g)
    ?@  y  y
    =/  f  (gap g(tub q.y))
    ?@  f  0
    =/  h  (tall f)
    ?@  h  h
    =/  f  (gap q.h)
    ?@  f  0
    =/  m  $(g f)
    ?^  m  [[[p.y p.h] p.m] q.m]
    ?.  =(0 m)  m
    =/  e  (stop '=' f)
    ?@  e  0
    [[[p.y p.h] ~] e]
  ::
  ++  thin                                              ::  n or [n m]
    |=  s=sat
    ^-  $@(@ [p=$@(@ [@ @]) q=sat])
    (whin s)
  ::
  ++  tlyn                                              ::  a, [a b] or a  b  ==
    |=  s=sat
    ^-  $@(@ [p=(lest term) q=sat])
    =/  tub  tub.s
    ?~  tub  0
    ?:  =('[' i.tub)
      =/  l  (lynq t.tub)
      ?@  l  l
      [p.l s(tub q.l)]
    ?.  &((gte i.tub 'a') (lte i.tub 'z'))  0
    =/  y  (sym tub)
    =/  one=[p=(lest term) q=sat]  [[p.y ~] s(tub q.y)]
    =/  r=$@(@ [p=(list term) q=sat])
      =/  e  q.one
      |-  ^-  $@(@ [p=(list term) q=sat])
      =/  g  (gap e)
      ?@  g  0
      =/  n  (tsym g)
      ?@  n
        =/  d  (stop '=' g)
        ?@  d  0
        [~ d]
      =/  m  $(e q.n)
      ?@  m  m
      [[p.n p.m] q.m]
    ?@  r  one
    [[p.y p.r] q.r]
  ::
  ++  teek                                              ::  wing or hoon to test
    |=  s=sat
    ^-  $@(@ [p=tiki q=sat])
    =/  w  (wtea s)
    ?.  =(0 w)  w
    =/  tub  tub.s
    =/  r=$@(@ [p=tiki q=sat])
      ?.  ?=([%'^' %'=' *] tub)  0
      =/  g  (gap s(tub t.t.tub))
      ?@  g  0
      =/  y  (tsym g)
      ?@  y  y
      =/  g  (gap q.y)
      ?@  g  0
      =/  w  (rope tub.g)
      ?^  w  [[%& [~ p.y] p.w] g(tub q.w)]
      ?.  =(0 w)  w
      =/  h  (tall g)
      ?@  h  h
      [[%| [~ p.y] p.h] q.h]
    ?.  =(0 r)  r
    =/  h  (tall s)
    ?@  h  h
    [[%| ~ p.h] q.h]
  ::
  ::    cores
  ::
  ++  boog                                              ::  ++ or +$ arm
    |=  s=sat
    ^-  $@(@ [p=[p=term q=hoon] q=sat])
    =/  tub  tub.s
    ?.  ?=([%'+' @ *] tub)  0
    ?:  =('+' i.t.tub)
      =/  g  (gap s(tub t.t.tub))
      ?@  g  0
      =/  y  (tbuc g)
      ?@  y  y
      =/  g  (gap q.y)
      ?@  g  0
      =/  h  (tall g)
      ?@  h  h
      [[p.y p.h] q.h]
    ?.  =('$' i.t.tub)  0
    =/  g  (gap s(tub t.t.tub))
    ?@  g  0
    =/  y  (tsym g)
    ?@  y  y
    =/  g  (gap q.y)
    ?@  g  0
    =/  h  (till g)
    ?@  h  h
    [[p.y [%ktcl %name p.y p.h]] q.h]
  ::
  ++  whap                                              ::  arms, to a map
    |=  s=sat
    ^-  $@(@ [p=(map term hoon) q=sat])
    =/  b  (boog s)
    ?@  b  b
    =/  m=(map term hoon)  [[p.p.b q.p.b] ~ ~]
    =/  s  q.b
    |-  ^-  $@(@ [p=(map term hoon) q=sat])
    =/  g  (gap s)
    ?@  g  [m s]
    =/  b  (boog g)
    ?@  b  ?:(=(0 b) [m s] b)
    =/  gen=hoon
      ?.  (~(has by m) p.p.b)  q.p.b
      [%eror (weld "duplicate arm: +" (trip p.p.b))]
    $(m (~(put by m) p.p.b gen), s q.b)
  ::
  ++  whip                                              ::  +| chapters
    |=  s=sat
    ^-  $@(@ [p=(list (pair term (map term hoon))) q=sat])
    =/  tub  tub.s
    ?.  ?=([%'+' %'|' *] tub)  0
    =/  g  (gap s(tub t.t.tub))
    ?@  g  0
    =/  y  (tcen g)
    ?@  y  y
    =/  g  (gap q.y)
    ?@  g  0
    =/  w  (whap g)
    ?@  w  w
    =/  g  (gap q.w)
    ?@  g  [[[p.y p.w] ~] q.w]
    =/  m  $(s g)
    ?@  m  ?:(=(0 m) [[[p.y p.w] ~] q.w] m)
    [[[p.y p.w] p.m] q.m]
  ::
  ++  wisp                                              ::  core tail
    |=  s=sat
    ^-  $@(@ [p=(map term tome) q=sat])
    =/  tub  tub.s
    ?:  ?=([%'-' %'-' *] tub)  [~ s(tub t.t.tub)]
    =/  r=$@(@ [p=(list (pair term (map term hoon))) q=sat])
      ?:  ?=([%'+' %'|' *] tub)  (whip s)
      =/  w  (whap s)
      ?@  w  w
      [[[%$ p.w] ~] q.w]
    ?@  r  r
    =/  g  (gap q.r)
    ?@  g  0
    ?.  ?=([%'-' %'-' *] tub.g)  0
    [(chap p.r) g(tub t.t.tub.g)]
  ::
  ++  chap                                              ::  chapters, to a map
    |=  a=(list (pair term (map term hoon)))
    ^-  (map term tome)
    ?~  a  ~
    ?~  t.a  [[p.i.a q.i.a] ~ ~]
    =/  c=(list (pair term (map term hoon)))  a
    =<  p
    |-  ^-  (pair (map term tome) (map term hoon))
    ?~  c  [~ ~]
    =/  mor  $(c t.c)
    =.  q.i.c
      %-  ~(urn by q.i.c)
      |=  b=(pair term hoon)  ^+  +.b
      ?.  (~(has by q.mor) p.b)  +.b
      [%eror (weld "duplicate arm: +" (trip p.b))]
    :_  (~(uni by q.mor) q.i.c)
    %+  ~(put by p.mor)
      p.i.c
    ?.  (~(has by p.mor) p.i.c)
      q.i.c
    [[%$ [%eror (weld "duplicate chapter: |" (trip p.i.c))]] ~ ~]
  ::
  ++  wasp                                              ::  +* aliases, if any
    |=  s=sat
    ^-  $@(@ [p=alas q=sat])
    =/  tub  tub.s
    ?.  ?=([%'+' %'*' *] tub)  [~ s]
    =/  r=$@(@ [p=alas q=sat])
      =/  g  (gap s(tub t.t.tub))
      ?@  g  0
      (wasq g)
    ?@  r  ?:(=(0 r) [~ s] r)
    =/  g  (gap q.r)
    ?@  g  [~ s]
    [p.r g]
  ::
  ++  wasq                                              ::  name hoon ..
    |=  s=sat
    ^-  $@(@ [p=alas q=sat])
    =/  y  (tsym s)
    ?@  y  y
    =/  g  (gap q.y)
    ?@  g  0
    =/  h  (tall g)
    ?@  h  h
    =/  g  (gap q.h)
    ?@  g  [[[p.y p.h] ~] q.h]
    =/  m  $(s g)
    ?@  m  ?:(=(0 m) [[[p.y p.h] ~] q.h] m)
    [[[p.y p.h] p.m] q.m]
  ::
  ::    sail, tall form
  ::
  ++  runt                                              ::  xy(..) at a tape
    |=  tub=tape
    ^-  res
    ?.  ?=([@ @ %'(' *] tub)  0
    (rune i.tub i.t.tub t.t.t.tub)
  ::
  ++  sin1  |=(tub=tape (sinn & & tub))                 ::  tall text
  ++  squ1  |=(tub=tape (squo & tub))                   ::  quote in a tall top
  ++  sait                                              ::  tall sail, after mic
    |=  s=sat
    ^-  tes
    =/  t  (stot s)
    ?@  t  t
    :_  q.t
    ?-  -.p.t
      %&  [%xray p.p.t]
      %|  [%mcts p.p.t]
    ==
  ::
  ++  stot                                              ::  one tall top
    |=  s=sat
    ^-  $@(@ [p=(each manx:hoot marl:hoot) q=sat])
    =/  tub  tub.s
    ?:  ?=([%' ' *] tub)
      =/  t=tape  t.tub
      =.  t  |-(?:(?=([%' ' *] t) $(t t.t) t))
      =/  i  (wsin s(tub t))
      ?@  i  i
      [[%| (coll & p.i)] q.i]
    =/  r=$@(@ [p=(each manx:hoot marl:hoot) q=sat])
      =/  n=$@(~ [p=@tas q=tape])
        ?:  ?=([%'s' %'c' %'r' %'i' %'p' %'t' *] tub)
          [%script t.t.t.t.t.t.tub]
        ?:  ?=([%'s' %'t' %'y' %'l' %'e' *] tub)  [%style t.t.t.t.t.tub]
        ~
      ?~  n  0
      =/  a  (wsat s(tub q.n))
      ?@  a  a
      =/  l  (ssty q.a)
      ?@  l  l
      [[%& [p.n p.a] p.l] q.l]
    ?.  =(0 r)  r
    =/  r=$@(@ [p=(each manx:hoot marl:hoot) q=sat])
      =/  h  (wshe s)
      ?@  h  h
      =/  a  (stat q.h)
      ?@  a  a
      =/  t  (stta q.a)
      ?@  t  t
      [[%& [n.p.h (weld a.p.h p.a)] p.t] q.t]
    ?.  =(0 r)  r
    =/  r=$@(@ [p=(each manx:hoot marl:hoot) q=sat])
      ?.  ?=([%'"' *] tub)  0
      =/  q  (wsqu s)
      ?@  q  q
      [[%| p.q] q.q]
    ?.  =(0 r)  r
    =/  non=[p=(each manx:hoot marl:hoot) q=sat]  [[%| (tnod "\0a") ~] s]
    ?~  tub  non
    =/  c  i.tub
    =/  r=$@(@ [p=(each manx:hoot marl:hoot) q=sat])
      ?.  =('=' c)  0
      =/  t  (stta s(tub t.tub))
      ?@  t  t
      [[%| p.t] q.t]
    ?.  =(0 r)  r
    =/  r=$@(@ [p=(each manx:hoot marl:hoot) q=sat])
      ?.  =('>' c)  0
      =/  g  (gap s(tub t.tub))
      ?@  g  0
      =/  m  (scra g)
      ?@  m  m
      [[%& [%div ~] p.m] q.m]
    ?.  =(0 r)  r
    =/  r=$@(@ [p=(each manx:hoot marl:hoot) q=sat])
      ?.  |(=('-' c) =('+' c) =('*' c) =('%' c))  0
      =/  g  (gap s(tub t.tub))
      ?@  g  0
      =/  h  (tall g)
      ?@  h  h
      :_  q.h
      :-  %|
      :_  ~
      ?:  =('-' c)  [%tape p.h]
      ?:  =('+' c)  [%manx p.h]
      ?:  =('*' c)  [%marl p.h]
      [%call p.h]
    ?.  =(0 r)  r
    non
  ::
  ++  stat                                              ::  =name  value lines
    |=  s=sat
    ^-  $@(@ [p=mart:hoot q=sat])
    =/  g  (gap s)
    ?@  g  [~ s]
    ?.  ?=([%'=' *] tub.g)  [~ s]
    =/  n  (sman t.tub.g)
    ?@  n  [~ s]
    =/  f  (gap g(tub q.n))
    ?@  f  [~ s]
    =/  h  (wsho f)
    ?@  h  ?:(=(0 h) [~ s] h)
    =/  m  $(s q.h)
    ?@  m  m
    [[[p.n p.h] p.m] q.m]
  ::
  ++  stta                                              ::  tail of a tall tag
    |=  s=sat
    ^-  $@(@ [p=marl:hoot q=sat])
    =/  tub  tub.s
    ?:  ?=([%';' *] tub)  [~ s(tub t.tub)]
    =/  r=$@(@ [p=marl:hoot q=sat])
      ?.  ?=([%':' *] tub)  0
      =/  w  (wswr s(tub t.tub))
      ?.  =(0 w)  w
      ?.  ?=([%' ' *] t.tub)  0
      =/  i  (wsin s(tub t.t.tub))
      ?@  i  i
      [(coll | p.i) q.i]
    ?.  =(0 r)  r
    =/  g  (gap s)
    ?@  g  0
    (skid g)
  ::
  ++  skid                                              ::  child tops, then ==
    |=  s=sat
    ^-  $@(@ [p=marl:hoot q=sat])
    =/  tub  tub.s
    =/  k=$@(@ [p=marl:hoot q=sat])
      ?:  ?=([%'=' %'=' *] tub)  0
      ?.  ?=([%';' *] tub)  (scra s)
      =/  t  (stot s(tub t.tub))
      ?@  t  t
      [(drop p.t) q.t]
    ?@  k  k
    =/  g  (gap q.k)
    ?@  g  0
    =/  m  $(s g)
    ?^  m  [(weld p.k p.m) q.m]
    ?.  =(0 m)  m
    ?.  ?=([%'=' %'=' *] tub.g)  0
    [p.k g(tub t.t.tub.g)]
  ::
  ++  scra                                              ::  markdown, by
    |=  s=sat                                           ::  cram:vast
    ^-  $@(@ [p=marl:hoot q=sat])
    =/  k=@ud  16
    |-  ^-  $@(@ [p=marl:hoot q=sat])
    =/  j  (lump k s)
    =/  mex  (mule |.((cram:vaz [(hare s) all.j])))
    ?.  ?=(%& -.mex)  1
    =/  vex  p.mex
    ?~  q.vex
      ?:  =(%eof end.j)  0
      ?:(out.j 2 $(k (mul 2 k)))
    =/  r  q.q.u.q.vex
    ?.  &(?=(~ r) !=(%eof end.j))
      [p.u.q.vex (seat s all.j r)]
    ?:(out.j 2 $(k (mul 2 k)))
  ::
  ++  ssty                                              ::  script or style lines
    |=  s=sat
    ^-  $@(@ [p=marl:hoot q=sat])
    =/  g  (gap s)
    ?@  g  0
    |-  ^-  $@(@ [p=marl:hoot q=sat])
    =/  tub  tub.g
    ?.  ?=([%';' *] tub)  0
    =/  l=[p=tape q=tape]
      ?.  ?=([%' ' *] t.tub)  ["\0a" t.tub]
      (prnt t.t.tub)
    =/  f  (gap g(tub q.l))
    ?@  f  0
    ?:  ?=([%';' *] tub.f)
      =/  m  $(g f)
      ?@  m  m
      [[(tnod p.l) p.m] q.m]
    ?.  ?=([%'=' %'=' *] tub.f)  0
    [[(tnod p.l) ~] f(tub t.t.tub.f)]
  ::
  ++  texa                                              ::  one hoon
    |=  s=sat
    ^-  $@(@ [p=hoon q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tall g)
    ?@  a  a
    a
  ::
  ++  texb                                              ::  two hoons
    |=  s=sat
    ^-  $@(@ [p=[hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tall g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  texc                                              ::  three hoons
    |=  s=sat
    ^-  $@(@ [p=[hoon hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tall g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (tall g)
    ?@  c  c
    [[p.a p.b p.c] q.c]
  ::
  ++  texd                                              ::  four hoons
    |=  s=sat
    ^-  $@(@ [p=[hoon hoon hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tall g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (tall g)
    ?@  c  c
    =/  g  (gap q.c)
    ?@  g  0
    =/  d  (tall g)
    ?@  d  d
    [[p.a p.b p.c p.d] q.d]
  ::
  ++  texf                                              ::  %term and hoon
    |=  s=sat
    ^-  $@(@ [p=[term hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tcen g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  texg                                              ::  name, two hoons
    |=  s=sat
    ^-  $@(@ [p=[[term (unit spec)] hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tlom g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (tall g)
    ?@  c  c
    [[p.a p.b p.c] q.c]
  ::
  ++  texj                                              ::  skin and hoon
    |=  s=sat
    ^-  $@(@ [p=[skin hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tlor g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  texo                                              ::  skin, two hoons
    |=  s=sat
    ^-  $@(@ [p=[skin hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (twis g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (tall g)
    ?@  c  c
    [[p.a p.b p.c] q.c]
  ::
  ++  texq                                              ::  wing, two hoons
    |=  s=sat
    ^-  $@(@ [p=[wing hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (trop g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (tall g)
    ?@  c  c
    [[p.a p.b p.c] q.c]
  ::
  ++  text                                              ::  skin, wing, hoons
    |=  s=sat
    ^-  $@(@ [p=[skin wing hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (twis g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (trop g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (tall g)
    ?@  c  c
    =/  g  (gap q.c)
    ?@  g  0
    =/  d  (tall g)
    ?@  d  d
    [[p.a p.b p.c p.d] q.d]
  ::
  ++  texw                                              ::  wing, three hoons
    |=  s=sat
    ^-  $@(@ [p=[wing hoon hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (trop g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (tall g)
    ?@  c  c
    =/  g  (gap q.c)
    ?@  g  0
    =/  d  (tall g)
    ?@  d  d
    [[p.a p.b p.c p.d] q.d]
  ::
  ++  texx                                              ::  wings, two hoons
    |=  s=sat
    ^-  $@(@ [p=[(list wing) hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (trpa g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (tall g)
    ?@  c  c
    [[p.a p.b p.c] q.c]
  ::
  ++  texz                                              ::  spec, three hoons
    |=  s=sat
    ^-  $@(@ [p=[spec hoon hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (till g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (tall g)
    ?@  c  c
    =/  g  (gap q.c)
    ?@  g  0
    =/  d  (tall g)
    ?@  d  d
    [[p.a p.b p.c p.d] q.d]
  ::
  ++  teqa                                              ::  one spec
    |=  s=sat
    ^-  $@(@ [p=spec q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (till g)
    ?@  a  a
    a
  ::
  ++  teqb                                              ::  two specs
    |=  s=sat
    ^-  $@(@ [p=[spec spec] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (till g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (till g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  teqc                                              ::  spec then hoon
    |=  s=sat
    ^-  $@(@ [p=[spec hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (till g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  teqd                                              ::  hoon then spec
    |=  s=sat
    ^-  $@(@ [p=[hoon spec] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tall g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (till g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  teqe                                              ::  names and a spec
    |=  s=sat
    ^-  $@(@ [p=[(lest term) spec] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tlyn g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (till g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  teqg                                              ::  term and spec
    |=  s=sat
    ^-  $@(@ [p=[term spec] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tsym g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (till g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  teqx                                              ::  hoon, two specs
    |=  s=sat
    ^-  $@(@ [p=[hoon spec spec] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tall g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (till g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (till g)
    ?@  c  c
    [[p.a p.b p.c] q.c]
  ::
  ++  teqy                                              ::  hoon, three specs
    |=  s=sat
    ^-  $@(@ [p=[hoon spec spec spec] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tall g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (till g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (till g)
    ?@  c  c
    =/  g  (gap q.c)
    ?@  g  0
    =/  d  (till g)
    ?@  d  d
    [[p.a p.b p.c p.d] q.d]
  ::
  ++  tinb                                              ::  hint and hoon
    |=  s=sat
    ^-  $@(@ [p=[$@(term [term hoon]) hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tbon g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  tind                                              ::  jet, hoon, hooks, hoon
    |=  s=sat
    ^-  $@(@ [p=[chum hoon tyre hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tbok g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (tbnz g)
    ?@  c  c
    =/  g  (gap q.c)
    ?@  g  0
    =/  d  (tall g)
    ?@  d  d
    [[p.a p.b p.c p.d] q.d]
  ::
  ++  tine                                              ::  jet and hoon
    |=  s=sat
    ^-  $@(@ [p=[chum hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tbok g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  tinh                                              ::  1/2 numbers, hoon
    |=  s=sat
    ^-  $@(@ [p=[$@(@ [@ @]) hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (thin g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  tkts                                              ::  spec and test
    |=  s=sat
    ^-  $@(@ [p=[spec tiki] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (till g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (teek g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  tkhx                                              ::  skin and test
    |=  s=sat
    ^-  $@(@ [p=[skin tiki] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tlor g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (teek g)
    ?@  b  b
    [[p.a p.b] q.b]
  ::
  ++  tk3h                                              ::  test, two hoons
    |=  s=sat
    ^-  $@(@ [p=[tiki hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (teek g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    =/  g  (gap q.b)
    ?@  g  0
    =/  c  (tall g)
    ?@  c  c
    [[p.a p.b p.c] q.c]
  ::
  ++  texi                                              ::  two or more hoons, ==
    |=  s=sat
    ^-  $@(@ [p=[i=hoon t=(list hoon)] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tall g)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  b  (tall g)
    ?@  b  b
    =/  l  (thak '=' p.b q.b)
    ?@  l  l
    [[p.a p.l] q.l]
  ::
  ++  texs                                              ::  hoons, ==
    |=  s=sat
    ^-  $@(@ [p=[i=hoon t=(list hoon)] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (tall g)
    ?@  a  a
    (thak '=' p.a q.a)
  ::
  ++  teqs                                              ::  specs, ==
    |=  s=sat
    ^-  $@(@ [p=[i=spec t=(list spec)] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  a  (till g)
    ?@  a  a
    (thuk p.a q.a)
  ::
  ++  teqz                                              ::  hoon, specs, ==
    |=  s=sat
    ^-  $@(@ [p=[hoon i=spec t=(list spec)] q=sat])
    =/  a  (texa s)
    ?@  a  a
    =/  l  (teqs q.a)
    ?@  l  l
    [[p.a p.l] q.l]
  ::
  ++  teqn                                              ::  spec, hoons as %cltr
    |=  s=sat
    ^-  $@(@ [p=[spec %cltr (list hoon)] q=sat])
    =/  a  (teqa s)
    ?@  a  a
    =/  l  (texs q.a)
    ?@  l  l
    [[p.a %cltr p.l] q.l]
  ::
  ++  texh                                              ::  wing, changes, ==
    |=  s=sat
    ^-  $@(@ [p=[wing (list (pair wing hoon))] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  w  (rope tub.g)
    ?@  w  w
    =/  g  (gap g(tub q.w))
    ?@  g  0
    =/  k  (trik g)
    ?@  k  k
    [[p.w p.k] q.k]
  ::
  ++  texm                                              ::  wing, hoon, changes
    |=  s=sat
    ^-  $@(@ [p=[wing hoon (list (pair wing hoon))] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  w  (rope tub.g)
    ?@  w  w
    =/  a  (texa g(tub q.w))
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  k  (trik g)
    ?@  k  k
    [[p.w p.a p.k] q.k]
  ::
  ++  texp                                              ::  changes, ==, hoon
    |=  s=sat
    ^-  $@(@ [p=[(list (pair wing hoon)) hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  k  (trik g)
    ?@  k  k
    =/  a  (texa q.k)
    ?@  a  a
    [[p.k p.a] q.a]
  ::
  ++  texr                                              ::  hoon and core tail
    |=  s=sat
    ^-  $@(@ [p=[hoon (map term tome)] q=sat])
    =/  a  (texa s)
    ?@  a  a
    =/  w  (texe q.a)
    ?@  w  w
    [[p.a p.w] q.w]
  ::
  ++  texe                                              ::  core tail
    |=  s=sat
    ^-  $@(@ [p=(map term tome) q=sat])
    =/  g  (gap s)
    ?@  g  0
    (wisp g)
  ::
  ++  teqr                                              ::  spec, aliases, tail
    |=  s=sat
    ^-  $@(@ [p=[spec alas (map term tome)] q=sat])
    =/  a  (teqa s)
    ?@  a  a
    =/  g  (gap q.a)
    ?@  g  0
    =/  l  (wasp g)
    ?@  l  l
    =/  w  (wisp q.l)
    ?@  w  w
    [[p.a p.l p.w] q.w]
  ::
  ++  tinc                                              ::  optional =s, hoon
    |=  s=sat
    ^-  $@(@ [p=[@ hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  n  (tiss tub.g)
    =/  r=$@(@ [p=[@ hoon] q=sat])
      ?:  =(0 p.n)  0
      =/  a  (texa g(tub q.n))
      ?@  a  a
      [[p.n p.a] q.a]
    ?.  =(0 r)  r
    =/  a  (tall g)
    ?@  a  a
    [[0 p.a] q.a]
  ::
  ++  tinf                                              ::  0-3 >s, two hoons
    |=  s=sat
    ^-  $@(@ [p=[@ hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  n  (garz tub.g)
    =/  r=$@(@ [p=[@ hoon hoon] q=sat])
      ?:  =(0 p.n)  0
      =/  b  (texb g(tub q.n))
      ?@  b  b
      [[p.n p.b] q.b]
    ?.  =(0 r)  r
    =/  a  (tall g)
    ?@  a  a
    =/  b  (texa q.a)
    ?@  b  b
    [[0 p.a p.b] q.b]
  ::
  ++  ting                                              ::  0-3 >s, three hoons
    |=  s=sat
    ^-  $@(@ [p=[@ hoon hoon hoon] q=sat])
    =/  g  (gap s)
    ?@  g  0
    =/  n  (garz tub.g)
    =/  r=$@(@ [p=[@ hoon hoon hoon] q=sat])
      ?:  =(0 p.n)  0
      =/  b  (texc g(tub q.n))
      ?@  b  b
      [[p.n p.b] q.b]
    ?.  =(0 r)  r
    =/  a  (tall g)
    ?@  a  a
    =/  b  (texb q.a)
    ?@  b  b
    [[0 p.a p.b] q.b]
  ::
  ++  garz                                              ::  up to three gars
    |=  tub=tape
    ^-  [p=@ q=tape]
    =/  n=@  0
    |-  ^-  [p=@ q=tape]
    ?:  &((lth n 3) ?=([%'>' *] tub))  $(n +(n), tub t.tub)
    [n tub]
  ::
  ++  thxp                                              ::  test, cases, ==
    |=  s=sat
    ^-  tes
    =/  g  (gap s)
    ?@  g  0
    =/  t  (teek g)
    ?@  t  t
    =/  g  (gap q.t)
    ?@  g  0
    =/  k  (truk g)
    ?@  k  k
    [(gray p.t [%wthp (puce p.t) (bleu p.t p.k)]) q.k]
  ::
  ++  thls                                              ::  test, hoon, cases, ==
    |=  s=sat
    ^-  tes
    =/  g  (gap s)
    ?@  g  0
    =/  t  (teek g)
    ?@  t  t
    =/  h  (texa q.t)
    ?@  h  h
    =/  g  (gap q.h)
    ?@  g  0
    =/  k  (truk g)
    ?@  k  k
    [(gray p.t [%wtls (puce p.t) (blue p.t p.h) (bleu p.t p.k)]) q.k]
  ::                                                    ::  ++tune
  ++  tune                                              ::  tall rune, after xy
    |=  [a=@ b=@ s=sat]
    ^-  tes
    ?+    a  0
        %'?'
      ?+    b  0
          %':'  =/(r (texc s) ?@(r r [[%wtcl p.r] q.r]))
          %'.'  =/(r (texc s) ?@(r r [[%wtdt p.r] q.r]))
          %'>'  =/(r (texb s) ?@(r r [[%wtgr p.r] q.r]))
          %'<'  =/(r (texb s) ?@(r r [[%wtgl p.r] q.r]))
          %'&'  =/(r (texs s) ?@(r r [[%wtpm p.r] q.r]))
          %'|'  =/(r (texs s) ?@(r r [[%wtbr p.r] q.r]))
          %'!'  =/(r (texa s) ?@(r r [[%wtzp p.r] q.r]))
          %'-'  (thxp s)
          %'+'  (thls s)
          %'~'  =/(r (tk3h s) ?@(r r [(tiky %wtsg p.r) q.r]))
          %'^'  =/(r (tk3h s) ?@(r r [(tiky %wtkt p.r) q.r]))
          %'@'  =/(r (tk3h s) ?@(r r [(tiky %wtpt p.r) q.r]))
            %'='
          =/  r  (tkts s)
          ?@  r  r
          =/  t  +.p.r
          [(gray t [%wtts (teal t -.p.r) (puce t)]) q.r]
        ::
            %'#'
          =/  r  (tkhx s)
          ?@  r  r
          =/  t  +.p.r
          [(gray t [%wthx (tele t -.p.r) (puce t)]) q.r]
      ==
    ::
        %'='
      ?+    b  0
          %'>'  =/(r (texb s) ?@(r r [[%tsgr p.r] q.r]))
          %'<'  =/(r (texb s) ?@(r r [[%tsgl p.r] q.r]))
          %'+'  =/(r (texb s) ?@(r r [[%tsls p.r] q.r]))
          %'-'  =/(r (texb s) ?@(r r [[%tshp p.r] q.r]))
          %','  =/(r (texb s) ?@(r r [[%tscm p.r] q.r]))
          %'|'  =/(r (teqc s) ?@(r r [[%tsbr p.r] q.r]))
          %'.'  =/(r (texq s) ?@(r r [[%tsdt p.r] q.r]))
          %'?'  =/(r (texw s) ?@(r r [[%tswt p.r] q.r]))
          %'^'  =/(r (text s) ?@(r r [[%tskt p.r] q.r]))
          %':'  =/(r (texp s) ?@(r r [[%tscl p.r] q.r]))
          %'/'  =/(r (texo s) ?@(r r [[%tsfs p.r] q.r]))
          %';'  =/(r (texo s) ?@(r r [[%tsmc p.r] q.r]))
          %'*'  =/(r (texg s) ?@(r r [[%tstr p.r] q.r]))
          %'~'  =/(r (texi s) ?@(r r [[%tssg p.r] q.r]))
      ==
    ::
        %'|'
      ?+    b  0
          %'='  =/(r (teqc s) ?@(r r [[%brts p.r] q.r]))
          %'*'  =/(r (teqc s) ?@(r r [[%brtr p.r] q.r]))
          %'~'  =/(r (teqc s) ?@(r r [[%brsg p.r] q.r]))
          %'.'  =/(r (texa s) ?@(r r [[%brdt p.r] q.r]))
          %'-'  =/(r (texa s) ?@(r r [[%brhp p.r] q.r]))
          %'?'  =/(r (texa s) ?@(r r [[%brwt p.r] q.r]))
          %':'  =/(r (texb s) ?@(r r [[%brcl p.r] q.r]))
          %'$'  =/(r (teqe s) ?@(r r [[%brbc p.r] q.r]))
          %'^'  =/(r (texr s) ?@(r r [[%brkt p.r] q.r]))
          %'_'  =/(r (teqr s) ?@(r r [[%brcb p.r] q.r]))
          %'%'  =/(r (texe s) ?@(r r [[%brcn ~ p.r] q.r]))
          %'@'  =/(r (texe s) ?@(r r [[%brpt ~ p.r] q.r]))
      ==
    ::
        %';'
      ?+    b  0
          %'~'  =/(r (texi s) ?@(r r [[%mcsg p.r] q.r]))
          %':'  =/(r (texi s) ?@(r r [[%mccl p.r] q.r]))
          %';'  =/(r (teqc s) ?@(r r [[%mcmc p.r] q.r]))
          %'/'  =/(r (texa s) ?@(r r [[%mcfs p.r] q.r]))
          %'<'  =/(r (texz s) ?@(r r [[%mcgl p.r] q.r]))
      ==
    ::
        %'%'
      ?+    b  0
          %'-'  =/(r (texb s) ?@(r r [[%cnhp p.r] q.r]))
          %'+'  =/(r (texc s) ?@(r r [[%cnls p.r] q.r]))
          %'^'  =/(r (texd s) ?@(r r [[%cnkt p.r] q.r]))
          %'.'  =/(r (texb s) ?@(r r [[%cndt p.r] q.r]))
          %':'  =/(r (texi s) ?@(r r [[%cncl p.r] q.r]))
          %'='  =/(r (texh s) ?@(r r [[%cnts p.r] q.r]))
          %'_'  =/(r (texh s) ?@(r r [[%cncb p.r] q.r]))
          %'*'  =/(r (texm s) ?@(r r [[%cntr p.r] q.r]))
            %'~'
          =/  r  (texq s)
          ?@  r  r
          [[%cnsg -.p.r +<.p.r +>.p.r ~] q.r]
      ==
    ::
        %':'
      ?+    b  0
          %'-'  =/(r (texb s) ?@(r r [[%clhp p.r] q.r]))
          %'_'  =/(r (texb s) ?@(r r [[%clcb p.r] q.r]))
          %'+'  =/(r (texc s) ?@(r r [[%clls p.r] q.r]))
          %'^'  =/(r (texd s) ?@(r r [[%clkt p.r] q.r]))
          %'~'  =/(r (texs s) ?@(r r [[%clsg p.r] q.r]))
          %'*'  =/(r (texs s) ?@(r r [[%cltr p.r] q.r]))
      ==
    ::
        %'^'
      ?+    b  0
          %'-'  =/(r (teqc s) ?@(r r [[%kthp p.r] q.r]))
          %'+'  =/(r (texb s) ?@(r r [[%ktls p.r] q.r]))
          %'.'  =/(r (texb s) ?@(r r [[%ktdt p.r] q.r]))
          %'_'  =/(r (texb s) ?@(r r [[%ktcb p.r] q.r]))
          %'='  =/(r (texj s) ?@(r r [[%ktts p.r] q.r]))
          %'|'  =/(r (texa s) ?@(r r [[%ktbr p.r] q.r]))
          %'&'  =/(r (texa s) ?@(r r [[%ktpm p.r] q.r]))
          %'~'  =/(r (texa s) ?@(r r [[%ktsg p.r] q.r]))
          %'?'  =/(r (texa s) ?@(r r [[%ktwt p.r] q.r]))
          %'*'  =/(r (teqa s) ?@(r r [[%kttr p.r] q.r]))
          %':'  =/(r (teqa s) ?@(r r [[%ktcl p.r] q.r]))
      ==
    ::
        %'~'
      ?+    b  0
          %'|'  =/(r (texb s) ?@(r r [[%sgbr p.r] q.r]))
          %'_'  =/(r (texb s) ?@(r r [[%sgcb p.r] q.r]))
          %'='  =/(r (texb s) ?@(r r [[%sgts p.r] q.r]))
          %'!'  =/(r (texb s) ?@(r r [[%sgzp p.r] q.r]))
          %'>'  =/(r (tinb s) ?@(r r [[%sggr p.r] q.r]))
          %'<'  =/(r (tinb s) ?@(r r [[%sggl p.r] q.r]))
          %'$'  =/(r (texf s) ?@(r r [[%sgbc p.r] q.r]))
          %'%'  =/(r (tind s) ?@(r r [[%sgcn p.r] q.r]))
          %'/'  =/(r (tine s) ?@(r r [[%sgfs p.r] q.r]))
          %'+'  =/(r (tinc s) ?@(r r [[%sgls p.r] q.r]))
          %'&'  =/(r (tinf s) ?@(r r [[%sgpm p.r] q.r]))
          %'?'  =/(r (ting s) ?@(r r [[%sgwt p.r] q.r]))
      ==
    ::
        %'.'
      ?+    b  0
          %'+'  =/(r (texa s) ?@(r r [[%dtls p.r] q.r]))
          %'?'  =/(r (texa s) ?@(r r [[%dtwt p.r] q.r]))
          %'*'  =/(r (texb s) ?@(r r [[%dttr p.r] q.r]))
          %'='  =/(r (texb s) ?@(r r [[%dtts p.r] q.r]))
          %'^'  =/(r (teqn s) ?@(r r [[%dtkt p.r] q.r]))
      ==
    ::
        %'!'
      ?+    b  0
          %'>'  =/(r (texa s) ?@(r r [[%zpgr p.r] q.r]))
          %'='  =/(r (texa s) ?@(r r [[%zpts p.r] q.r]))
          %','  =/(r (texb s) ?@(r r [[%zpcm p.r] q.r]))
          %';'  =/(r (texb s) ?@(r r [[%zpmc p.r] q.r]))
          %'<'  =/(r (teqc s) ?@(r r [[%zpgl p.r] q.r]))
          %'@'  =/(r (texx s) ?@(r r [[%zppt p.r] q.r]))
          %'?'  =/(r (tinh s) ?@(r r [[%zpwt p.r] q.r]))
            %':'
          =/  g  (gap s)
          ?@  g  0
          (tall(bug &) g)
        ::
            %'.'
          =/  g  (gap s)
          ?@  g  0
          (tall(bug |) g)
      ==
    ::
        %'$'
      ?:  =('.' b)  =/(r (teqa s) ?@(r r [[%kttr p.r] q.r]))
      ?:  =(',' b)  =/(r (teqa s) ?@(r r [[%ktcl p.r] q.r]))
      ?:  =(';' b)  0
      =/  r  (tstr b s)
      ?@  r  r
      [[%ktcl p.r] q.r]
    ==
  ::                                                    ::  ++tiky
  ++  tiky                                              ::  ?~ ?^ ?@ of a test
    |=  [tag=?(%wtsg %wtkt %wtpt) tik=tiki sic=hoon non=hoon]
    ^-  hoon
    =/  wig  (puce tik)
    =/  [a=hoon b=hoon]  [(blue tik sic) (blue tik non)]
    %+  gray  tik
    ?-  tag
      %wtsg  [%wtsg wig a b]
      %wtkt  [%wtkt wig a b]
      %wtpt  [%wtpt wig a b]
    ==
  ::                                                    ::  ++tstr
  ++  tstr                                              ::  tall $x, after it
    |=  [b=@ s=sat]
    ^-  tez
    ?+  b  0
      %':'  =/(r (teqs s) ?@(r r [[%bccl p.r] q.r]))
      %'%'  =/(r (teqs s) ?@(r r [[%bccn p.r] q.r]))
      %'?'  =/(r (teqs s) ?@(r r [[%bcwt p.r] q.r]))
      %'<'  =/(r (teqb s) ?@(r r [[%bcgl p.r] q.r]))
      %'>'  =/(r (teqb s) ?@(r r [[%bcgr p.r] q.r]))
      %'^'  =/(r (teqb s) ?@(r r [[%bckt p.r] q.r]))
      %'@'  =/(r (teqb s) ?@(r r [[%bcpt p.r] q.r]))
      %'-'  =/(r (teqb s) ?@(r r [[%bchp p.r] q.r]))
      %'~'  =/(r (teqd s) ?@(r r [[%bcsg p.r] q.r]))
      %'|'  =/(r (teqc s) ?@(r r [[%bcbr p.r] q.r]))
      %'&'  =/(r (teqc s) ?@(r r [[%bcpm p.r] q.r]))
      %'_'  =/(r (texa s) ?@(r r [[%bccb p.r] q.r]))
      %';'  =/(r (texa s) ?@(r r [[%bcmc p.r] q.r]))
      %'='  =/(r (teqg s) ?@(r r [[%bcts p.r] q.r]))
      %'+'  =/(r (teqg s) ?@(r r [[%bcls p.r] q.r]))
    ==
  ::                                                    ::  ++tstp
  ++  tstp                                              ::  tall %x, after it
    |=  [b=@ s=sat]
    ^-  tez
    ?+    b  0
        %'-'
      =/  r  (teqd s)
      ?@  r  r
      [[%make -.p.r +.p.r ~] q.r]
    ::
        %'.'
      =/  r  (teqc s)
      ?@  r  r
      [[%make +.p.r -.p.r ~] q.r]
    ::
        %'+'
      =/  r  (teqx s)
      ?@  r  r
      [[%make -.p.r +<.p.r +>.p.r ~] q.r]
    ::
        %'^'
      =/  r  (teqy s)
      ?@  r  r
      [[%make -.p.r +<.p.r +>-.p.r +>+.p.r ~] q.r]
    ::
        %':'
      =/  r  (teqz s)
      ?@  r  r
      [[%make p.r] q.r]
    ==
  ::
  ::    wide arms on a sat: on 2, again with spots and more lines
  ::
  ++  wlon
    |=  s=sat
    ^-  $@(@ [p=hoon q=sat])
    =/  w  (long:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=hoon q=sat])
    =/  j  (lump k s)
    =/  w  (long:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wrun
    |=  s=sat
    ^-  $@(@ [p=hoon q=sat])
    =/  w  (runt:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=hoon q=sat])
    =/  j  (lump k s)
    =/  w  (runt:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wwid
    |=  s=sat
    ^-  $@(@ [p=hoon q=sat])
    =/  w  (wide:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=hoon q=sat])
    =/  j  (lump k s)
    =/  w  (wide:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wwyo
    |=  s=sat
    ^-  $@(@ [p=spec q=sat])
    =/  w  (wydo:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=spec q=sat])
    =/  j  (lump k s)
    =/  w  (wydo:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wwye
    |=  s=sat
    ^-  $@(@ [p=spec q=sat])
    =/  w  (wyde:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=spec q=sat])
    =/  j  (lump k s)
    =/  w  (wyde:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wwis
    |=  s=sat
    ^-  $@(@ [p=skin q=sat])
    =/  w  (wise:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=skin q=sat])
    =/  j  (lump k s)
    =/  w  (wise:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wtea
    |=  s=sat
    ^-  $@(@ [p=tiki q=sat])
    =/  w  (teak:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=tiki q=sat])
    =/  j  (lump k s)
    =/  w  (teak:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wbon
    |=  s=sat
    ^-  $@(@ [p=chum q=sat])
    =/  w  (bonk:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=chum q=sat])
    =/  j  (lump k s)
    =/  w  (bonk:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  whin
    |=  s=sat
    ^-  $@(@ [p=$@(@ [@ @]) q=sat])
    =/  w  (hinn:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=$@(@ [@ @]) q=sat])
    =/  j  (lump k s)
    =/  w  (hinn:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wsin
    |=  s=sat
    ^-  $@(@ [p=(list $@(@ tuna:hoot)) q=sat])
    =/  w  (sin1:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=(list $@(@ tuna:hoot)) q=sat])
    =/  j  (lump k s)
    =/  w  (sin1:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wshe
    |=  s=sat
    ^-  $@(@ [p=marx:hoot q=sat])
    =/  w  (shed:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=marx:hoot q=sat])
    =/  j  (lump k s)
    =/  w  (shed:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wswr
    |=  s=sat
    ^-  $@(@ [p=marl:hoot q=sat])
    =/  w  (swra:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=marl:hoot q=sat])
    =/  j  (lump k s)
    =/  w  (swra:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wsqu
    |=  s=sat
    ^-  $@(@ [p=marl:hoot q=sat])
    =/  w  (squ1:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=marl:hoot q=sat])
    =/  j  (lump k s)
    =/  w  (squ1:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wsat
    |=  s=sat
    ^-  $@(@ [p=mart:hoot q=sat])
    =/  w  (satt:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=mart:hoot q=sat])
    =/  j  (lump k s)
    =/  w  (satt:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  ++  wsho
    |=  s=sat
    ^-  $@(@ [p=(list beer:hoot) q=sat])
    =/  w  (shop:(self s) tub.s)
    ?^  w  [p.w s(tub q.w)]
    ?.  =(2 w)  w
    =/  k=@ud  0
    |-  ^-  $@(@ [p=(list beer:hoot) q=sat])
    =/  j  (lump k s)
    =/  w  (shop:(slow s all.j end.j) all.j)
    ?^  w  [p.w (seat s all.j q.w)]
    ?.  =(2 w)  w
    ?:  =(%eof end.j)  1
    ?:  out.j  2
    $(k ?:(=(0 k) 1 (mul 2 k)))
  --  ::rd
--
