/-  *toyhoon
/+  *deriv
::
|%
+$  tag
  $:  tol=?
      $=  dat
      $@  $?  %ace   %gap   %per   %dot   %com
              %axis  %lark  %skip
              ::TODO  %sym
              %dttr  %dtwt  %dtls  %dtts
              %wtcl
              %tsgr  %tsls
              %cnts
              %sggr
              %brcn  %brpt
              %ktls
              %wtpt  %wtcn  %wtkt
          ==
      $%  [%atom const=? =aura]
      ==
  ==
::
+$  toke
  $@  ?(%ace %gap %dot %per)
  $%  [%limb =limb]
    ::
      [%atom tol=? const=? =aura a=@]  ::  %atomw + %atomt
    ::
      $:  ?(%dttr %dtwt %dtls %dtts %wtcl %tsgr %tsls %cnts %sggr %brcn %brpt %ktls %wtpt %wtcn %wtkt)
          tol=?
      ==
  ==
::
++  gate
  =+  [vec tagmap]=def
  :: ~>  %slog.[0 (machine:mump (vector-dfa vec))]
  =/  fol=nock
    (cord-nock (vector-dfa vec) (pick-first-mapped tagmap))
  =/  levi  |=(* &)
  !<  $-(cord-cursor $@(~ [=tag cur=cord-cursor]))
  `|=(bus=cord-cursor .*(bus fol))
::
++  def
  =/  rul  de:torp
  ::NOTE  important that %atomw is before %atomt,
  ::      if ambiguous we prefer the former
  %.  ^-  (list $@(term (pair term tag)))
      :~  ace+|+%ace    gap+&+%gap    per+|+%per    dot+|+%dot  com+|+%com
          axis+|+%axis  lark+|+%lark  skip+|+%skip
          ::  %sym
          c-tas+|+[%atom & %tas]
          c-ud-w+|+[%atom & %ud]  c-ud-t+&+[%atom & %ud]
          a-ud-w+|+[%atom | %ud]  a-ud-t+&+[%atom | %ud]
          dttrw+|+%dttr  dttrt+&+%dttr    dtwtw+|+%dtwt  dtwtt+&+%dtwt    dtlsw+|+%dtls  dtlst+&+%dtls    dttsw+|+%dtts  dttst+&+%dtts
          wtclw+|+%wtcl  wtclt+&+%wtcl
          tsgrw+|+%tsgr  tsgrt+&+%tsgr    tslsw+|+%tsls  tslst+&+%tsls
          cntsw+|+%cnts  cntst+&+%cnts
          sggrw+|+%sggr  sggrt+&+%sggr
          brcnw+|+%brcn  brcnt+&+%brcn    brptw+|+%brpt  brptt+&+%brpt
          ktlsw+|+%ktls  ktlst+&+%ktls
          wtptw+|+%wtpt  wtptt+&+%wtpt    wtcnw+|+%wtcn  wtcnt+&+%wtcn    wtktw+|+%wtkt  wtktt+&+%wtkt
      ==
  %~  select-rules  rul
  %-  ~(def-cords rul ~)  :~
    :-  %ace
    '''
    ' '
    '''
  ::
    :-  %comment
    '''
    '::' [^\n]* '\n'
    '''
  ::
    :-  %gap
    '''
    ( comment | [\r\n\t] | '  ' )
    ( comment | [\r\n\t ] )*
    '''
  ::
    :-  %per
    '''
    ')'
    '''
    :-  %dot
    '''
    '.'
    '''
    :-  %com
    '''
    ','
    '''
  ::
    :-  %sym
    '''
    ( '$' | [a-z][a-z0-9-]* )
    '''
  ::
    :-  %c-tas
    '''
    '%' sym
    '''
  ::
    :-  %a-ud-w
    '''
    '0' | ( [1-9] [0-9]{0,2} ( '.' [0-9]{3} )* )
    '''
  ::
    :-  %a-ud-t
    'a-ud-w'  ::TODO  real tall
  ::
    :-  %c-ud-w
    '''
    '%' a-ud-w
    '''
  ::
    :-  %c-ud-t
    '''
    '%' a-ud-t
    '''
  ::
    :-  %axis
    '''
    '+' ( '0' | ( [1-9] [0-9]* ) )
    '''
    :-  %lark
    '''
    [+-] ([<>] [+-])* [<>]?
    '''
    :-  %skip
    '''
    '^'* sym
    '''
  ::
    :-  %dttrw
    '''
    '.*('
    '''
    :-  %dttrt
    '''
    '.*' gap
    '''
  ::
    :-  %dtwtw
    '''
    '.?('
    '''
    :-  %dtwtt
    '''
    '.?' gap
    '''
  ::
    :-  %dtlsw
    '''
    '.+('
    '''
    :-  %dtlst
    '''
    '.+' gap
    '''
  ::
    :-  %dttsw
    '''
    '.=('
    '''
    :-  %dttst
    '''
    '.=' gap
    '''
  ::
    :-  %wtclw
    '''
    '?:('
    '''
    :-  %wtclt
    '''
    '?:' gap
    '''
  ::
    :-  %tsgrw
    '''
    '=>('
    '''
    :-  %tsgrt
    '''
    '=>' gap
    '''
  ::
    :-  %tslsw
    '''
    '=+('
    '''
    :-  %tslst
    '''
    '=+' gap
    '''
  ::
    :-  %cntsw
    '''
    '%=('
    '''
    :-  %cntst
    '''
    '%=' gap
    '''
  ::
    :-  %sggrw
    '''
    '~>('
    '''
    :-  %sggrt
    '''
    '~>' gap
    '''
  ::
    :-  %brcnw
    '''
    '|%('
    '''
    :-  %brcnt
    '''
    '|%' gap
    '''
  ::
    :-  %brptw
    '''
    '|@('
    '''
    :-  %brptt
    '''
    '|@' gap
    '''
  ::
    :-  %ktlsw
    '''
    '^+('
    '''
    :-  %ktlst
    '''
    '^+' gap
    '''
  ::
    :-  %wtptw
    '''
    '?@('
    '''
    :-  %wtptt
    '''
    '?@' gap
    '''
  ::
    :-  %wtcnw
    '''
    '?%('
    '''
    :-  %wtcnt
    '''
    '?%' gap
    '''
  ::
    :-  %wtktw
    '''
    '?^('
    '''
    :-  %wtktt
    '''
    '?^' gap
    '''
  ::
  ==
::
++  parser  :: modeled after te, but different (hard to abstract)
  :: differently structured: build the lexer just-in-time
  :: you really don't want to call this arm repeatedly!
  ::~&  'runt'
  ::~>  %slog.[2 (machine:mump (vector-dfa -:def))]
  =>  |%
      +$  post  [row=@ col=@]
      --
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
    ?~  p  ~  ::  for example, tall token in wide mode
    [t.p p.p cur.r]
  ++  here  `post`?@(buf.st rap.st p.buf.st)
  ++  gape
    |=  [beg=@ end=@ p=post]
    %+  fold-bytes  [txt.cur.st beg end]
    |=  [c=@ p=_p]  ^+  p
    ?:  =(10 c)
      p(row +(row.p), col 1)
    p(col +(col.p))
  ++  proc
    |=  [=tag beg=@ end=@]
    ^-  $@(~ [t=toke p=post])
    ?:  &(tol.tag !tol)  ~
    =/  len  (sub end beg)
    :_  ?.  tol.tag
          rap.st(col (add len col.rap.st))
        (gape beg end rap.st)
    ::
    ^-  toke
    ?+  dat.tag  [dat.tag tol.tag]
      ?(%ace %gap %dot %per)  dat.tag
    ::
        [%atom *]
      :-  %atom
      :^  tol.tag  const.dat.tag  aura.dat.tag
      =>  ?.(const.dat.tag . .(i.cur.st +(i.cur.st), len (dec len)))
      ?+  aura.dat.tag  ~|(strange-aura=aura.dat.tag !!)
          %ud
        %+  big:digits  10
        %-  decimal:digits
        (skip (trip (chunk i.cur.st len)) |=(c=@ =('.' c)))
      ::
          %tas
        (chunk i.cur.st len)
      ==
    ::
      %com    [%limb %| 0 ~]
      %axis   :+  %limb  %&
              -:(fold-bytes [txt.cur.st +(beg) end] (fold:digits 10))
    ::
        %lark
      :+  %limb  %&
      %+  fold-bytes  [txt.cur.st beg end]
      |=  [c=@ acc=_1]
      %+  peg  acc
      ?+  c  !!
        %'-'  %2
        %'+'  %3
        %'<'  %2
        %'>'  %3
      ==
    ::
        %skip
      :+  %limb  %|
      =/  ket=@ud  beg
      |-  ^-  [@ud (unit term)]
      ?:  =('^' (cut 3 [ket 1] txt.cur.st))
        $(ket +(ket))
      =+  s=(sub ket beg)
      =+  n=(cut 3 [ket (sub len s)] txt.cur.st)
      [s ~ ?:(=('$' n) %$ n)]
    ::
      :: %lark  :+  %atom  |
      ::         %+  fold-bytes  [txt.cur.st beg end]
      ::         |=  [c=@ acc=_1]
      ::         %+  peg  acc
      ::         ?+  c  !!
      ::           %'-'  %2
      ::           %'+'  %3
      ::           %'<'  %2
      ::           %'>'  %3
      ::         ==
      :: %cord  :+  %atom  |
      ::         (crip (unescape (trip (chunk +(i.cur.st) (sub len 2)))))
      :: %one   ?+  (chunk beg 1)  !!
      ::           %' '  %ace
      ::           %'('  %pel
      ::           %')'  %per
      ::           %'['  %sel
      ::           %']'  %ser
      ::           %'/'  %fas
    ==     ::  ==
  ::
  ++  expect
    |=  t=toke
    ^-  $@(~ s=_st)
    =+  gulp  ?@  -  ~
    ?:(=(u t) s ~)
  :: ++  expect-atom
  ::   ^-  (mandatory @ _st)
  ::   =+  peek  ?@  -  ~
  ::   ?.  ?=([%atom *] u)  ~
  ::   [a.u move(st s)]
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
  ::
  ++  tall
    ^-  (mandatory naty _st)
    =+  peek  ?@  -  ~  =>  [t=u +(st s)]
    ?+  t  wide(tol |)
      [%atom %& *]  [%noun [%atom aura.t ?.(const.t ~ `a.t)] a.t]^move
      [%dttr %&]    =+  tall-2(st move)       ?@  -  ~  [[%dttr u] s]
      [%dtwt %&]    =+  tall(st move)         ?@  -  ~  [[%dtwt u] s]
      [%dtls %&]    =+  tall(st move)         ?@  -  ~  [[%dtls u] s]
      [%dtts %&]    =+  tall-2(st move)       ?@  -  ~  [[%dtts u] s]
      [%wtcl %&]    =+  tall(st move)         ?@  -  ~  =>  [t=u +(st s)]
                    =+  (expect %gap)         ?@  -  ~  =>  +(st s)
                    =+  tall-2                ?@  -  ~  [[%wtcl t u] s]
      [%tsgr %&]    =+  tall-2(st move)       ?@  -  ~  [[%tsgr u] s]
      [%tsls %&]    =+  tall-2(st move)       ?@  -  ~  [[%tsls u] s]
      [%cnts %&]    !!  ::TODO
      [%sggr %&]    !!  ::TODO
      [%brcn %&]    !!  ::TODO
      [%brpt %&]    !!  ::TODO
      [%ktls %&]    =+  tall-2(st move)       ?@  -  ~  [[%ktls u] s]
      [%wtpt %&]    =+  tall-wing-2(st move)  ?@  -  ~  [[%wtpt u] s]
      [%wtcn %&]    =+  wing-full(st move)    ?@  -  ~  =>  [w=u +(st s)]
                    =+  (expect %gap)         ?@  -  ~  =>  +(st s)
                    =+  tall-atom             ?@  -  ~  =>  [a=u +(st s)]
                    =+  (expect %gap)         ?@  -  ~  =>  +(st s)
                    =+  tall-2                ?@  -  ~  [[%wtcn w a u] s]
      [%wtkt %&]    =+  tall-wing-2(st move)  ?@  -  ~  [[%wtkt u] s]
    ==
  ::
  ++  wide
    ^-  (mandatory naty _st)
    =+  gulp  ?@  -  ~  =>  [t=u +(st s)]
    ?+  t  ~
      [%limb *]     =+  (wing-tail limb.t)  ?@  -  ~  [[%cnts u ~] s]
      ::TODO  handle %dot
      [%atom %| *]  [%noun [%atom aura.t ?.(const.t ~ `a.t)] a.t]^st
      [%dttr %|]    =+  wide-2         ?@  -  ~  [[%dttr u] s]
      [%dtwt %|]    =+  wide-1         ?@  -  ~  [[%dtwt u] s]
      [%dtls %|]    =+  wide-1         ?@  -  ~  [[%dtls u] s]
      [%dtts %|]    =+  wide-2         ?@  -  ~  [[%dtts u] s]
      [%wtcl %|]    =+  wide           ?@  -  ~  =>  [t=u +(st s)]
                    =+  (expect %ace)  ?@  -  ~  =>  +(st s)
                    =+  wide-2         ?@  -  ~  [[%wtcl t u] s]
      [%tsgr %|]    =+  wide-2         ?@  -  ~  [[%tsgr u] s]
      [%tsls %|]    =+  wide-2         ?@  -  ~  [[%tsls u] s]
      [%cnts %|]    !!  ::TODO
      [%sggr %|]    !!  ::TODO
      [%brcn %|]    !!  ::TODO
      [%brpt %|]    !!  ::TODO
      [%ktls %|]    =+  wide-2         ?@  -  ~  [[%ktls u] s]
      [%wtpt %|]    =+  wide-wing-2    ?@  -  ~  [[%wtpt u] s]
      [%wtcn %|]    =+  wing-full      ?@  -  ~  =>  [w=u +(st s)]
                    =+  (expect %ace)  ?@  -  ~  =>  +(st s)
                    =+  wide-atom      ?@  -  ~  =>  [a=u +(st s)]
                    =+  (expect %ace)  ?@  -  ~  =>  +(st s)
                    =+  wide-2         ?@  -  ~  [[%wtcn w a u] s]
      [%wtkt %|]    =+  wide-wing-2    ?@  -  ~  [[%wtkt u] s]
    ==
  ::
  ++  tall-atom
    ^-  (mandatory @ _st)
    =+  peek  ?@  -  ~  =>  [t=u +(st s)]
    ?.  ?=([%atom %& *] t)  wide-atom
    [a.t move]
  ::
  ++  wide-atom
    ^-  (mandatory @ _st)
    =+  gulp  ?@  -  ~  =>  [t=u +(st s)]
    ?.  ?=([%atom %| *] t)  ~
    [a.t st]
  ::
  ++  wing-full
    ^-  (mandatory wing _st)
    =+  gulp  ?@  -  ~  =>  [t=u +(st s)]
    ?.  ?=([%limb *] t)  ~
    (wing-tail limb.t)
  ::
  ++  wing-tail
    |=  l=limb
    ^-  (mandatory wing _st)
    ::TODO  handle %dot as wing element
    =/  rev=(list limb)  [l]~
    |^
    =+  peek      ?@  -  done          =>  [t=u bak=s +(st s)]
    ?.  ?=(%dot t)       done          =.  st  move
    =+  peek      ?@  -  ~             =>  [l=u +(st s)]
    ?.  ?=([%limb *] l)  done(st bak)  $(rev [limb.l rev], st move)
    ++  done  [(flop rev) st]
    --
  :: ++  expr2p
  ::   |=  [sep=toke exp=$-(_st (mandatory expr _st))]
  ::   ^-  (mandatory [p=expr q=expr mor=(list expr)] _st)
  ::   =+  ((two-plus expr) +<)  ?@  -  ~  =>  [u +(st s)]
  ::   [[p q rev] st]
  :: ++  expr2p-w  (expr2p %ace |=(s=_st wide(st s)))
  ++  wide-close
    ^-  $@(~ s=_st)
    =+  (expect %per)
    ?@(- ~ s)
  :: ++  tall-close
  ::   ^-  $@(~ s=_st)
  ::   =+  (expect %gap)   ?@  -  ~  =>  +(st s)
  ::   =+  (expect %stet)  ?@  -  ~  s
  ++  wide-1
    ^-  (mandatory naty _st)
    =+  wide        ?@  -  ~  =>  [one=u +(st s)]
    =+  wide-close  ?@  -  ~  [one s]
  ++  wide-2
    ^-  (mandatory [naty naty] _st)
    =+  wide           ?@  -  ~  =>  [one=u +(st s)]
    =+  (expect %ace)  ?@  -  ~  =>  +(st s)
    =+  wide-1         ?@  -  ~  [[one u] s]
  ++  wide-wing-2
    ^-  (mandatory [wing naty naty] _st)
    =+  wing-full      ?@  -  ~  =>  [w=u +(st s)]
    =+  (expect %ace)  ?@  -  ~  =>  +(st s)
    =+  wide-2         ?@  -  ~  [[w u] s]
  ++  tall-2
    ^-  (mandatory [naty naty] _st)
    =+  tall           ?@  -  ~  =>  [one=u +(st s)]
    =+  (expect %gap)  ?@  -  ~  =>  +(st s)
    =+  tall           ?@  -  ~  [[one u] s]
  ++  tall-wing-2
    ^-  (mandatory [wing naty naty] _st)
    =+  wing-full      ?@  -  ~  =>  [w=u +(st s)]
    =+  (expect %gap)  ?@  -  ~  =>  +(st s)
    =+  tall-2         ?@  -  ~  [[w u] s]
  :: ++  dtls-w
  ::   ^-  (mandatory expr _st)
  ::   =+  wide-1  ?@  -  ~  [[%dtls u] s]
  :: ++  dtts-w
  ::   ^-  (mandatory expr _st)
  ::   =+  wide-2  ?@  -  ~  [[%dtts u] s]
  :: ++  wide  ::  pel is part of the opening token
  ::   ^-  (mandatory expr _st)
  ::   =+  gulp  ?@  -  ~  =>  [t=u +(st s)]
  ::   ?+  t  ~
  ::     %sel          =+  expr2p-w       ?@  -  ~  =>  [es=u +(st s)]
  ::                   =+  (expect %ser)  ?@  -  ~  [[%cltr es] s]
  ::                   ::  awkwardly post-process es to make a big %dtsq?
  ::     %fas          =+  expect-atom    ?@  -  ~  [[%dtfs u] s]
  ::     %ilus         dtls-w
  ::     %itis         dtts-w
  ::     [%cltr %|]    =+  expr2p-w       ?@  -  ~  =>  [es=u +(st s)]
  ::                   =+  wide-close     ?@  -  ~  [[%cltr es] s]
  ::     [%atom %| *]  [%dtsq a.t]^st
  ::     [%dtfs %|]    =+  expect-atom    ?@  -  ~  =>  [a=u +(st s)]
  ::                   =+  wide-close     ?@  -  ~  [[%dtfs a] s]
  ::     [%dtsq %|]    =+  lit-w          ?@  -  ~  =>  [v=u +(st s)]
  ::                   =+  wide-close     ?@  -  ~  [[%dtsq v] s]
  ::     [%dttr %|]    =+  wide-2         ?@  -  ~  [[%dttr u] s]
  ::     [%dtwt %|]    dtls-w
  ::     [%dtls %|]    =+  wide-1         ?@  -  ~  [[%dtls u] s]
  ::     [%dtts %|]    dtts-w
  ::     [%wtcl %|]    =+  wide           ?@  -  ~  =>  [t=u +(st s)]
  ::                   =+  (expect %ace)  ?@  -  ~  =>  +(st s)
  ::                   =+  wide-2         ?@  -  ~  [[%wtcl t u] s]
  ::     [%tsgr %|]    =+  wide-2         ?@  -  ~  [[%tsgr u] s]
  ::     [%tsls %|]    =+  wide-2         ?@  -  ~  [[%tsls u] s]
  ::     [%dtcn %|]    =+  expect-atom    ?@  -  ~  =>  [a=u +(st s)]
  ::                   =+  (expect %ace)  ?@  -  ~  =>  +(st s)
  ::                   =+  wide-1         ?@  -  ~  [[%dtcn a u] s]
  ::     [%dtbr %|]    =+  wide-1         ?@  -  ~  [[%dtbr u] s]
  ::     [%dthx %|]    =+  expect-atom    ?@  -  ~  =>  [a=u +(st s)]
  ::                   =+  (expect %ace)  ?@  -  ~  =>  +(st s)
  ::                   =+  wide-2         ?@  -  ~  [[%dthx a u] s]
  ::     [%sgpt %|]    =+  expect-atom    ?@  -  ~  =>  [a=u +(st s)]
  ::                   =+  (expect %ace)  ?@  -  ~  =>  +(st s)
  ::                   =+  wide-1         ?@  -  ~  [[%sgpt a u] s]
  ::     [%sgkt %|]    =+  expect-atom    ?@  -  ~  =>  [a=u +(st s)]
  ::                   =+  (expect %ace)  ?@  -  ~  =>  +(st s)
  ::                   =+  wide-2         ?@  -  ~  [[%sgkt a u] s]
  ::   ==
  :: ++  tall  ::  gap is part of the opening token
  ::   ^-  (mandatory expr _st)
  ::   =+  peek  ?@  -  ~  =>  [t=u +(st s)]
  ::   ?+  t  wide(tol |)
  ::     [%atom *]   [%dtsq a.t]^move
  ::     [%cltr %&]  =+  (expr2p(st move) %gap |=(s=_st tall(st s)))
  ::                 ?@  -  ~  =>  [in=u +(st s)]
  ::                 =+  tall-close  ?@  -  ~  [[%cltr in] s]
  ::     [%dtfs %&]  =+  expect-atom(st move)  ?@  -  ~  [[%dtfs u] s]
  ::     [%dtsq %&]  =+  lit-t(st move)        ?@  -  ~  [[%dtsq u] s]
  ::     [%dttr %&]  =+  tall-2(st move)       ?@  -  ~  [[%dttr u] s]
  ::     [%dtwt %&]  =+  tall(st move)         ?@  -  ~  [[%dtwt u] s]
  ::     [%dtls %&]  =+  tall(st move)         ?@  -  ~  [[%dtls u] s]
  ::     [%dtts %&]  =+  tall-2(st move)       ?@  -  ~  [[%dtts u] s]
  ::     [%wtcl %&]  =+  tall(st move)         ?@  -  ~  =>  [t=u +(st s)]
  ::                 =+  (expect %gap)         ?@  -  ~  =>  +(st s)
  ::                 =+  tall-2                ?@  -  ~  [[%wtcl t u] s]
  ::     [%tsgr %&]  =+  tall-2(st move)       ?@  -  ~  [[%tsgr u] s]
  ::     [%tsls %&]  =+  tall-2(st move)       ?@  -  ~  [[%tsls u] s]
  ::     [%dtcn %&]  =+  expect-atom(st move)  ?@  -  ~  =>  [a=u +(st s)]
  ::                 =+  (expect %gap)         ?@  -  ~  =>  +(st s)
  ::                 =+  tall                  ?@  -  ~  [[%dtcn a u] s]
  ::     [%dtbr %&]  =+  tall(st move)         ?@  -  ~  [[%dtbr u] s]
  ::     [%dthx %&]  =+  expect-atom(st move)  ?@  -  ~  =>  [a=u +(st s)]
  ::                 =+  (expect %gap)         ?@  -  ~  =>  +(st s)
  ::                 =+  tall-2                ?@  -  ~  [[%dthx a u] s]
  ::     [%sgpt %&]  =+  expect-atom(st move)  ?@  -  ~  =>  [a=u +(st s)]
  ::                 =+  (expect %gap)         ?@  -  ~  =>  +(st s)
  ::                 =+  tall                  ?@  -  ~  [[%sgpt a u] s]
  ::     [%sgkt %&]  =+  expect-atom(st move)  ?@  -  ~  =>  [a=u +(st s)]
  ::                 =+  (expect %gap)         ?@  -  ~  =>  +(st s)
  ::                 =+  tall-2                ?@  -  ~  [[%sgkt a u] s]
  ::   ==
  --
--
