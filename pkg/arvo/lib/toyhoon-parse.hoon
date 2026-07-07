/-  *toyhoon
/+  *deriv
::
|%
+$  tag
  $:  tol=?
      $=  dat
      $@  $?  %ace   %gap   %per   %dot   %com   %sel   %ser   %tar
              %coma  %stis  %slus  %shep
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
              %cltr
          ==
      $%  [%atom const=? =aura]
      ==
  ==
::
+$  toke
  $@  ?(%ace %gap %dot %per %sel %ser %tar %coma %stis %slus %shep)
  $%  [%limb =limb]
    ::
      [%atom tol=? const=? =aura a=@]  ::  %atomw + %atomt
    ::
      $:  ?(%dttr %dtwt %dtls %dtts %wtcl %tsgr %tsls %cnts %sggr %brcn %brpt %ktls %wtpt %wtcn %wtkt %cltr)
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
      :~  ace+|+%ace    gap+&+%gap    per+|+%per    dot+|+%dot  com+|+%com  sel+|+%sel  ser+|+%ser   tar+|+%tar
          coma+|+%coma  stis+&+%stis  slus+&+%slus  shep+|+%shep
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
          cltrw+|+%cltr  cltrt+&+%cltr
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
    :-  %sel
    '''
    '['
    '''
    :-  %ser
    '''
    ']'
    '''
    :-  %tar
    '''
    '*'
    '''
  ::
    :-  %coma
    '''
    com ace
    '''
  ::
    :-  %stis
    '''
    gap '=='
    '''
    :-  %slus
    '''
    '++' gap
    '''
  ::
    :-  %shep
    '''
    '--'
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
    :-  %cltrw
    '''
    ':*('
    '''
    :-  %cltrt
    '''
    ':*' gap
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
      ?(%ace %gap %dot %per %sel %ser %tar %coma %stis %slus %shep)  dat.tag
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
  +$  tope  $-(toke ?)
  ::
  ++  more  ::  repeatedly separator+element
    |*  m=mold
    |=  $:  sep=tope
            lem=$-(_st (mandatory m _st))
        ==
    =|  l=(list m)
    |-  ^-  (mandatory (list m) _st)
    =+  peek         ?@  -  ~  =>  [t=u +(st s)]
    ?.  (sep t)                    [(flop l) st]
    =+  (lem move)   ?@  -  ~  =>  [e=u +(st s)]
    $(l [e l])
  ::
  ++  most  ::  element and then +more
    |*  m=mold
    |=  $:  sep=tope
            lem=$-(_st (mandatory m _st))
        ==
    ^-  (mandatory (lest m) _st)
    =+  (lem st)            ?@  -  ~  =>  [i=u +(st s)]
    =+  ((more m) sep lem)  ?@  -  ~  =>  [l=u +(st s)]
    [[i l] st]
  ::
  ++  tuplish
    |*  m=mold
    |=  $:  [beg=tope sep=tope end=tope]
            lem=$-(_st (mandatory m _st))
        ==
    ^-  (mandatory [i=m t=(lest m)] _st)
    =+  peek                         ?@  -  ~  =>  [t=u +(st s)]
    ?.  (beg t)                             ~
    =+  (lem move)                   ?@  -  ~  =>  [i=u +(st s)]
    =+  peek                         ?@  -  ~  =>  [t=u +(st s)]
    ?.  (sep t)                             ~
    =+  ((most(st move) m) sep lem)  ?@  -  ~  =>  [l=u +(st s)]
    ::REVIEW  need another peek?
    ?.  (end t)                             ~      [[i l] move]
  ::
  ++  tall
    ^-  (mandatory naty _st)
    =+  peek  ?@  -  ~  =>  [t=u +(st s)]
    ?+  t  wide(tol |)
      [%atom %& *]  [%noun [%atom aura.t ?.(const.t ~ `a.t)] a.t]^move
      [%cltr %&]    =+  %+  (autocons(st move) naty)
                          |=(t=toke ?=(%gap t))
                        |=  s=_st
                        tall(st s)
                                              ?@  -  ~  =>  [n=u +(st s)]
                    =+  (expect %stis)        ?@  -  ~  [n s]
      [%dttr %&]    =+  tall-2(st move)       ?@  -  ~  [[%dttr u] s]
      [%dtwt %&]    =+  tall(st move)         ?@  -  ~  [[%dtwt u] s]
      [%dtls %&]    =+  tall(st move)         ?@  -  ~  [[%dtls u] s]
      [%dtts %&]    =+  tall-2(st move)       ?@  -  ~  [[%dtts u] s]
      [%wtcl %&]    =+  tall(st move)         ?@  -  ~  =>  [t=u +(st s)]
                    =+  (expect %gap)         ?@  -  ~  =>  +(st s)
                    =+  tall-2                ?@  -  ~  [[%wtcl t u] s]
      [%tsgr %&]    =+  tall-2(st move)       ?@  -  ~  [[%tsgr u] s]
      [%tsls %&]    =+  tall-2(st move)       ?@  -  ~  [[%tsls u] s]
      [%cnts %&]    =+  wing-full(st move)    ?@  -  ~  =>  [w=u +(st s)]
                    =+  (expect %gap)         ?@  -  ~  =>  +(st s)
                    =+  %+  (most (pair wing naty))
                          |=(t=toke ?=(%gap t))
                        |=  s=_st
                        =+  wing-full(st s)       ?@  -  ~  =>  [v=u +(st s)]
                        =+  (expect %gap)         ?@  -  ~  =>  +(st s)
                        =+  tall                  ?@  -  ~  [[v u] s]
                                              ?@  -  ~  =>  [l=u +(st s)]
                    =+  (expect %stis)        ?@  -  ~      [[%cnts w l] s]
      [%sggr %&]    =+  peek(st move)         ?@  -  ~  =>  [t=u +(st s)]
                    ?.  ?=([%atom @ %& %tas @] t)  ~
                    =+  peek(st move)  ?@  -  ~  =>  [tt=u +(st s)]
                    ?.  ?=(%dot tt)
                      =+  (expect %gap)  ?@  -  ~  =>  +(st s)
                      =+  tall           ?@  -  ~  [[%sggr a.t u] s]
                    =+  tall(st move)  ?@  -  ~  =>  [clu=u +(st s)]
                    =+  (expect %gap)  ?@  -  ~  =>  +(st s)
                    =+  tall           ?@  -  ~  [[%sggr [a.t clu] u] s]
      [%brcn %&]    !!  ::TODO
      [%brpt %&]    =+  any-layout(st move)   ?@  -  ~  =>  [l=n +(st s)]
                    =+  (expect %slus)        ?@  -  ~  =>  +(st s)
                    =+  %+  (most (pair term naty))
                          |=(t=toke ?=(%slus t))
                        |=  s=_st
                        =+  peek(st s)          ?@  -  ~  =>  [t=u +(st s)]
                        ?.  ?=([%limb %| %0 ~ @] t)    ~  =>  [a=u.q.limb.t +(st move)]
                        =+  (expect %gap)       ?@  -  ~  =>  +(st s)
                        =+  tall                ?@  -  ~  =>  [b=u +(st s)]
                        =+  (expect %gap)       ?@  -  ~  [[a b] s]
                                              ?@  -  ~  =>  [b=u +(st s)]
                    =+  (expect %shep)        ?@  -  ~  =>  +(st s)
                    ?~  cor=(validate-layout l b)    ~  [[%brpt u.cor] st]
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
  ++  validate-layout
    |=  [lay=(unit layout) bat=(list (pair term naty))]
    =|  bam=(map term naty)
    |-  ^-  (unit [lay=(unit layout) bat=(map term naty)])
    ?^  bat
      ?:  (~(has by bam) p.i.bat)
        ~&([%duplicate-arm p.i.bat] ~)
      $(bam (~(put by bam) i.bat), bat t.bat)
    ?~  lay  `[~ bam]
    =+  sat=[dot=| nos=~(key by bam) err=|]
    =-  ?:  err  ~
        ?:  &(!dot ?=(^ nos))
          ~&([%layout-incomplete nos] ~)
        `[lay bam]
    |-  ^+  sat
    ?@  u.lay
      ?:  (~(has in nos.sat) u.lay)  sat(nos (~(del in nos.sat) u.lay))
      ?:  (~(has by bam) u.lay)
        ~&([%duplicate-layout-arm u.lay] sat(err &))
      ~&([%unimplemented-layout-arm u.lay] sat(err &))
    ?:  ?=([~ ~] u.lay)
      ?:  dot.sat
        ~&(%duplicate-layout-wildcard sat(err &))
      sat(dot &)
    =.  sat  $(u.lay -.u.lay)
    $(u.lay +.u.lay)
  ::
  ++  wide
    ^-  (mandatory naty _st)
    =+  gulp  ?@  -  ~  =>  [t=u +(st s)]
    ?+  t  ~
      [%limb *]     =+  (wing-tail limb.t)  ?@  -  ~  [[%cnts u ~] s]
      ::TODO  handle %dot
      [%atom %| *]  [%noun [%atom aura.t ?.(const.t ~ `a.t)] a.t]^st
      %sel          =+  wide-cell-bod  ?@  -  ~  =>  [n=u +(st s)]
                    =+  (expect %ser)  ?@  -  ~  [n s]
      [%cltr %|]    =+  wide-cell-bod  ?@  -  ~  =>  [n=u +(st s)]
                    =+  (expect %per)  ?@  -  ~  [n s]
      [%dttr %|]    =+  wide-2         ?@  -  ~  [[%dttr u] s]
      [%dtwt %|]    =+  wide-1         ?@  -  ~  [[%dtwt u] s]
      [%dtls %|]    =+  wide-1         ?@  -  ~  [[%dtls u] s]
      [%dtts %|]    =+  wide-2         ?@  -  ~  [[%dtts u] s]
      [%wtcl %|]    =+  wide           ?@  -  ~  =>  [t=u +(st s)]
                    =+  (expect %ace)  ?@  -  ~  =>  +(st s)
                    =+  wide-2         ?@  -  ~  [[%wtcl t u] s]
      [%tsgr %|]    =+  wide-2         ?@  -  ~  [[%tsgr u] s]
      [%tsls %|]    =+  wide-2         ?@  -  ~  [[%tsls u] s]
      [%cnts %|]    =+  wing-full        ?@  -  ~  =>  [w=u +(st s)]
                    =+  (expect %ace)    ?@  -  ~  =>  +(st s)
                    =+  %+  (most (pair wing naty))
                          |=(t=toke ?=(%coma t))
                        |=  s=_st
                        =+  wing-full(st s)  ?@  -  ~  =>  [v=u +(st s)]
                        =+  (expect %ace)    ?@  -  ~  =>  +(st s)
                        =+  wide             ?@  -  ~      [[v u] s]
                                         ?@  -  ~  =>  [l=u +(st s)]
                    =+  (expect %per)    ?@  -  ~      [[%cnts w l] st]
      [%sggr %|]    =+  peek           ?@  -  ~  =>  [t=u +(st s)]
                    ?.  ?=([%atom %| %& %tas @] t)  ~
                    =+  peek(st move)  ?@  -  ~  =>  [tt=u +(st s)]
                    ?.  ?=(%dot tt)
                      =+  (expect %ace)  ?@  -  ~  =>  +(st s)
                      =+  wide-1         ?@  -  ~  [[%sggr a.t u] s]
                    =+  wide(st move)  ?@  -  ~  =>  [clu=u +(st s)]
                    =+  (expect %ace)  ?@  -  ~  =>  +(st s)
                    =+  wide-1         ?@  -  ~  [[%sggr [a.t clu] u] s]
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
  ::
  ++  wide-1
    ^-  (mandatory naty _st)
    =+  wide           ?@  -  ~  =>  [one=u +(st s)]
    =+  (expect %per)  ?@  -  ~  [one s]
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
  ++  wide-cell-bod
    ^-  (mandatory naty _st)
    %+  (autocons naty)
      |=(t=toke ?=(%ace t))
    |=(s=_st wide(st s))
  ++  autocons
    |*  m=mold
    |=  $:  sep=tope
            lem=$-(_st (mandatory m _st))
        ==
    ^-  (mandatory m _st)
    =+  ((most m) sep lem)
    ?@  -  ~
    :_  s
    |-  ^-  m
    ?~(t.u i.u [i.u $(u t.u)])  ::TODO  paul homework
  ++  wide-layout  ::  WideLayout
    ^-  (mandatory layout _st)
    =+  peek           ?@  -  ~  =>  [t=u +(st s)]
    ?+  t  ~
      %sel          =+  wide-layout-element(st move)  ?@  -  ~  =>  [h=u +(st s)]
                    =+  (expect %ace)                 ?@  -  ~  =>  +(st s)
                    =+  %+  (autocons layout)
                          |=(t=toke ?=(%ace t))
                        |=  s=_st
                        wide-layout-element(st s)
                                                      ?@  -  ~  =>  [l=[h u] +(st s)]
                    =+  (expect %ser)                 ?@  -  ~  [l s]
      [%cltr %|]    =+  wide-layout-element(st move)  ?@  -  ~  =>  [h=u +(st s)]
                    =+  (expect %ace)                 ?@  -  ~  =>  +(st s)
                    =+  %+  (autocons layout)
                          |=(t=toke ?=(%ace t))
                        |=  s=_st
                        wide-layout-element(st s)
                                                      ?@  -  ~  =>  [l=[h u] +(st s)]
                    =+  (expect %per)                 ?@  -  ~  [l s]
    ==
  ++  wide-layout-element  ::  LayoutElementW
    ^-  (mandatory layout _st)
    =+  peek           ?@  -  ~  =>  [t=u +(st s)]
    ?+  t  ~
      [%limb %| %0 ~ @]   [u.q.limb.t move]
      %tar                [[~ ~] move]
      ?(%sel [%cltr %|])  wide-layout
    ==
  ::
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
  ++  tall-layout  ::  TallLayout
    ^-  (mandatory layout _st)
    =+  peek           ?@  -  ~  =>  [t=u +(st s)]
    ?+  t  ~
      [%cltr %&]    =+  tall-layout-element(st move)  ?@  -  ~  =>  [h=u +(st s)]
                    =+  (expect %gap)                 ?@  -  ~  =>  +(st s)
                    =+  %+  (autocons layout)
                          |=(t=toke ?=(%gap t))
                        |=  s=_st
                        tall-layout-element(st s)
                                                      ?@  -  ~  =>  [l=[h u] +(st s)]
                    =+  (expect %stis)                ?@  -  ~  [l s]
    ==
  ++  tall-layout-element  ::  LayoutElementT
    ^-  (mandatory layout _st)
    =+  peek           ?@  -  ~  =>  [t=u +(st s)]
    ?+  t  ~
      [%limb %| %0 ~ @]   [u.q.limb.t move]
      %tar                [[~ ~] move]
      ?(%sel [%cltr %|])  wide-layout
      [%cltr %&]          tall-layout
    ==
  ::
  ++  any-layout
    ^-  (nullable-a layout _st)
    =+  peek             ?@  -  ~  =>  [t=u +(st s)]
    ::NOTE  intentionally not moving post-peek
    ?+  t  [~ st]
        [%cltr %&]
      =+  tall-layout    ?@  -  ~  =>  [l=u +(st s)]
      =+  (expect %gap)  ?@  -  ~  [`l s]
    ::
        ?(%sel [%cltr %|])
      =+  wide-layout    ?@  -  ~  =>  [l=u +(st s)]
      =+  (expect %gap)  ?@  -  ~  [`l s]
    ==
  ::
  ++  nullable-c  :: nullable grammar arm with certainly cellular product
    |$([c st] $@(~ [n=$@(~ u=c) s=st]))
  ++  nullable-a  :: with a possibly atomic product
    |$([a st] $@(~ [n=(unit a) s=st]))
  --
--
