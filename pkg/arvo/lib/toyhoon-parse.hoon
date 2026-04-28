/-  *toyhoon
/+  *deriv
::
|%
++  gate
  =+  [vec tag]=def
  :: ~>  %slog.[0 (machine:mump (vector-dfa vec))]
  =/  fol=nock
    (cord-nock (vector-dfa vec) (pick-first-mapped tag))
  =/  levi  |=(* &)
  !<  (tokenize-gate cord-cursor)
  `|=(bus=cord-cursor .*(bus fol))
::
++  def
  =/  rul  de:torp
  ::TODO  dtwt(,dtwt_,dtls(,dtls_,dtts(,dtts_,wtcl(,wtcl_,tsgr(,tsgr_,tsls(,tsls_,dot,ε,axis,skip,lark,com,cnts(,cnts_,gap_stat,cen_sym,sggr(,sggr_,ktls(,ktls_,wtpt(,wtpt_,wtcn(,wtcn_,wtkt(,wtkt_,cltr(,[,],cltr_,sym,tar,gap_stet,slus,gap_stop,bar,wut,brpt_,brcn_,$
  ::NOTE  important that %atomw is before %atomt,
  ::      if ambiguous we prefer the former
  %.  ~[%ace %gap %per %atomw %atomt %dtlsw %dtlst %dttrw %dttrt]
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
  ::
    :-  %atomw  ::TODO  moar auras
    '''
    '0' | ( [1-9] [0-9]{0,2} ( '.' [0-9]{3} )* )
    '''
  ::
    :-  %atomt
    'atomw'
  ::
    :-  %dttrw
    '''
    '.*('
    '''
  ::
    :-  %dttrt
    '''
    '.*' gap
    '''
  ::
    :-  %dtlsw
    '''
    '.+('
    '''
  ::
    :-  %dtlst
    '''
    '.+' gap
    '''
  ==
::
++  parser  :: modeled after te, but different (hard to abstract)
  :: differently structured: build the lexer just-in-time
  :: you really don't want to call this arm repeatedly!
  ::~&  'runt'
  ::~>  %slog.[2 (machine:mump (vector-dfa -:def))]
  =>  |%
      +$  toke
        $@  %per
        $%  [%atom tol=? a=@]  ::  %atomw + %atomt
            [%dtls tol=?]
        ==
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
    ?@  p  ~  ::  for example, tall token in wide form
    [t.p p.p cur.r]
  ++  here  `post`?@(buf.st rap.st p.buf.st)
  :: ++  gape
  ::   |=  [beg=@ end=@ p=post]
  ::   %+  fold-bytes  [txt.cur.st beg end]
  ::   |=  [c=@ p=_p]  ^+  p
  ::   ?:  =(10 c)
  ::     p(row +(row.p), col 1)
  ::   p(col +(col.p))
  ++  proc
    |=  [tag=* beg=@ end=@]
    ^-  $@(~ [t=toke p=post])
    :: ?:  =(%gap tag)
    ::   ?.  tol  ~
    ::   gap+(gape beg end rap.st)
    :: ?:  =(%stet tag)
    ::   ?.  tol  ~
    ::   stet+rap.st(col +(+(col.rap.st)))
    :: ?:  =(%ipfx tag)
    ::   :_  rap.st(col +(+(col.rap.st)))
    ::   ?+  (chunk beg 1)  !!
    ::     %'+'  %ilus
    ::     %'='  %itis
    ::   ==
    ::  if there were any tall form atoms, we would have to
    ::  have to deal with their internal gaps here
    =/  len  (sub end beg)
    :_  rap.st(col (add len col.rap.st))
    ^-  toke
    ?+  tag  !!
      %per  tag
    ::
        ?(%atomw %atomt)
      :+  %atom  ?=(%atomt tag)
      %+  big:digits  10
      %-  decimal:digits
      (skip (trip (chunk i.cur.st len)) |=(c=@ =('.' c)))
    ::
      %dtlsw  [%dtls |]
      %dtlst  [%dtls &]
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
      [%atom %& *]  [%noun [%atom %ud ~] a.t]^move
      [%dtls %&]    =+  tall(st move)  ?@  -  ~  [[%dtls u] s]
    ==
  ::
  ++  wide
    ^-  (mandatory naty _st)
    =+  gulp  ?@  -  ~  =>  [t=u +(st s)]
    ?+  t  ~
      [%atom %| *]  [%noun [%atom %ud ~] a.t]^st
      [%dtls %|]    =+  wide-1   ?@  -  ~  [[%dtls u] s]
    ==
  ::
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
  :: ++  wide-2
  ::   ^-  (mandatory [expr expr] _st)
  ::   =+  wide           ?@  -  ~  =>  [one=u +(st s)]
  ::   =+  (expect %ace)  ?@  -  ~  =>  +(st s)
  ::   =+  wide-1         ?@  -  ~  [[one u] s]
  :: ++  tall-2
  ::   ^-  (mandatory [expr expr] _st)
  ::   =+  tall           ?@  -  ~  =>  [one=u +(st s)]
  ::   =+  (expect %gap)  ?@  -  ~  =>  +(st s)
  ::   =+  tall           ?@  -  ~  [[one u] s]
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
