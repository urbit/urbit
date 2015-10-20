section 2eM, regular-expressions
================================

### `++pars`

    ++  pars
      |=  [a=tape]                                          ::  parse tape to rege
      ^-  (unit rege)
      =+  foo=((full apex:rags) [[1 1] a])
      ?~  q.foo
        ~
      [~ p.u.q.foo]
    ::

Parse regular expression

    ~zod/try=> (pars "samo")
    [ ~
      [ %pair
        p=[%lite p=~~s]
        q=[%pair p=[%lite p=~~a] q=[%pair p=[%lite p=~~m] q=[%lite p=~~o]]]
      ]
    ]
    ~zod/try=> (pars "so[,.0-9]")
    [ ~
      [ %pair
        p=[%lite p=~~s]
        q=[%pair p=[%lite p=~~o] q=[%brac p=288.036.862.105.223.168]]
      ]
    ]
    ~zod/try=> `@ub`288.036.862.105.223.168
    0b11.1111.1111.0101.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
    ~zod/try=> `@ub`(lsh 0 `@`'9' 1)
    0b10.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
    ~zod/try=> `@ub`(roll (turn ",.0123456789" |=(a=@ (lsh 0 a 1))) con)
    0b11.1111.1111.0101.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000
    ~zod/try=> (pars "sop.*")
    [ ~
      [ %pair
        p=[%lite p=~~s] 
        q=[%pair p=[%lite p=~~o] q=[%pair p=[%lite p=~~p] q=[%mant p=%dote]]]
      ]
    ]
    ~zod/try=> (pars "(hel)?")
    [ ~
      [ %eith
          p
        [ %capt
          p=[%pair p=[%lite p=~~h] q=[%pair p=[%lite p=~~e] q=[%lite p=~~l]]]
          q=0
        ]
        q=%empt
      ]
    ]
    ~zod/try=> (pars "(hel)??")
    [ ~ 
      [ %eith
        p=%empt
          q
        [ %capt
          p=[%pair p=[%lite p=~~h] q=[%pair p=[%lite p=~~e] q=[%lite p=~~l]]]
          q=0
        ]
      ]
    ]
    ~zod/try=> (pars "a\{1,20}")
    [~ [%betw p=[%lite p=~~a] q=1 r=20]]

### `++rags`

    ++  rags                                                ::  rege parsers
      =>  |%

Regex parser arms

### `++nor`

          ++  nor  ;~(less (mask "^$()|*?+.[\\") (shim 1 127)) :: non-control char

XX document

### `++les`

          ++  les  ;~(less bas asp)                         ::  not backslash

XX document

### `++lep`

          ++  lep  ;~(less (mask "^[]\\") asp)              ::  charset non-control

XX document

### `++asp`

          ++  asp  (shim 32 126)                            ::  printable ascii

XX document

### `++alb`

          ++  alb  ;~(less ser asp)                         ::  charset literal char

XX document

### `++mis`

          ++  mis  ;~(less aln asp)                         ::  non alphanumeric
          --
      |%

XX document

### `++apex`

      ++  apex                                              ::  top level
        %+  knee  *rege  |.  ~+
        ;~  pose
          ;~((bend |=(a=[rege rege] (some [%eith a]))) mall ;~(pfix bar apex))
          (stag %eith ;~(plug (easy %empt) ;~(pfix bar apex)))
          (easy %empt)
        ==
      ::

XX document

### `++mall`

      ++  mall
        %+  knee  *rege  |.  ~+
        ;~((bend |=(a=[rege rege] (some [%pair a]))) bets mall)
      ::

XX document

### `++bets`

      ++  bets
        %+  knee  *rege  |.  ~+
        |=  tub=nail
        =+  vex=(chun tub)
        ?~  q.vex
          vex
        =+  a=p.u.q.vex
        %-  ;~  pose
              (cold [%eith %empt a] (jest '??'))
              (cold [%manl a] (jest '*?'))
              (cold [%plll a] (jest '+?'))
              (cold [%eith a %empt] wut)
              (cold [%mant a] tar)
              (cold [%plls a] lus)
              (stag %betl ;~(plug (easy a) ;~(sfix rang wut)))
              (stag %betw ;~(plug (easy a) rang))
              (stag %binl ;~(plug (easy a) (ifix [kel (jest ',}?')] dim:ag)))
              (stag %bant ;~(plug (easy a) (ifix [kel (jest '}?')] dim:ag)))
              (stag %bant ;~(plug (easy a) (ifix [kel ker] dim:ag)))
              (stag %bint ;~(plug (easy a) (ifix [kel (jest ',}')] dim:ag)))
              (easy a)
            ==
        q.u.q.vex
      ::

XX document

### `++ranc`

      ++  ranc
        |=  [a=@ b=@]
        ^-  @
        ?:((gth a b) 0 (con (bex a) $(a +(a))))
      ::

XX document

### `++flap`

      ++  flap  |=(a=@ (mix a (dec (bex 256))))
      ::

XX document

### `++rang`

      ++  rang
        %+  sear  |=([a=@ b=@] ?:((lte a b) (some [a b]) ~))
          (ifix [kel ker] ;~(plug dim:ag ;~(pfix com dim:ag)))
      ::

XX document

### `++chun`

      ++  chun
        %+  knee  *rege  |.  ~+
        ;~  pose
          (cold %ende buc)
          (cold %sart ket)
          (cold %dote dot)
          %+  cook  |=(a=(list char) (reel a |=([p=char q=rege] [%pair [%lite p] q])))
            ;~(pfix (jest '\\Q') cape)
          |=  tub=nail
          =+  foo=;~(plug kel dim:ag ;~(pose ker (jest ',}') ;~(plug com dim:ag ker)))
          =+  bar=(foo tub)
          ?~(q.bar (chad tub) (fail tub))
          (cook |=([a=rege] [%capt a 0]) (ifix [pel per] apex))
          %+  cook  |=([a=rege] [%capt a 0])
            (ifix [;~(plug (jest '(?P<') (plus aln) gar) per] apex)
          (ifix [(jest '(?:') per] apex)
          (stag %brac ;~(pfix sel seac))
        ==
      ::

XX document

### `++seac`

      ++  seac
        |=  tub=nail
        ?~  q.tub
          (fail tub)
        ?:  =(i.q.tub '^')
          (;~(pfix ket (cook flap sead)) tub)
        (sead tub)
      ::

XX document

### `++sead`

      ++  sead
        %+  knee  *@  |.  ~+
        ;~  pose
          |=  tub=nail
          ?~  q.tub
            (fail tub)
          ?.  =(i.q.tub ']')
            (fail tub)
          ?~  t.q.tub
            (fail tub)
          ?:  =(i.t.q.tub '-')
            ?~  t.t.q.tub
              (fail tub)
            ?:  =(i.t.t.q.tub ']')
              (;~(pfix ser (cook |=(a=@ (con (bex ']') a)) sade)) tub)
            (fail tub)
          (;~(pfix ser (cook |=(a=@ (con (bex ']') a)) sade)) tub)
          |=  tub=nail
          ?~  q.tub
            (fail tub)
          ?.  =(i.q.tub '-')
            (fail tub)
          ?~  t.q.tub
            (fail tub)
          ?:  =(i.t.q.tub '-')
            ?~  t.t.q.tub
              (fail tub)
            ?:  =(i.t.t.q.tub ']')
              (;~(pfix hep (cook |=(a=@ (con (bex '-') a)) sade)) tub)
            (fail tub)
          (;~(pfix hep (cook |=(a=@ (con (bex '-') a)) sade)) tub)
          (cook |=(a=[@ @] (con a)) ;~(plug seap sade))
        ==
      ::

XX document

### `++sade`

      ++  sade
        %+  knee  *@  |.  ~+
        ;~  pose
          (cold (bex '-') (jest '-]'))
          (cold 0 ser)
          (cook |=([p=@ q=@] `@`(con p q)) ;~(plug seap sade))
        ==
      ::

XX document

### `++seap`

      ++  seap
        %+  knee  *@  |.  ~+
        ;~  pose
          unid
          %+  ifix  (jest '[:')^(jest ':]')
          ;~(pose ;~(pfix ket (cook flap chas)) chas)
          %+  sear  |=([a=@ b=@] ?:((gth a b) ~ (some (ranc a b))))
            ;~(plug asp ;~(pfix hep alb))
          |=  tub=nail
          ?~  q.tub
            (fail tub)
          ?~  t.q.tub
            ((cook bex les) tub)
          ?:  =(i.t.q.tub '-')
            ?~  t.t.q.tub
              ((cook bex les) tub)
            ?:  =(i.t.t.q.tub ']')
              ((cook bex les) tub)
            (fail tub)
          ((cook bex les) tub)
          ;~(pfix bas escd)
        ==
      ::

XX document

### `++cape`

      ++  cape
        %+  knee  *tape  |.  ~+
        ;~  pose
          (cold ~ (jest '\\E'))
          ;~(plug next cape)
          (cook |=(a=char (tape [a ~])) next)
          (full (easy ~))
        ==

XX document

### `++chas`

      ++  chas                                              ::  ascii character set
        =-  (sear ~(get by -) sym)
        %-  mo  ^-  (list ,[@tas @I])
        :~  alnum/alnum  alpha/alpha  ascii/ascii  blank/blank  cntrl/cntrl
            digit/digit  graph/graph  lower/lower  print/print  punct/punct
            space/space  upper/upper  word/wordc   xdigit/xdigit
        ==
      ::  Character sets

++ alnum :(con lower upper digit)


    XX  document

    ###++alpha

++ alpha :(con lower upper)


    XX  document

    ###++ascii

++ ascii (ranc 0 127)


      ++  blank  (con (bex 32) (bex 9))

XX document

### `++cntrl`

      ++  cntrl  :(con (ranc 0 31) (bex 127))

XX document

### `++digit`

      ++  digit  (ranc '0' '9')

XX document

### `++graph`

      ++  graph  (ranc 33 126)

XX document

### `++lower`

      ++  lower  (ranc 'a' 'z')

XX document

### `++print`

      ++  print  (ranc 32 126)

XX document

### `++punct`

      ++  punct  ;:  con
                   (ranc '!' '/')
                   (ranc ':' '@')
                   (ranc '[' '`')
                   (ranc '{' '~')
                 ==

XX document

### `++space`

      ++  space  :(con (ranc 9 13) (bex ' '))

XX document

### `++upper`

      ++  upper  (ranc 'A' 'Z')

XX document

### `++white`

      ++  white  :(con (bex ' ') (ranc 9 10) (ranc 12 13))

XX document

### `++wordc`

      ++  wordc  :(con digit lower upper (bex '_'))

XX document

### `++xdigit`

      ++  xdigit  :(con (ranc 'a' 'f') (ranc 'A' 'F') digit)
      ::

XX document

### `++chad`

      ++  chad
        %+  knee  *rege  |.  ~+
        ;~(pose (stag %lite nor) (stag %brac unid) ;~(pfix bas escp))
      ::

XX document

### `++escd`

      ++  escd
        %+  knee  *@  |.  ~+
        ;~  pose
          (cold (bex 7) (just 'a'))
          (cold (bex 9) (just 't'))
          (cold (bex 10) (just 'n'))
          (cold (bex 11) (just 'v'))
          (cold (bex 12) (just 'f'))
          (cold (bex 13) (just 'r'))
          (cold (bex 0) (just '0'))
          (sear |=(a=@ ?:((lth a 256) (some (bex a)) ~)) (bass 8 (stun [2 3] cit)))
          (cook bex ;~(pfix (just 'x') (bass 16 (stun [2 2] hit))))
          (cook bex (ifix [(jest 'x{') ker] (bass 16 (stun [2 2] hit))))
          (cook bex mis)
        ==
      ::

XX document

### `++escp`

      ++  escp
        %+  knee  *rege  |.  ~+
        ;~  pose
          (cold %empt (just 'Q'))
          (cold [%lite `@tD`0] (just '0'))
          (cold [%lite `@tD`7] (just 'a'))
          (cold [%lite `@tD`9] (just 't'))
          (cold [%lite `@tD`10] (just 'n'))
          (cold [%lite `@tD`11] (just 'v'))
          (cold [%lite `@tD`12] (just 'f'))
          (cold [%lite `@tD`13] (just 'r'))
          (sear |=(a=@ ?:((lth a 256) (some [%lite a]) ~)) (bass 8 (stun [2 3] cit)))
          (stag %lite ;~(pfix (just 'x') (bass 16 (stun [2 2] hit))))
          (stag %lite (ifix [(jest 'x{') ker] (bass 16 (stun [2 2] hit))))
          (cold %dote (just 'C'))
          (cold %sart (just 'A'))
          (cold %ende (just 'z'))
          (cold %boun (just 'b'))
          (cold %bout (just 'B'))
          (stag %brac (cold wordc (just 'w')))
          (stag %brac (cold (flap wordc) (just 'W')))
          (stag %lite mis)
        ==
      ::

XX document

### `++unid`

      ++  unid
        %+  knee  *@  |.  ~+
        ;~  pose
          (cold digit (jest '\\d'))
          (cold (flap digit) (jest '\\D'))
          (cold white (jest '\\s'))
          (cold (flap white) (jest '\\S'))
          (cold wordc (jest '\\w'))
          (cold (flap wordc) (jest '\\W'))
        ==
      --
    ::

XX document

### `++ra`

    ++  ra                                                  ::  regex engine
      |_  a=rege

XX document

### `++proc`

      ++  proc                                              ::  capture numbering
        |=  b=@
        =-  -(+ +>.$(a a))
        ^-  [p=@ a=rege]
        ?-  a
          [%capt *]  =+  foo=$(a p.a, b +(b))
                     [p.foo [%capt a.foo b]]
          [%eith *]  =+  foo=$(a p.a)
                     =+  bar=$(a q.a, b p.foo)
                     [p.bar [%eith a.foo a.bar]]
          [%pair *]  =+  foo=$(a p.a)
                     =+  bar=$(a q.a, b p.foo)
                     [p.bar [%pair a.foo a.bar]]
          [%manl *]  =+  foo=$(a p.a)
                     [p.foo [%manl a.foo]]
          [%plll *]  =+  foo=$(a p.a)
                     [p.foo [%plll a.foo]]
          [%binl *]  =+  foo=$(a p.a)
                     [p.foo [%binl a.foo q.a]]
          [%betl *]  =+  foo=$(a p.a)
                     [p.foo [%betl a.foo q.a r.a]]
          [%mant *]  =+  foo=$(a p.a)
                     [p.foo [%mant a.foo]]
          [%plls *]  =+  foo=$(a p.a)
                     [p.foo [%plls a.foo]]
          [%bant *]  =+  foo=$(a p.a)
                     [p.foo [%bant a.foo q.a]]
          [%bint *]  =+  foo=$(a p.a)
                     [p.foo [%bint a.foo q.a]]
          [%betw *]  =+  foo=$(a p.a)
                     [p.foo [%betw a.foo q.a r.a]]
          *  [b a]
        ==
      ::

XX document

### `++cont`

      ++  cont
        |=  [a=(map ,@u tape) b=(map ,@u tape)]
        (~(gas by _(map ,@u tape)) (weld (~(tap by a)) (~(tap by b))))
      ::

XX document

### `++abor`

      ++  abor
        |=  [a=char b=(unit ,[tape (map ,@u tape)])]
        ^-  (unit ,[tape (map ,@u tape)])
        ?~  b
          b
        [~ [[a -.u.b] +.u.b]]
      ::

XX document

### `++matc`

      ++  matc
        |=  [b=tape c=tape]
        ^-  (unit (map ,@u tape))
        =+  foo=`(unit ,[tape (map ,@u tape)])`(deep b %empt c)
        (bind foo |*(a=^ (~(put by +.a) 0 -.a)))
      ::

XX document

### `++chet`

      ++  chet
        |=  [b=(unit ,[tape (map ,@u tape)]) c=tape d=tape]
        ^-  (unit ,[tape (map ,@u tape)])
        ?~  b
          b
        ?~  -.u.b
          b
        =+  bar=(deep (slag (lent -.u.b) c) %empt d)
        ?~  bar
          bar
        b

XX document

### `++blak`

      ++  blak  (some ["" _(map ,@u tape)])

XX document

### `++word`

      ++  word  |=(a=char =((dis wordc:rags (bex a)) 0))

XX document

### `++deep`

      ++  deep
        |=  [b=tape c=rege d=tape]
        ^-  (unit ,[tape (map ,@u tape)])
        ?-  a
          %dote  ?~(b ~ (some [[i.b ~] _(map ,@u tape)]))
          %ende  ?~(b blak ~)
          %sart  ?:(=(b d) blak ~)
          %empt  blak
          %boun  =+  ^=  luc
                     ?:  =(b d)
                       &
                     =+  foo=(slag (dec (sub (lent d) (lent b))) d)
                     (word -.foo)
                 =+  cuc=?~(b & (word -.b))
                 ?:(!=(luc cuc) blak ~)
          %bout  =+  ^=  luc
                     ?:  =(b d)
                       &
                     =+  foo=(slag (dec (sub (lent d) (lent b))) d)
                     (word -.foo)
                 =+  cuc=?~(b & (word -.b))
                 ?:(=(luc cuc) blak ~)
          [%capt *]  =+  foo=$(a p.a)
                     ?~  foo
                       foo
                     =+  ft=u.foo
                     =+  bar=$(a c, b (slag (lent -.ft) b), c %empt)
                     ?~  bar
                       bar
                     [~ [-.ft (~(put by +.ft) q.a -.ft)]]
          [%lite *]  ?~(b ~ ?:(=(i.b p.a) (some [[i.b ~] _(map ,@u tape)]) ~))
          [%brac *]  ?~  b
                       ~
                     ?.  =((dis (bex `@`i.b) p.a) 0)
                       (some [[i.b ~] _(map ,@u tape)])
                     ~
          [%eith *]  =+  foo=(chet(a c) $(a p.a) b d)
                     =+  bar=(chet(a c) $(a q.a) b d)
                     ?~  foo
                       bar
                     ?~  bar
                       foo
                     =+  ft=u.foo
                     =+  bt=u.bar
                     ?:  (gte (lent -.ft) (lent -.bt))
                       foo
                     bar
          [%pair *]  =+  foo=$(a p.a, c [%pair q.a c])
                     ?~  foo
                       foo
                     =+  ft=u.foo
                     =+  bar=$(a q.a, b (slag (lent -.ft) b))
                     ?~  bar
                       bar
                     =+  bt=u.bar
                     [~ [(weld -.ft -.bt) (cont +.ft +.bt)]]
          [%manl *]  =+  foo=$(a p.a)
                     ?~  foo
                       blak
                     ?~  -.u.foo
                       blak
                     $(a [%eith %empt [%pair p.a [%eith %empt a]]])
          [%mant *]  =+  foo=$(a p.a)
                     ?~  foo
                       blak
                     =+  ft=u.foo
                     ?~  -.ft
                       blak
                     $(a [%eith [%pair p.a [%eith a %empt]] %empt])
          [%plls *]  $(a [%pair p.a [%mant p.a]])
          [%plll *]  $(a [%pair p.a [%manl p.a]])
          [%binl *]  =+  min=?:(=(q.a 0) 0 (dec q.a))
                     ?:  =(q.a 0)
                       $(a [%manl p.a])
                     $(a [%pair p.a [%binl p.a min]])
          [%bant *]  ?:  =(0 q.a)
                       blak
                     $(a [%pair p.a [%bant p.a (dec q.a)]])
          [%bint *]  =+  min=?:(=(q.a 0) 0 (dec q.a))
                     ?:  =(q.a 0)
                       $(a [%mant p.a])
                     $(a [%pair p.a [%bint p.a min]])
          [%betw *]  ?:  =(0 r.a)
                       blak
                     ?:  =(q.a 0)
                       $(a [%eith [%pair p.a [%betw p.a 0 (dec r.a)]] %empt])
                     $(a [%pair p.a [%betw p.a (dec q.a) (dec r.a)]])
          [%betl *]  ?:  =(0 r.a)
                       blak
                     ?:  =(q.a 0)
                       $(a [%eith %empt [%pair p.a [%betl p.a 0 (dec r.a)]]])
                     $(a [%pair p.a [%betl p.a (dec q.a) (dec r.a)]])
        ==
      --
    ::

XX document

### `++rexp`

    ++  rexp                                                :: Regex match
      ~/  %rexp
      |=  [a=tape b=tape]
      ^-  (unit (unit (map ,@u tape)))
      =+  ^=  bar
          |=  [a=@ b=(map ,@u tape)]
          ?:  =(a 0)
            b
          =+  c=(~(get by b) a)
          ?~  c
            $(a (dec a), b (~(put by b) a ""))
          $(a (dec a))
      =+  par=(pars a)
      ?~  par  ~
      =+  poc=(~(proc ra u.par) 1)
      =+  c=b
      |-
      =+  foo=(matc:poc c b)
      ?~  foo
        ?~  c
          [~ ~]
        $(c t.c)
      [~ [~ (bar (dec p.poc) u.foo)]]
    ::

XX document

### `++repg`

    ++  repg                                                :: Global regex replace
      ~/  %repg
      |=  [a=tape b=tape c=tape]
      ^-  (unit tape)
      =+  par=(pars a)
      ?~  par  ~
      =+  poc=(~(proc ra u.par) 1)
      =+  d=b
      :-  ~
      |-
      ^-  tape
      =+  foo=(matc:poc d b)
      ?~  foo
        ?~  d
          ~
        [i.d $(d t.d)]
      =+  ft=(need (~(get by u.foo) 0))
      ?~  d
        c
      (weld c $(d `tape`(slag (lent ft) `tape`d)))
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
