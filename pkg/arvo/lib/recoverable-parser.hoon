|%
+$  parse-error  [=pint message=@t]
::
::  parsing output
+$  edge  [p=hair q=(unit [p=* q=nail]) r=(list parse-error)]
::
::  parsing rule
++  rule  _|:($:nail $:edge)
::
::  generic edge
++  like  |*  a/$-(* *)
          |:  b=`*`[(hair) ~ ~]
          :-  p=(hair -.b)
          :_  r=((list parse-error) +>.b)
          ^=  q
          ?@  +<.b  ~
          :-  ~
          u=[p=(a +<+<.b) q=[p=(hair -.b) q=(tape +<.b)]]
::  product from edge
++  wonk  =+  veq=$:edge
          |@  ++  $  ?~(q.veq !! p.u.q.veq)           
          --
++  parse-with-error
  |*  [message=@t sab=rule]
  |=  tub=nail
  =+  roq=(sab tub)
  =/  =parse-error
    [[p.tub p.roq] message]
  [p=p.roq q=q.roq r=[parse-error r.roq]]
::
:: outside callers
::  ++  rash  |*({naf/@ sab/rule} (scan (trip naf) sab))
::  ++  rose  |*  {los/tape sab/rule}
::            =+  vex=(sab [[1 1] los])
::            =+  len=(lent los)
::            ?.  =(+(len) q.p.vex)  [%| p=(dec q.p.vex)]
::            ?~  q.vex
::              [%& p=~]
::            [%& p=[~ u=p.u.q.vex]]
::  ++  rush  |*({naf/@ sab/rule} (rust (trip naf) sab))
::  ++  rust  |*  {los/tape sab/rule}
::            =+  vex=((full sab) [[1 1] los])
::            ?~(q.vex ~ [~ u=p.u.q.vex])
++  scan  |*  {los/tape sab/rule}
          =+  vex=(sab [1 1] los)
          ?~  q.vex
            ~_  (show [%m '{%d %d}'] p.p.vex q.p.vex ~)
            ~_(leaf+"syntax error" !!)
          [p.u.q.vex r.vex]
::
:: pronunciation to rule
::  +|  %glyphs
::
++  ace  (just ' ')
++  ban  (just '>')
++  bar  (just '|')
++  bas  (just '\\')  ::  XX deprecated
++  bat  (just '\\')
++  buc  (just '$')   ::  XX deprecated
++  bus  (just '$')
++  cab  (just '_')
++  cen  (just '%')
++  col  (just ':')
++  com  (just ',')
++  dot  (just '.')
++  fas  (just '/')  ::  XX deprecated?
++  gal  (just '<')   ::  XX deprecated
++  gar  (just '>')   ::  XX deprecated
++  vat  (just '@')   ::  pronounced "at"
++  hax  (just '#')
++  hep  (just '-')   ::  pronounced "ep"
++  ket  (just '^')
++  leb  (just '{')
++  led  (just '<')
++  lob  (just '{')
++  lit  (just '(')
++  lac  (just '[')
++  lus  (just '+')
++  mic  (just ';')   ::  pronounced "mick"
++  net  (just '/')
++  pad  (just '&')
++  rac  (just ']')
++  reb  (just '}')
++  rob  (just '}')
++  rit  (just ')')
++  say  (just '\'')
++  sig  (just '~')
++  tar  (just '*')
++  tec  (just '`')
++  tis  (just '=')   ::  pronounced "is"
++  toc  (just '"')   ::  XX deprecated
++  yel  (just '"')
++  wut  (just '?')
++  zap  (just '!')
::

::  %|  %idioms

++  low  (shim 'a' 'z')                                 ::  lowercase
::
++  prn  ;~(less (just `@`127) (shim 32 256))           ::  non-control
++  nud  (shim '0' '9')                                 ::  numeric
++  gah  (mask [`@`10 ' ' ~])                           ::  newline or ace
++  gap  (cold ~ ;~(plug gaq (star ;~(pose vul gah))))  ::  plural space
++  gaq  ;~  pose                                       ::  end of line
             (just `@`10)
             ;~(plug gah ;~(pose gah vul))
             vul
         ==
++  gaw  (cold ~ (star ;~(pose vul gah)))               ::  classic white
++  gay  ;~(pose gap (easy ~))                          ::
++  vul  %+  cold   ~                                   ::  comments
         ;~  plug  col  col
           (star prn)
           (just `@`10)
         ==
::
::  +|  %combinators
::
::  arbitrary compose
++  comp
  ~/  %comp
  =+  raq=|*({a/* b/*} [a b])
  |@
  ++  $
    ~/  %fun
    |*  {vex/edge sab/rule}
    ?~  q.vex
      vex
    =+  yit=(sab q.u.q.vex)
    =+  yur=(last p.vex p.yit)
    ?~  q.yit
      [p=yur q=q.yit r=~]
    [p=yur q=[~ u=[p=(raq p.u.q.vex p.u.q.yit) q=q.u.q.yit]] r=(weld r.vex r.yit)]
  --
::
::  XX redundant, jest
::  match a char
++  just
  |=  daf/char
  |=  tub/nail
  ^-  (like char)
  ?~  q.tub
    (fail tub)
  ?.  =(daf i.q.tub)
    (fail tub)
  (next tub)
::
::
::  first then second
++  plug
  |*  {vex/edge sab/rule}
  ?~  q.vex
    vex
  =+  yit=(sab q.u.q.vex)
  =+  yur=(last p.vex p.yit)
  =+  errs=(weld r.yit r.vex)
  ?~  q.yit
    [p=yur q=q.yit r=errs]
  [p=yur q=[~ u=[p=[p.u.q.vex p.u.q.yit] q=q.u.q.yit]] r=errs]
::
::  add a label
++  stag
  |*  {gob/* sef/rule}
  |=  tub/nail
  =+  vex=(sef tub)
  ?~  q.vex
    vex
  [p=p.vex q=[~ u=[p=[gob p.u.q.vex] q=q.u.q.vex]] r=r.vex]
::
::  first or second
++  pose
  |*  {vex/edge sab/rule}
  ~!  sab
  ?~  q.vex
    =+  roq=(sab)
    [p=(last p.vex p.roq) q=q.roq r=(weld r.roq r.vex)]
  vex
::
::  no first and second
++  less
  |*  {vex/edge sab/rule}
  ?~  q.vex
    =+  roq=(sab)
    [p=(last p.vex p.roq) q=q.roq r=r.roq]
  (fail +<.sab)
::
::
++  star
  |*  fel/rule
  (stir `(list _(wonk *fel))`~ |*({a/* b/*} [a b]) fel)
::  One or more matches
++  plus  |*(fel/rule ;~(plug fel (star fel)))          ::
++  stir
  |*  {rud/* raq/_=>(~ |*({a/* b/*} [a b])) fel/rule}
  |=  tub/nail
  ^-  (like _rud)
  =+  vex=(fel tub)
  ?~  q.vex
    [p.vex [~ rud tub] r=r.vex]
  =+  wag=$(tub q.u.q.vex)
  ?>  ?=(^ q.wag)
  [(last p.vex p.wag) [~ (raq p.u.q.vex p.u.q.wag) q.u.q.wag] r=(weld r.vex r.wag)]
::
:: infix, discarding delimiters
++  ifix
  |*  [fel=[rule rule] hof=rule]
  ;~(pfix -.fel ;~(sfix hof +.fel))
::
::  discard second rule
++  sfix
  |*  sam={vex/edge sab/rule}
  %.  sam
  (comp |*({a/* b/*} a))
::
::  discard first rule
++  pfix
  |*  sam={vex/edge sab/rule}
  %.  sam
  (comp |*({a/* b/*} b))
::
::  always parse
++  easy
  |*  huf/*
  |=  tub/nail
  ^-  (like _huf)
  [p=p.tub q=[~ u=[p=huf q=tub]] r=~]
::
::  match char in range
++  shim
  |=  {les/@ mos/@}
  |=  tub/nail
  ^-  (like char)
  ?~  q.tub
    (fail tub)
  ?.  ?&((gte i.q.tub les) (lte i.q.tub mos))
    (fail tub)
  (next tub)
::
::  consume a char
++  next
  |=  tub/nail
  ^-  (like char)
  ?~  q.tub
    (fail tub)
  =+  zac=(lust i.q.tub p.tub)
  [zac [~ i.q.tub [zac t.q.tub]] r=~]
::
::  parse several times
++  stun
  |*  {lig/{@ @} fel/rule}
  |=  tub/nail
  ^-  (like (list _(wonk (fel))))
  ?:  =(0 +.lig)
    [p.tub [~ ~ tub] r=~]
  =+  vex=(fel tub)
  ?~  q.vex
    ?:  =(0 -.lig)
      [p.vex [~ ~ tub] r=r.vex]
    vex
  =+  ^=  wag  %=  $
                 -.lig  ?:(=(0 -.lig) 0 (dec -.lig))
                 +.lig  ?:(=(0 +.lig) 0 (dec +.lig))
                 tub  q.u.q.vex
               ==
  ?~  q.wag
    wag
  [p.wag [~ [p.u.q.vex p.u.q.wag] q.u.q.wag] r=(weld r.vex r.wag)]
::
::  replace w+ constant
++  cold
  |*  {cus/* sef/rule}
  |=  tub/nail
  =+  vex=(sef tub)
  ?~  q.vex
    vex
  [p=p.vex q=[~ u=[p=cus q=q.u.q.vex]] r=r.vex]
::
::
++  mask                                                ::  match char in set
  |=  bud/(list char)
  |=  tub/nail
  ^-  (like char)
  ?~  q.tub
    (fail tub)
  ?.  (lien bud |=(a/char =(i.q.tub a)))
    (fail tub)
  (next tub)
::
:: never parse
++  fail  |=(tub/nail [p=p.tub q=~ r=~])
::
::  match a cord
++  jest
  |=  daf/@t
  |=  tub/nail
  =+  fad=daf
  |-  ^-  (like @t)
  ?:  =(`@`0 daf)
    [p=p.tub q=[~ u=[p=fad q=tub]] r=~]
  ?:  |(?=(~ q.tub) !=((end 3 1 daf) i.q.tub))
    (fail tub)
  $(p.tub (lust i.q.tub p.tub), q.tub t.q.tub, daf (rsh 3 1 daf))
::
::  callbacks
++  knee
  =|  {gar/* sef/_|.(*rule)}
  |@  ++  $
        |=  tub/nail
        ^-  (like _gar)
        ((sef) tub)
  --
--
