!:  =~
|%
++  twyg  $&  [p=twyg q=twyg]
          $%
            [%bccb p=tyle]
            [%bccm p=tyle]
            [%bctr p=tyle]
            [%brls p=tyle q=twyg]
            [%brtr p=tyle q=twyg]
            [%brts p=tyle q=twyg]
            [%brcn p=(map term ,[?(%ash %elm) twyg])]
            [%cltr p=tufk]
            [%clsg p=tufk]
            [%clhp p=twyg q=twyg]
            [%cncl p=twyg q=twyg]
            [%cnhp p=twyg q=tufk]
            [%cnzy p=term]
            [%dtzy p=dime]
            [%dtzz p=dime]
            [%ktts p=term q=twyg]
            [%smsm p=tyle q=twyg]
            [%smdq p=(list $|(@ [~ twyg]))]
            [%tsgr p=twyg q=twyg]
            [%tsls p=twyg q=twyg]
            [%tssg p=tufk]
            [%wtcl p=twyg q=twyg r=twyg]
            [%wtdt p=twyg q=twyg r=twyg]
            
            [%clcb p=twyg q=twyg]
            [%cndt p=twyg q=twyg]
            [%dttr p=twyg q=twyg]
            [%dtts p=twyg q=twyg]
            [%ktdt p=twyg q=twyg]
            [%ktls p=twyg q=twyg]
            [%kthx p=twyg q=twyg]
            [%sgbr p=twyg q=twyg]
            [%sgcb p=twyg q=twyg]
            [%sgts p=twyg q=twyg]
            [%sgzp p=twyg q=twyg]
            [%tscn p=twyg q=twyg]
            [%tsfs p=twyg q=twyg]
            [%tsgl p=twyg q=twyg]
            [%tshp p=twyg q=twyg]
            [%wtgl p=twyg q=twyg]
            [%wtgr p=twyg q=twyg]
            [%zpcm p=twyg q=twyg]
            [%zpsm p=twyg q=twyg]

            [%zpzp ~]
          ==
::
++  tyle  $&  [p=tyle q=tyle]
          $%  [%axil p=base]
              [%bark p=term q=tyle]
              [%fern p=(mist tyle)]
              [%leaf p=dime]
          ==
:: 
++  tufk  (list twyg)
++  tamk  $%  [%leaf p=tape]
              [%lone n=rune p=tamk]
              [%pair n=rune p=tamk q=tamk]
              [%trel n=rune p=tamk q=tamk r=tamk]
              [%quad n=rune p=tamk q=tamk r=tamk s=tamk]
              [%many n=rune p=(list tamk)]
          ==
::
++  rune  term
++  mist  |*(a=_,* ,[i=a t=(list a)])
--
=+  ^=  tex
    =+  ^=  a
        %-  ream
        '''
        |%
        ++  ace  (just ' ')                                     ::  spACE
        ++  bar  (just '|')                                     ::  vertical BAR
        ++  bas  (just '\\')                                    ::  Back Slash (escaped)
        ++  buc  (just '$')                                     ::  dollars BUCks
        ++  cab  (just '_')                                     ::  CABoose
        ++  cen  (just '%')                                     ::  perCENt
        ++  col  (just ':')                                     ::  COLon
        ++  com  (just ',')                                     ::  COMma
        ++  doq  (just '"')                                     ::  Double Quote
        ++  dot  (just '.')                                     ::  dot dot dot ...
        ++  fas  (just '/')                                     ::  Forward Slash
        ++  gal  (just '<')                                     ::  Greater Left
        ++  gar  (just '>')                                     ::  Greater Right
        ++  hax  (just '#')                                     ::  Hash
        ++  kel  (just '{')                                     ::  Curly Left
        ++  ker  (just '}')                                     ::  Curly Right
        ++  ket  (just '^')                                     ::  CareT
        ++  lus  (just '+')                                     ::  pLUS
        ++  hep  (just '-')                                     ::  HyPhen
        ++  pel  (just '(')                                     ::  Paren Left
        ++  pam  (just '&')                                     ::  AMPersand pampersand
        ++  per  (just ')')                                     ::  Paren Right
        ++  pat  (just '@')                                     ::  AT pat
        ++  sel  (just '[')                                     ::  Square Left
        ++  sem  (just ';')                                     ::  SEMicolon
        ++  ser  (just ']')                                     ::  Square Right
        ++  sig  (just '~')                                     ::  SIGnature squiggle
        ++  soq  (just '\'')                                    ::  Single Quote
        ++  tar  (just '*')                                     ::  sTAR
        ++  tec  (just '`')                                     ::  backTiCk
        ++  tis  (just '=')                                     ::  'tis tis, it is
        ++  wut  (just '?')                                     ::  wut, what?
        ++  zap  (just '!')                                     ::  zap! bang! crash!!
        --
        '''
    =+  b=(~(tap by ;;((map ,@tas ,[@ @t ^ [@ @ p=@t] ~]) +.a)))
    %-  ~(gas by *(map ,@tas ,@t))
    (turn b |=(_i.-.b [(rap 3 (cut 3 [0 1] p) (cut 3 [2 1] p) ~) p.q]))
|%
++  temt
  |=  a=@tasG
  `tape`~[(~(got by tex) (cut 4 [0 1] a)) (~(got by tex) (cut 4 [1 1] a))]
::
++  mear
  |=  a=twig
  ^-  @t
  %-  role
  (turn `wall`~(rend ta (payg (flag a))) crip)
::
++  flat
  |=  a=tile
  ^-  tyle
  ?-  -.a
    ^  [$(a p.a) $(a q.a)]
    ?(%axil %leaf)  a
    %bark  a(q $(a q.a))
    ?(%bush %reed)  fern/~[$(a p.a) $(a q.a)]  :: XX a
    %fern  a(p [$(a i.p.a) (turn t.p.a ..$)])
    ?(%herb %weed %kelp)  (tyle a)  ::  XX
  ==
::
++  limb-to-cord
  |=  b=limb  ^-  cord
  ?@  b  ?~(b '$' b)
  ?-  -.b
    %|  (cat 3 (fil 3 p.b '^') q.b)
    %&  ?:  =(1 (mod 2 (met 0 p.b)))
          (rsh 3 1 $(p.b (lsh 0 1 p.b)))
        =+  (slag 1 (rip 1 p.b))
        ?:  =(~ -)  '.'
        (rap 3 (turn - ~(got by (mo 0^'-<' 1^'->' 2^'+<' 3^'+>' ~))))
  ==
++  flag
  |=  a=twig
  ^-  twyg
  ?-  -.a
      ^
    [$(a p.a) $(a q.a)]
      %bczp
    ?:  ?=(%null p.a)  [%dtzz %n ~]
    $(a ~(open ap a))
      ?(%bccb %bccm %bctr)
    a(p (flat p.a))
      ?(%brls %brtr %brts %smsm)
    a(p (flat p.a), q $(a q.a))
      %brcn
    =<  a(p (~(run by p.a) .))
    |=(b=foot ?+(-.b !! ?(%ash %elm) b(p (flag p.b))))
      ?(%clsg %cltr %tssg)
    a(p (turn p.a ..$))
      %cnts
    ?^  q.a  cnzy/'-lost-cnts-mods'
    ?~  p.a  !!
    ?^  t.p.a  cnzy/(cat 3 '-lost-wing-many.' (crip <p.a>))
    cnzy/(limb-to-cord i.p.a)
      %dtzz
    a(q (,@ q.a))
      %ktts
    a(p (term p.a), q $(a q.a))
      ?(%tsls %tsgr %clhp %cncl)
    a(p $(a p.a), q $(a q.a))
      ?(%clcb %cndt %dttr %dtts %ktdt %ktls %kthx %sgbr %sgcb %sgts)
    a(p $(a p.a), q $(a q.a))
      ?(%sgzp %tscn %tsfs %tsgl %tshp %wtgl %wtgr %zpcm %zpsm)
    a(p $(a p.a), q $(a q.a))
      ?(%wtcl %wtdt)
    a(p $(a p.a), q $(a q.a), r $(a r.a))
      %smdq
    a(p (turn p.a |=(b=beer ?@(b b [~ ^$(a p.b)]))))
      @
    =+  b=(twyg a)
    ?:  =(a b)  b
    =+  c=~(open ap a)
    ::~&  open/[a c]
    ?.  =(a c)  $(a c)
    cnzy/(cat 3 '-lost-rune.' -.a)
   ==
::
++  payl
  |=  a=tyle
  ^-  tamk
  ?-    -.a
      ^
    :+  %many  %bccl
    |-  ^-  (list tamk)
    :-  ^$(a p.a)
    ?^  -.q.a  $(a q.a)
    [^$(a q.a)]~
      %axil
    :-  %leaf
    ?-    p.a
      [%atom @]  ['@' (trip p.p.a)]
      %noun  "*"
      %cell  "^"
      %bean  "?"
      %null  "~"
    ==
      %bark  pair/[%bcts leaf/(trip p.a) $(a q.a)]
      %leaf  leaf/(scow p.a)
      %fern  many/[%bccl (turn `(list tyle)`p.a ..$)]
  ==
::
++  back  |=(a=tank `tamk`leaf/~(ram re a))    ::  from old tank
++  payg
  |=  a=twyg
  ^-  tamk
  ?-  -.a
      ^
    ?.  ?=($|(%clhp ^) -.q.a)
      pair/[%clhp $(a p.a) $(a q.a)]
    ?@  -.q.a  $(q.a +.q.a)
    ?.  ?=($|(%clhp ^) -.q.q.a)
      trel/[%clls $(a p.a) $(a p.q.a) $(a q.q.a)]
    ?@  -.q.q.a  $(q.q.a +.q.q.a)
    quad/[%clkt $(a p.a) $(a p.q.a) $(a p.q.q.a) $(a q.q.q.a)]
    ::
      ?(%bccb %bccm %bctr)
    lone/[-.a (payl p.a)]
      ?(%brls %brtr %brts %smsm)
    pair/[-.a (payl p.a) $(a q.a)]
      %brcn
    many/[-.a (turn (~(tap by p.a)) paym)]
      ?(%cltr %clsg %tssg)
    many/[-.a (turn p.a ..$)]
      %cnhp
    =+  tal=?:(?=([^ ~] q.a) i.q.a cltr/q.a)
    pair/[-.a $(a p.a) $(a tal)]
      %cnzy  leaf/(trip p.a)
      %dtzy  (back (sell [%atom p.p.a] q.p.a))
      %dtzz  (back (sell [%cube q.p.a [%atom p.p.a]] q.p.a))
      %ktts  pair/[-.a leaf/(trip p.a) $(a q.a)]
      %clhp  $(a +.a)
      ?(%tsls %tsgr %cncl)
    pair/[-.a $(a p.a) $(a q.a)]
      ?(%clcb %cndt %dttr %dtts %ktdt %ktls %kthx %sgbr %sgcb %sgts)
    pair/[-.a $(a p.a) $(a q.a)]
      ?(%sgzp %tscn %tsfs %tsgl %tshp %wtgl %wtgr %zpcm %zpsm)
    pair/[-.a $(a p.a) $(a q.a)]
      %smdq
    =<  leaf/"\"{(zing (turn p.a .))}\""
    |=  b=$|(@ [~ p=twyg])  ^-  tape
    ?@  b  ~[b]
    ?:  ?=([%cltr ^ ~] p.b)
      $(p.b i.p.p.b)
    =+  c=^$(a p.b)
    ?+  -.c  "-rune-wide.{<`term`?^(-.p.b %cell -.p.b)>}"
      %leaf  "\{{p.c}}"
    ==
      ?(%wtcl %wtdt)
    trel/[-.a $(a p.a) $(a q.a) $(a r.a)] 
      %zpzp  leaf/(temt -.a)
  ==
++  paym
  |=  [a=term b=?(%ash %elm) c=twyg]  ^-  tamk
  :+  %pair  ?-(b %ash %lsls, %elm %lshp)
  [leaf/(trip a) (payg c)]
::
++  ta
  =|  [tab=@ edg=_80]
  |_  tam=tamk
  ++  tmix
    |=  [a=tape b=tape]
    `tape`(weld a (slag (lent a) b))
  ::
  ++  spac
    |=([a=@u b=tape] `tape`?~(a b [' ' $(a (dec a))]))
  ::
  ++  rend
    |-
    =-  [i=(spac tab i) t=(turn t (cury spac tab))]
    ^-  (mist tape)
    ?-  -.tam
        %leaf
      [p.tam]~
        %lone
      =+  hed=$(tab 0, tam p.tam)
      ?~  t.hed  [(tmix (temt n.tam) (spac 4 i.hed))]~
      [(temt n.tam) hed]
        %pair
      =+  hed=$(tab 4, tam p.tam)
      [(tmix (temt n.tam) i.hed) (weld t.hed $(tab 0, tam q.tam))]
        %trel
      =+  hed=$(tab 4, tam p.tam)
      :-  (tmix (temt n.tam) i.hed)
      :(weld t.hed $(tab 2, tam q.tam) $(tab 0, tam r.tam))
        %quad
      =+  hed=$(tab 6, tam p.tam)
      :-  (tmix (temt n.tam) i.hed)
      :(weld t.hed $(tab 4, tam q.tam) $(tab 2, tam r.tam) $(tab 0, tam s.tam))
        %many
      =.  tab  4
      ?~  p.tam  [(temt n.tam)]~
      =+  hed=$(tam i.p.tam)
      =+  tas=t.p.tam
      =+  tal=|-(`wall`?~(tas ["=="]~ (weld ^$(tam i.tas) $(tas t.tas))))
      [(tmix (temt n.tam) i.hed) (weld t.hed tal)]
    ==
  --
--
  ==
