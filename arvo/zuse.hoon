!:
::  zuse (3), standard library (tang)
::
|%
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 3b, Arvo libraries            ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bA, lite number theory       ::
::
++  egcd                                                ::  schneier's egcd
  |=  [a=@ b=@]
  =+  si
  =+  [c=(sun a) d=(sun b)]
  =+  [u=[c=(sun 1) d=--0] v=[c=--0 d=(sun 1)]]
  |-  ^-  [d=@ u=@ v=@]
  ?:  =(--0 c)
    [(abs d) d.u d.v]
  ::  ?>  ?&  =(c (sum (pro (sun a) c.u) (pro (sun b) c.v)))
  ::          =(d (sum (pro (sun a) d.u) (pro (sun b) d.v)))
  ::      ==
  =+  q=(fra d c)
  %=  $
    c  (dif d (pro q c))
    d  c
    u  [(dif d.u (pro q c.u)) c.u]
    v  [(dif d.v (pro q c.v)) c.v]
  ==
::
++  pram                                                ::  rabin-miller
  |=  a=@  ^-  ?
  ?:  ?|  =(0 (end 0 1 a))
          =(1 a)
          =+  b=1
          |-  ^-  ?
          ?:  =(512 b)
            |
          ?|(=+(c=+((mul 2 b)) &(!=(a c) =(a (mul c (div a c))))) $(b +(b)))
      ==
    |
  =+  ^=  b
      =+  [s=(dec a) t=0]
      |-  ^-  [s=@ t=@]
      ?:  =(0 (end 0 1 s))
        $(s (rsh 0 1 s), t +(t))
      [s t]
  ?>  =((mul s.b (bex t.b)) (dec a))
  =+  c=0
  |-  ^-  ?
  ?:  =(c 64)
    &
  =+  d=(~(raw og (add c a)) (met 0 a))
  =+  e=(~(exp fo a) s.b d)
  ?&  ?|  =(1 e)
          =+  f=0
          |-  ^-  ?
          ?:  =(e (dec a))
            &
          ?:  =(f (dec t.b))
            |
          $(e (~(pro fo a) e e), f +(f))
      ==
      $(c +(c))
  ==
::
++  ramp                                                ::  make r-m prime
  |=  [a=@ b=(list ,@) c=@]  ^-  @ux                    ::  [bits snags seed]
  =>  .(c (shas %ramp c))
  =+  d=_@
  |-
  ?:  =((mul 100 a) d)
    ~|(%ar-ramp !!)
  =+  e=(~(raw og c) a)
  ?:  &(|-(?~(b & &(!=(1 (mod e i.b)) $(b +.b)))) (pram e))
    e
  $(c +(c), d (shax d))
::
++  fo                                                  ::  modulo prime
  |_  a=@
  ++  dif
    |=  [b=@ c=@]
    (sit (sub (add a b) c))
  ::
  ++  exp
    |=  [b=@ c=@]
    ?:  =(0 b)
      1
    =+  d=$(b (rsh 0 1 b))
    =+  e=(pro d d)
    ?:(=(0 (end 0 1 b)) e (pro c e))
  ::
  ++  fra
    |=  [b=@ c=@]
    (pro b (inv c))
  ::
  ++  inv
    |=  b=@
    =+  c=(dul:si u:(egcd b a) a)
    c
  ::
  ++  pro
    |=  [b=@ c=@]
    (sit (mul b c))
  ::
  ++  sit
    |=  b=@
    (mod b a)
  ::
  ++  sum
    |=  [b=@ c=@]
    (sit (add b c))
  --
::
++  fu                                                  ::  modulo (mul p q)
  |=  a=[p=@ q=@]
  =+  b=?:(=([0 0] a) 0 (~(inv fo p.a) (~(sit fo p.a) q.a)))
  |%
  ++  dif
    |=  [c=[@ @] d=[@ @]]
    [(~(dif fo p.a) -.c -.d) (~(dif fo q.a) +.c +.d)]
  ::
  ++  exp
    |=  [c=@ d=[@ @]]
    :-  (~(exp fo p.a) (mod c (dec p.a)) -.d)
    (~(exp fo q.a) (mod c (dec q.a)) +.d)
  ::
  ++  out                                               ::  garner's formula
    |=  c=[@ @]
    %+  add
      +.c
    (mul q.a (~(pro fo p.a) b (~(dif fo p.a) -.c (~(sit fo p.a) +.c))))
  ::
  ++  pro
    |=  [c=[@ @] d=[@ @]]
    [(~(pro fo p.a) -.c -.d) (~(pro fo q.a) +.c +.d)]
  ::
  ++  sum
    |=  [c=[@ @] d=[@ @]]
    [(~(sum fo p.a) -.c -.d) (~(sum fo q.a) +.c +.d)]
  ::
  ++  sit
    |=  c=@
    [(mod c p.a) (mod c q.a)]
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bB, cryptosuites             ::
::
++  crya                                                ::  cryptosuite A (RSA)
  ^-  acro
  =|  [mos=@ pon=(unit ,[p=@ q=@ r=[p=@ q=@] s=_*fu])]
  =>  |%
      ++  dap                                           ::  OEAP decode
        |=  [wid=@ xar=@ dog=@]  ^-  [p=@ q=@]
        =+  pav=(sub wid xar)
        =+  qoy=(cut 0 [xar pav] dog)
        =+  dez=(mix (end 0 xar dog) (shaw %pad-b xar qoy))
        [dez (mix qoy (shaw %pad-a pav dez))]
      ::
      ++  pad                                           ::  OEAP encode
        |=  [wid=@ rax=[p=@ q=@] meg=@]  ^-  @
        =+  pav=(sub wid p.rax)
        ?>  (gte pav (met 0 meg))
        ^-  @
        =+  qoy=(mix meg (shaw %pad-a pav q.rax))
        =+  dez=(mix q.rax (shaw %pad-b p.rax qoy))
        (can 0 [p.rax dez] [pav qoy] ~)
      ::
      ++  pull  |=(a=@ (~(exp fo mos) 3 a))
      ++  push  |=(a=@ (~(exp fo mos) 5 a))
      ++  pump
        |=  a=@  ^-  @
        ?~  pon  !!
        (out.s.u.pon (exp.s.u.pon p.r.u.pon (sit.s.u.pon a)))
      ::
      ++  punt
        |=  a=@  ^-  @
        ?~  pon  !!
        (out.s.u.pon (exp.s.u.pon q.r.u.pon (sit.s.u.pon a)))
      --
  |%
  ++  de
    |+  [key=@ cep=@]  ^-  (unit ,@)
    =+  toh=(met 8 cep)
    ?:  (lth toh 2)
      ~
    =+  adj=(dec toh)
    =+  [hax=(end 8 1 cep) bod=(rsh 8 1 cep)]
    =+  msg=(mix (~(raw og (mix hax key)) (mul 256 adj)) bod)
    ?.  =(hax (shax (mix key (shax (mix adj msg)))))
      ~
    [~ msg]
  ::
  ++  dy  |+([a=@ b=@] (need (de a b)))
  ++  en
    |+  [key=@ msg=@]  ^-  @ux
    =+  len=(met 8 msg)
    =+  adj=?:(=(0 len) 1 len)
    =+  hax=(shax (mix key (shax (mix adj msg))))
    (rap 8 hax (mix msg (~(raw og (mix hax key)) (mul 256 adj))) ~)
  ::
  ++  es  |+(a=@ (shas %anex a))
  ++  ex  ^?
    |%  ++  fig  ^-  @uvH  (shaf %afig mos)
        ++  pac  ^-  @uvG  (end 6 1 (shaf %acod sec))
        ++  pub  ^-  pass  (cat 3 'a' mos)
        ++  sec  ^-  ring  ?~(pon !! (cat 3 'A' (jam p.u.pon q.u.pon)))
    --
  ::
  ++  mx  (dec (met 0 mos))
  ++  nu
    =>  |%
        ++  elcm
          |=  [a=@ b=@]
          (div (mul a b) d:(egcd a b))
        ::
        ++  eldm
          |=  [a=@ b=@ c=@]
          (~(inv fo (elcm (dec b) (dec c))) a)
        ::
        ++  ersa
          |=  [a=@ b=@]
          [a b [(eldm 3 a b) (eldm 5 a b)] (fu a b)]
        --
    ^?
    |%  ++  com
          |=  a=@
          ^+  ^?(..nu)
          ..nu(mos a, pon ~)
        ::
        ++  pit
          |=  [a=@ b=@]
          =+  c=(rsh 0 1 a)
          =+  [d=(ramp c [3 5 ~] b) e=(ramp c [3 5 ~] +(b))]
          ^+  ^?(..nu)
          ..nu(mos (mul d e), pon [~ (ersa d e)])
        ::
        ++  nol
          |=  a=@
          ^+  ^?(..nu)
          =+  b=((hard ,[p=@ q=@]) (cue a))
          ..nu(mos (mul p.b q.b), pon [~ (ersa p.b q.b)])
    --
  ++  pu  ^?
    |%  ++  seal
          |=  [a=@ b=@]
          ^-  @
          =+  det=(lte (add 256 (met 0 b)) mx)
          =+  lip=?:(det b 0)
          =-  (add ?:(p.mav 0 1) (lsh 0 1 q.mav))
          ^=  mav  ^-  [p=? q=@]
          :-  det
          =+  dog=(pad mx [256 a] lip)
          =+  hog=(push dog)
          =+  ben=(en a b)
          ?:(det hog (jam hog ben))
        ::
        ++  sure
          |=  [a=@ b=@]
          ^-  (unit ,@)
          =+  [det==(0 (end 0 1 b)) bod=(rsh 0 1 b)]
          =+  gox=?:(det [p=bod q=0] ((hard ,[p=@ q=@]) (cue bod)))
          =+  dog=(pull p.gox)
          =+  pig=(dap mx 128 dog)
          =+  log=?:(det q.pig q.gox)
          ?.(=(p.pig (shaf (mix %agis a) log)) ~ [~ log])
    --
  ++  se  ^?
    |%  ++  sign
          |=  [a=@ b=@]  ^-  @
          =-  (add ?:(p.mav 0 1) (lsh 0 1 q.mav))
          ^=  mav  ^-  [p=? q=@]
          =+  det=(lte (add 128 (met 0 b)) mx)
          :-  det
          =+  hec=(shaf (mix %agis a) b)
          =+  dog=(pad mx [128 hec] ?:(det b 0))
          =+  hog=(pump dog)
          ?:(det hog (jam hog b))
        ::
        ++  tear
          |=  a=@
          ^-  (unit ,[p=@ q=@])
          =+  [det==(0 (end 0 1 a)) bod=(rsh 0 1 a)]
          =+  gox=?:(det [p=bod q=0] ((hard ,[p=@ q=@]) (cue bod)))
          =+  dog=(punt p.gox)
          =+  pig=(dap mx 256 dog)
          ?:  det
            [~ p.pig q.pig]
          =+  cow=(de p.pig q.gox)
          ?~(cow ~ [~ p.pig u.cow])
    --
  --
++  brew                                                ::  create keypair
  |=  [a=@ b=@]                                         ::  width seed
  ^-  acro
  (pit:nu:crya a b)
::
++  hail                                                ::  activate public key
  |=  a=pass
  ^-  acro
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('a' mag)
  (com:nu:crya bod)
::
++  wear                                                ::  activate secret key
  |=  a=ring
  ^-  acro
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('A' mag)
  (nol:nu:crya bod)
::
++  trsa                                                ::  test rsa
  |=  msg=@tas
  ^-  @
  =+  rsa=(brew 1.024 (shax msg))
  =+  key=(shax (shax (shax msg)))
  =+  sax=(seal:pu:rsa key msg)
  =+  tin=(tear:se:rsa sax)
  ?.  &(?=(^ tin) =(key p.u.tin) =(msg q.u.tin))
    ~|(%test-fail-seal !!)
  =+  tef=(sign:se:rsa [0 msg])
  =+  lov=(sure:pu:rsa [0 tef])
  ?.  &(?=(^ lov) =(msg u.lov))
    ~|(%test-fail-sign !!)
  msg
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bC, JSON and XML             ::
::
++  moon                                                ::  mime type to text
  |=  myn=mime
  %+  rap
    3
  |-  ^-  tape
  ?~  myn  ~
  ?~  t.myn  (trip i.myn)
  (weld (trip i.myn) ['/' $(myn t.myn)])
::
++  pojo                                                ::  print json
  |=  val=jval
  ^-  tape
  ?~  val  "null"
  ?-    -.val
      %a
    ;:  weld
      "["
      =|  rez=tape
      |-  ^+  rez
      ?~  p.val  rez
      $(p.val t.p.val, rez :(weld rez ^$(val i.p.val) ?~(t.p.val ~ ",")))
      "]"
    ==
 ::
      %b  ?:(p.val "true" "false")
      %n  (trip p.val)
      %s  :(weld "\"" (trip p.val) "\"")
      %o
    ;:  weld
      "\{"
      =+  viz=(~(tap by p.val) ~)
      =|  rez=tape
      |-  ^+  rez
      ?~  viz  rez
      %=    $
          viz  t.viz
          rez
        :(weld rez "\"" (trip p.i.viz) "\":" ^$(val q.i.viz) ?~(t.viz ~ ","))
      ==
      "}"
    ==
  ==
::
++  tact                                                ::  tape to octstream
  |=  tep=tape  ^-  octs
  =+  buf=(rap 3 tep)
  [(met 3 buf) buf]
::
++  txml                                                ::  string to xml
  |=  tep=tape  ^-  manx
  [[%$ [%$ tep] ~] ~]
::
++  xmla                                                ::  attributes to tape
  |=  [tat=mart rez=tape]
  ^-  tape
  ?~  tat  rez
  =+  ryq=$(tat t.tat)
  :(weld (xmln n.i.tat) "=\"" (xmle v.i.tat '"' ?~(t.tat ryq [' ' ryq])))
::
++  xmle                                                ::  escape for xml
  |=  [tex=tape rez=tape]
  =+  xet=`tape`(flop tex)
  |-  ^-  tape
  ?~  xet  rez
  %=    $
    xet  t.xet
    rez  ?-  i.xet
           34  ['&' 'q' 'u' 'o' 't' ';' rez]
           38  ['&' 'a' 'm' 'p' ';' rez]
           39  ['&' 'a' 'p' 'o' 's' ';' rez]
           60  ['&' 'l' 't' ';' rez]
           62  ['&' 'g' 't' ';' rez]
           *   [i.xet rez]
         ==
  ==
::
++  xmln                                                ::  name to tape
  |=  man=mane  ^-  tape
  ?@  man  (trip man)
  (weld (trip -.man) [':' (trip +.man)])
::
++  xmll                                                ::  nodes to tape
  |=  [lix=(list manx) rez=tape]
  =+  xil=(flop lix)
  |-  ^-  tape
  ?~  xil  rez
  $(xil t.xil, rez (xmlt i.xil rez))
::
++  xmlt                                                ::  node to tape
  |=  [mex=manx rez=tape]
  ^-  tape
  ?:  ?=([%$ [[%$ *] ~]] t.mex)
    (xmle v.i.a.t.mex rez)
  =+  man=`mane`?@(t.mex t.mex -.t.mex)
  =+  tam=(xmln man)
  =+  end=:(weld "</" tam ">" rez)
  =+  bod=['>' (xmll c.mex :(weld "</" tam ">" rez))]
  =+  att=`mart`?@(t.mex ~ a.t.mex)
  :-  '<'
  %+  weld  tam
  ?~(att bod [' ' (xmla att bod)])
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bD, tree sync                ::
::
++  cure                                                ::  invert miso
  |=  mis=miso
  ?-  -.mis
    %del  [%ins p.mis]
    %ins  [%del p.mis]
    %mut  [%mut (limp p.mis)]
  ==
::
++  curl                                                ::  invert soba
  |=  doz=soba
  :-  [q.p.doz p.p.doz]
  (turn (flop q.doz) |=([a=path b=miso] [a (cure b)]))
::
++  cosh                                                ::  locally rehash
  |=  ank=ankh
  ank(p dash:(zu ank))
::
++  cost                                                ::  new external patch
  |=  [bus=ankh ank=ankh]
  ^-  soba
  [[p.ank p.bus] (flop myz:(dist:(zu ank) %c bus))]
::
++  ze  !:
  |_  [lim=@da dome]
  ++  aeon                                              ::    aeon:ze
    |=  lok=case                                        ::  act count through
    ^-  (unit ,@ud)
    ?-    -.lok
        %da
      ?:  (gth p.lok lim)  ~
      |-  ^-  (unit ,@ud)
      ?~  hit  [~ let]
      ?:  (gte p.lok p.i.hit)  [~ let]
      $(hit t.hit, let (dec let))
    ::
        %tas  (~(get by lab) p.lok)
        %ud   ?:((gth p.lok let) ~ [~ p.lok])
    ==
  ::
  ++  ache                                              ::    ache:ze
    ^-  arch                                            ::  arch report
    :+  p.ank
      ?~(q.ank ~ [~ p.u.q.ank])
    |-  ^-  (map ,@ta ,~)
    ?~  r.ank  ~
    [[p.n.r.ank ~] $(r.ank l.r.ank) $(r.ank r.r.ank)]
  ::
  ++  acai                                              ::    acai:ze
    |=  mus=masu                                        ::  inverse trout
    [p.mus q.mus [q.r.mus p.r.mus] [q.s.mus p.s.mus]]
  ::
  ++  acid                                              ::    acid:ze
    |=  oan=@ud                                         ::  invert to
    ^-  (list tako)
    =+  mar=(scag (sub let oan) hit)
    (alar (adze (turn mar |=([a=@da b=nori] b))))
  ::
  ++  adze                                              ::    adze:ze
    |=  may=(list nori)                                 ::  reverse nori
    %-  flop
    |-  ^-  (list nori)
    ?~  may  ~
    =+  yam=$(may t.may)
    ?-  -.i.may
      &  [[%& (bind p.i.may acai) (curl q.i.may)] yam]
      |  yam
    ==
  ::
  ++  alar                                              ::    alar:ze
    |=  may=(list nori)                                 ::  nori to tako
    ^-  (list tako)
    ?~  may  ~
    ?-  -.i.may
      &  (weld q.q.i.may $(may t.may))
      |  $(may t.may)
    ==
  ::
  ++  alba                                              ::    alba:ze
    |=  hoe=(list tako)                                 ::  deconstruct
    |-  ^-  (map path (list udon))
    ?~  hoe  ~
    =+  hom=$(hoe t.hoe)
    %+  ~(put by hom)  p.i.hoe
    =+  vue=(~(get by hom) p.i.hoe)
    [?>(?=(%mut -.q.i.hoe) p.q.i.hoe) ?~(vue ~ u.vue)]
  ::
  ++  aloe                                              ::    aloe:ze
    |=  yop=(map path (list udon))                      ::  reconstruct
    ^-  (list tako)
    =+  puy=(~(tap by yop) ~)
    |-  ^-  (list tako)
    ?~  puy  ~
    (weld (turn q.i.puy |=(a=udon [p.i.puy %mut a])) $(puy t.puy))
  ::
  ++  alto                                              ::    alto:ze
    |=  $:  yop=(map path (list udon))                  ::  yop before peq
            peq=(map path (list udon))
        ==
    =+  puy=(~(tap by yop) ~)
    |-  ^+  peq
    ?~  puy  peq
    %=    $
        puy  t.puy
        peq
      =+  peb=(~(get by peq) p.i.puy)
      ?~  peb  peq
      (~(put by peq) p.i.puy (lyre q.i.puy u.peb))
    ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::
++  woof  (list $|(@ud [p=@ud q=@ud]))                  ::  udon transform
++  lisp                                                ::  transform urge
  |#  wig=(urge)
  |=  dog=woof
  ^+  [p=dog q=wig]
  ?~  wig  [~ ~]
  ?:  =([%& 0] i.wig)  $(wig t.wig)
  ?~  dog
    ?-  -.i.wig
      &  $(dog [p.i.wig ~])
      |  $(dog [(lent p.i.wig) ~])
    ==
  ?@  i.dog
    ?:  =(0 i.dog)  $(dog t.dog)
    ?-    -.i.wig
        &
      ?:  (lth i.dog p.i.wig)
        =+  mol=$(dog t.dog, p.i.wig (sub p.i.wig i.dog))
        [[i.dog p.mol] [[%& i.dog] q.mol]]
      =+  mol=$(i.dog (sub i.dog p.i.wig), wig t.wig)
      [[p.i.wig p.mol] [[%& p.i.wig] q.mol]]
    ::
        |
      =+  leg=(lent p.i.wig)
      ?>  (gte i.dog leg)
      =+  mol=$(i.dog (sub i.dog leg), wig t.wig)
      :-  `woof`[`[@ @]`[leg (lent q.i.wig)] `woof`p.mol]
      ^+(wig [i.wig q.mol])
    ==
  ?>  ?=(& -.i.wig)
  ?>  (gte p.i.wig -.i.dog)
  =+  mol=$(dog t.dog, p.i.wig (sub p.i.wig -.i.dog))
  [[i.dog p.mol] [[%& +.i.dog] q.mol]]
::
++  lith                                                ::  initial transform
  |=  wig=(urge)
  ^-  woof
  =+  wug=((lisp wig) *woof)
  ?>(=(wig q.wug) p.wug)
::
++  lobe                                                ::  udonous urge
  |=  [ump=umph heb=(list (urge))]
  ^-  (list udon)
  (turn heb |=(a=(urge) `udon`[ump %c a]))
::
++  lobo                                                ::  urgey udon
  |=  [ump=umph yeb=(list udon)]
  ^-  (list (urge))
  (turn yeb |=(a=udon ?>(&(=(ump p.a) ?=(%c -.q.a)) p.q.a)))
::
++  loch                                                ::  put wof before weq
  |=  [wof=woof weq=(list (urge))]
  ^-  (list (urge))
  |-  ^+  weq
  ?~  weq  ~
  =+  wug=((lisp i.weq) wof)
  [q.wug $(weq t.weq, wof p.wug)]
::
++  loup                                                ::  put heb before weq
  |=  [heb=(list (urge)) weq=(list (urge))]
  ^-  (list (urge))
  ?~  heb  weq
  ?~  weq  heb
  ?:  =(i.heb i.weq)  $(heb t.heb, weq t.weq)
  $(heb t.heb, weq (loch (lith i.heb) weq))
::
++  lyre                                                ::  put yeb before zeq
  |=  [yeb=(list udon) zeq=(list udon)]
  ^-  (list udon)
  ?~  yeb  zeq
  ?~  zeq  yeb
  ?:  =(i.yeb i.zeq)  $(yeb t.yeb, zeq t.zeq)
  =+  ump=p.i.yeb
  (lobe ump (loup (lobo ump yeb) (lobo ump zeq)))
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ++  amor                                              ::    amor:ze
    |=  ren=?(%v %x %y %z)                              ::  endpoint query
    ^-  (unit ,*)
    ?-  ren
      %v  [~ `dome`+<+.amor]
      %x  ?~(q.ank ~ [~ q.u.q.ank])
      %y  [~ ache]
      %z  [~ ank]
    ==
  ::
  ++  ante                                              ::    ante:ze
    |=  lem=nori                                        ::  rewind by change
    ^+  +>
    ?-  -.lem
      &  (avon:(anti q.lem) (bind p.lem acai))
      |  +>(lab (~(del by lab) p.lem))
    ==
  ::
  ++  anti                                              ::    axon:ze
    |=  nyp=soba                                        ::  reverse axon
    ^+  +>
    +>(ank ank:(dusk:(zu ank) nyp))
  ::
  ++  argo                                              ::    argo:ze
    |=  oan=@ud                                         ::  rewind to aeon
    ^+  +>
    ?:  =(let oan)  +>
    =+  lem=?>(?=(^ hit) q.i.hit)
    =:  let  (dec let)
        hit  t.hit
      ==
    $(+> (ante lem))
  ::
  ++  arum                                              ::    arum:ze
    |=  [tak=(list tako) kat=(list tako)]               ::  hard merge
    (aloe (alto (alba tak) (alba kat)))
  ::
  ++  auld                                              ::    auld:ze
    |=  [wen=@da gem=germ sab=saba]                     ::  construct merge
    ^-  (unit (unit nori))                          ::::::
    =+  ^=  viq  ^-  [p=@ud q=@ud]                  ::
        =+  viq=(~(get by ang) [p.sab q.sab])       ::
        ?~(viq [0 0] u.viq)                         ::
    =.  sab                                         ::
      ?:  =(q.viq p.r.sab)  sab                     ::  perfect motion
      ?>  (gth q.viq p.r.sab)                       ::  proper motion
      %=  sab                                       ::
        p.r  q.viq                                  ::
        s    (slag (sub q.viq p.r.sab) s.sab)       ::
      ==                                            ::
    ?~  s.sab  [~ ~]
    =+  ^=   mus  ^-  masu
        [p.sab q.sab [p.viq +(let)] [q.viq q.r.sab]]
    ::  ~&  [%auld p.mus q.mus [%too r.mus] [%fro s.mus]]
    =+  kat=(alar s.sab)
    =+  lem=`nori`[%& [~ mus] `soba`[_[@ @] kat]]
    ?:  =(let p.viq)
      ::  ~&  [%nice let]
      [~ ~ lem]                                     ::  perfect fit
    =+  paj=(axel:(argo p.viq) wen lem)             ::   XX !!
    ?:  =(ank.paj ank)                              ::
      ::  ~&  [%same [let p.viq]]
      [~ ~ lem(q.q ~)]
    =+  ^=  tak
        %-  alar  %-  flop
        %+  turn  (scag (sub let p.viq) hit)
        |=(a=frog q.a)
    =+  tig=(mang [arum tak kat] |=(* *(unit)))
    ?^  tig  [~ ~ lem(q.q ((hard (list tako)) u.tig))]
    ?-  gem
      %fine  ~                                    ::  nothing perfect
      %mate  ~|(%mate-stub !!)                    ::  not supported
      %that  [~ ~ lem(q.q (weld (acid p.viq) q.q.lem))]
      %this  [~ ~ lem(q.q ~)]
    ==
  ::
  ++  auto                                              ::    auto:ze
    |=  mun=mood                                        ::  read at point
    ^-  (unit)
    ?:  ?=(%v p.mun)
      [~ `dome`+<+.auto]
    ?:  &(?=(%w p.mun) !?=(%ud -.q.mun))
      ?^(r.mun ~ [~ let])
    ?:  ?=(%w p.mun)
      ?>  ?=(^ hit)  ?^(r.mun ~ [~ i.hit])
    (amor(ank ank:(deny:(zu ank) r.mun)) p.mun)
  ::
  ++  aver                                              ::    aver:ze
    |=  mun=mood                                        ::  direct read
    ^-  (unit (unit ,*))
    =+  nao=(aeon q.mun)
    ?~(nao ~ [~ (avid u.nao mun)])
  ::
  ++  avid                                              ::    avid:ze
    |=  [oan=@ud mun=mood]                              ::  seek and read
    ^-  (unit)
    ?:  &(?=(%w p.mun) !?=(%ud -.q.mun))                ::  NB only for speed
      ?^(r.mun ~ [~ oan])
    (auto:(argo oan) mun)
  ::
  ++  avon                                              ::    avon:ze
    |=  mys=(unit masu)                                 ::  apply trout
    ^+  +>
    ?~  mys  +>
    =+  [yek=[p.u.mys q.u.mys] det=[q.r.u.mys q.s.u.mys]]
    =+  guf=(~(get by ang) yek)
    =+  ted=`moar`?~(guf [0 0] u.guf)
    ::  ~&  [%avon p.u.mys q.u.mys [%haz ted] [%too r.u.mys] [%fro s.u.mys]]
    ?>  &(=(p.ted p.r.u.mys) =(q.ted p.s.u.mys))
    +>.$(ang ?:(=([0 0] det) (~(del by ang) yek) (~(put by ang) yek det)))
  ::
  ++  axel                                              ::    axel:ze
    |=  [wen=@da lem=nori]                              ::  edit
    ^+  +>
    =:  let  +(let)
        hit  :_(hit [wen lem])
      ==
    ?-  -.lem
      &  (avon:(axon q.lem) p.lem)
      |  +>(lab ?<((~(has by lab) p.lem) (~(put by lab) p.lem let)))
    ==
  ::
  ++  axon                                              ::    axon:ze
    |=  nyp=soba                                        ::  apply changes
    ^+  +>
    +>(ank ank:(durn:(zu ank) nyp))
  --
::
++  zu  !:                                              ::  filesystem
  |=  ank=ankh                                          ::  filesystem state
  =|  myz=(list ,[p=path q=miso])                       ::  changes in reverse
  =|  ram=path                                          ::  reverse path into
  |%
  ++  dash                                              ::  local rehash
    ^-  cash
    %+  mix  ?~(q.ank 0 p.u.q.ank)
    =+  axe=1
    |-  ^-  cash
    ?~  r.ank  _@
    ;:  mix
      (shaf %dash (mix axe (shaf %dush (mix p.n.r.ank p.q.n.r.ank))))
      $(r.ank l.r.ank, axe (peg axe 2))
      $(r.ank r.r.ank, axe (peg axe 3))
    ==
  ::
  ++  dosh  %_(. p.ank dash)                            ::  rehash and save
  ++  dose                                              ::  ascend
    |=  [lol=@ta kan=ankh]
    ^+  +>
    ?>  &(?=(^ ram) =(lol i.ram))
    %=    +>
        ram  t.ram
        ank
      ?:  =([0 ~ ~] ank)
        ?.  (~(has by r.kan) lol)  kan
        kan(r (~(del by r.kan) lol))
      kan(r (~(put by r.kan) lol ank))
    ==
  ::
  ++  deaf                                              ::  add change
    |=  mis=miso
    ^+  +>
    +>(myz [[(flop ram) mis] myz])
  ::
  ++  dent                                              ::  descend
    |=  lol=@ta
    ^+  +>
    =+  you=(~(get by r.ank) lol)
    +>.$(ram [lol ram], ank ?~(you [*cash ~ ~] u.you))
  ::
  ++  deny                                              ::  descend recursively
    |=  way=path
    ^+  +>
    ?~(way +> $(way t.way, +> (dent i.way)))
  ::
  ++  dest                                              ::  write over
    |=  [pum=umph val=(unit ,[p=cash q=*])]
    ^+  +>
    ?~  q.ank
      ?~  val  +>
      (deaf %ins q.u.val)
    ?~  val
      (deaf %del q.u.q.ank)
    ?:  =(q.u.val q.u.q.ank)  +>
    (deaf %mut ((diff pum) q.u.q.ank q.u.val))
  ::
  ++  dist                                              ::  modify tree
    |=  [pum=umph bus=ankh]
    ^+  +>
    =.  +>  (dest pum q.bus)
    =+  [yeg=(~(tap by r.ank) ~) gey=(~(tap by r.bus) ~)]
    =.  +>.$
      |-  ^+  +>.^$
      ?~  yeg  +>.^$
      ?:  (~(has by r.bus) p.i.yeg)  $(yeg t.yeg)
      $(yeg t.yeg, myz myz:dirk(ank q.i.yeg, ram [p.i.yeg ram]))
    |-  ^+  +>.^$
    ?~  gey  +>.^$
    $(gey t.gey, myz myz:^$(bus q.i.gey, +> (dent p.i.gey)))
  ::
  ++  dirk                                              ::  rm -r
    |-  ^+  +
    =.  +  ?~(q.ank + (deaf %del q.u.q.ank))
    =+  dyr=(~(tap by r.ank) ~)
    |-  ^+  +.^$
    ?~  dyr  +.^$
    =.  +.^$  dirk:(dent p.i.dyr)
    $(dyr t.dyr)
  ::
  ++  drum                                              ::  apply effect
    |=  [pax=path mis=miso]
    ^+  +>
    ?^  pax
      dosh:(dose:$(pax t.pax, +> (dent i.pax)) i.pax ank)
    ?-    -.mis
        %del
      ?>  &(?=(^ q.ank) =(q.u.q.ank p.mis))
      +>.$(p.ank (mix p.u.q.ank p.ank), q.ank ~)
    ::
        %ins
      ?>  ?=(~ q.ank)
      =+  sam=(sham p.mis)
      +>.$(p.ank (mix sam p.ank), q.ank [~ sam p.mis])
    ::
        %mut
      ?>  ?=(^ q.ank)
      =+  nex=(lump p.mis q.u.q.ank)
      =+  sam=(sham nex)
      +>.$(p.ank :(mix sam p.u.q.ank p.ank), q.ank [~ sam nex])
    ==
  ::
  ++  dune                                              ::  apply
    |-  ^+  +
    ?~  myz  +
    =>  .(+ (drum p.i.myz q.i.myz))
    $(myz t.myz)
  ::
  ++  durn                                              ::  apply forward
    |=  nyp=soba
    ^+  +>
    ?:  =([0 0] p.nyp)
      dune(myz q.nyp)
    =>  ?:  =(p.ank p.p.nyp)  .
        ~&  [%durn-in-wrong p.ank p.p.nyp]
        .
    ::  ?>  =(p.ank p.p.nyp)
    =.  +>  dune(myz q.nyp)
    =>  ?:  =(p.ank q.p.nyp)  .
        ~&  [%durn-out-wrong p.ank q.p.nyp]
        .
    +>
  ::
  ++  dusk                                              ::  apply reverse
    |=  nyp=soba
    (durn (curl nyp))
--
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bE, names etc                ::
::
++  clan                                                ::  ship to rank
  |=  who=ship  ^-  rank
  =+  wid=(met 3 who)
  ?:  (lte wid 1)   %czar
  ?:  =(2 wid)      %king
  ?:  (lte wid 4)   %duke
  ?:  (lte wid 8)   %earl
  ?>  (lte wid 16)  %pawn
::
++  deft                                                ::  import url path
  |=  rax=(list ,@t)
  |-  ^-  pork
  ?~  rax
    [~ ~]
  ?~  t.rax
    =+  den=(trip i.rax)
    =+  ^=  vex
      %-  %-  full
          ;~(plug sym ;~(pose (stag ~ ;~(pfix dot sym)) (easy ~)))
      [[1 1] (trip i.rax)]
    ?~  q.vex
      [~ [~(rent co %$ %t i.rax) ~]]
    [+.p.u.q.vex [-.p.u.q.vex ~]]
  =+  pok=$(rax t.rax)
  :-  p.pok
  :_  q.pok
  ?:(((sane %tas) i.rax) i.rax ~(rent co %$ %t i.rax))
::
++  epur                                                ::  url/header parser
  |%
  ++  apat  (cook deft ;~(pfix fas (more fas smeg)))    ::  2396 abs_path
  ++  auri
    ;~  plug
      ;~  plug
        %+  sear
          |=  a=@t
          ^-  (unit ,?)
          ?+(a ~ %http [~ %|], %https [~ %&])
        ;~(sfix scem ;~(plug col fas fas))
        thor
      ==
      ;~(plug apat yque)
    ==
  ++  bite                                              ::  cookies (ours)
    (most sem ;~(plug nuck:so ;~(pfix sem nuck:so)))
  ++  dlab                                              ::  2396 domainlabel
    %+  sear
      |=  a=@ta
      ?.(=('-' (rsh 3 a (dec (met 3 a)))) [~ u=a] ~)
    %+  cook  cass
    ;~(plug aln (star alp))
  ::
  ++  fque  (cook crip (plus pquo))                     ::  normal query field
  ++  pcar  ;~(pose pure pesc psub col pat)             ::  2396 path char
  ++  pesc  ;~(pfix cen mes)                            ::  2396 escaped
  ++  pold  (cold ' ' (just '+'))                       ::  old space code
  ++  pque  ;~(pose pcar fas wut)                       ::  3986 query char
  ++  pquo  ;~(pose pure pesc pold)                     ::  normal query char
  ++  pure  ;~(pose aln hep dot cab sig)                ::  2396 unreserved
  ++  psub  ;~  pose                                    ::  3986 sub-delims
              zap  buc  pam  soq  pel  per
              tar  lus  com  sem  tis
            ==
  ++  scem                                              ::  2396 scheme
    %+  cook  cass
    ;~(plug alf (star ;~(pose aln lus hep dot)))
  ::
  ++  smeg  (cook crip (plus pcar))                     ::  2396 segment
  ++  thor                                              ::  2396 host/port
    %+  cook  |#(a=[* *] [+.a -.a])
    ;~  plug
      thos
      ;~(pose (stag ~ ;~(pfix col dim:ag)) (easy ~))
    ==
  ++  thos                                              ::  2396 host, no local
    ;~  plug
      ;~  pose
        %+  stag  %&
        %+  sear                                        ::  LL parser weak here
          |=  a=(list ,@t)
          =+  b=(flop a)
          ?>  ?=(^ b)
          =+  c=(end 3 1 i.b)
          ?.(&((gte c 'a') (lte c 'z')) ~ [~ u=b])
        (most dot dlab)
      ::
        %+  stag  %|
        =+  tod=(ape:ag ted:ab)
        %+  bass  256
        ;~(plug tod (stun [3 3] ;~(pfix dot tod)))
      ==
    ==
  ++  yque                                              ::  query ending
    ;~  pose
      ;~(pfix wut yquy)
      (easy ~)
    ==
  ++  yquy                                              ::  query
    %+  cook
      |=  a=(list ,[p=@t q=@t])
      (~(gas by *(map ,@t ,@t)) a)
    ;~  pose                                            ::  proper query
      %+  more
        ;~(pose pam sem)
      ;~(plug fque ;~(pfix tis fque))
    ::
      %+  cook                                          ::  funky query
        |=(a=tape [[%$ (crip a)] ~])
      (star pque)
    ==
  ++  zest                                              ::  2616 request-uri
    ;~  pose
      (stag %& (cook |=(a=purl a) auri))
      (stag %| ;~(plug apat yque))
    ==
  --
::
++  glam
  |=  zar=@p  ^-  tape
  %+  snag  zar
  ^-  (list tape)
  :~  "Tianming"  "Pepin the Short"  "Haile Selassie"  "Alfred the Great"
      "Tamerlane"  "Pericles"  "Talleyrand"  "Yongle"  "Seleucus"
      "Uther Pendragon"  "Louis XVI"  "Ahmad Shāh Durrānī"  "Constantine"
      "Wilhelm I"  "Akbar"  "Louis XIV"  "Nobunaga"  "Alexander VI"
      "Philippe II"  "Julius II"  "David"  "Niall Noígíallach"  "Kublai Khan"
      "Öz Beg Khan"  "Ozymandias"  "Ögedei Khan"  "Jiang Jieshi"  "Darius"
      "Shivaji"  "Qianlong"  "Bolesław I Chrobry"  "Tigranes"  "Han Wudi"
      "Charles X"  "Naresuan"  "Frederick II"  "Simeon"  "Kangxi"
      "Suleiman the Magnificent"  "Pedro II"  "Genghis Khan"  "Đinh Bộ Lĩnh"
      "Porfirio Díaz"  "Pakal"  "Wu Zetian"  "Garibaldi"  "Pius IX"
      "Leopold II"  "Leonidas"  "Sitting Bull"  "Nebuchadnezzar II"
      "Rhodes"  "Henry VIII"  "Attila"  "Catherine II"  "Chulalongkorn"
      "Uthmān"  "Augustus"  "Faustin"  "Chongde"  "Justinian"
      "Afonso de Albuquerque"  "Antoninus Pius"  "Cromwell"  "Innocent X"
      "Fidel"  "Frederick the Great"  "Canute"  "Vytautas"  "Amina"
      "Hammurabi"  "Suharto"  "Victoria"  "Hiawatha"  "Paul V"  "Shaka"
      "Lê Thánh Tông"  "Ivan Asen II"  "Tiridates"  "Nefertiti"  "Gwangmu"
      "Ferdinand & Isabella"  "Askia"  "Xuande"  "Boris Godunov"  "Gilgamesh"
      "Maximillian I"  "Mao"  "Charlemagne"  "Narai"  "Hanno"  "Charles I & V"
      "Alexander II"  "Mansa Musa"  "Zoe Porphyrogenita"  "Metternich"
      "Robert the Bruce"  "Pachacutec"  "Jefferson"  "Solomon"  "Nicholas I"
      "Barbarossa"  "FDR"  "Pius X"  "Gwanggaeto"  "Abbas I"  "Julius Caesar"
      "Lee Kuan Yew"  "Ranavalona I"  "Go-Daigo"  "Zenobia"  "Henry V"
      "Bảo Đại"  "Casimir III"  "Cyrus"  "Charles the Wise"  "Sandrokottos"
      "Agamemnon"  "Clement VII"  "Suppiluliuma"  "Deng Xiaoping"
      "Victor Emmanuel"  "Ajatasatru"  "Jan Sobieski"  "Huangdi"  "Xuantong"
      "Narmer"  "Cosimo de' Medici"  "Möngke Khan"  "Stephen Dušan"  "Henri IV"
      "Mehmed Fatih"  "Conn Cétchathach"  "Francisco Franco"  "Leo X"
      "Kammu"  "Krishnadevaraya"  "Elizabeth I"  "Norton I"  "Washington"
      "Meiji"  "Umar"  "TR"  "Peter the Great"  "Agustin I"  "Ashoka"
      "William the Conqueror"  "Kongolo Mwamba"  "Song Taizu"
      "Ivan the Terrible"  "Yao"  "Vercingetorix"  "Geronimo"  "Lincoln"
      "Urban VIII"  "Alexios Komnenos"  "Maria I"  "Tamar"  "Bismarck"
      "Arthur"  "Jimmu"  "Gustavus Adolphus"  "Suiko"  "Basil I"  "Montezuma"
      "Santa Anna"  "Xerxes"  "Beyazıt Yıldırım"  "Samudragupta"  "James I"
      "George III"  "Kamehameha"  "Francesco Sforza"  "Trajan"
      "Rajendra Chola"  "Hideyoshi"  "Cleopatra"  "Alexander"
      "Ashurbanipal"  "Paul III"  "Vespasian"  "Tecumseh"  "Narasimhavarman"
      "Suryavarman II"  "Bokassa I"  "Charles Canning"  "Theodosius"
      "Francis II"  "Zhou Wen"  "William Jardine"  "Ahmad al-Mansur"
      "Lajos Nagy"  "Theodora"  "Mussolini"  "Samuil"  "Osman Gazi"
      "Kim Il-sung"  "Maria Theresa"  "Lenin"  "Tokugawa"  "Marcus Aurelius"
      "Nzinga Mbande"  "Edward III"  "Joseph II"  "Pulakesi II"  "Priam"
      "Qin Shi Huang"  "Shah Jahan"  "Sejong"  "Sui Wendi"  "Otto I"
      "Napoleon III"  "Prester John"  "Dido"  "Joao I"  "Gregory I"
      "Gajah Mada"  "Abd-ar Rahmān III"  "Taizong"  "Franz Josef I"
      "Nicholas II"  "Gandhi"  "Chandragupta II"  "Peter III"
      "Oba Ewuare"  "Louis IX"  "Napoleon"  "Selim Yavuz"  "Shun"
      "Hayam Wuruk"  "Jagiełło"  "Nicaule"  "Sargon"  "Saladin"  "Charles II"
      "Brian Boru"  "Da Yu"  "Antiochus III"  "Charles I"
      "Jan Pieterszoon Coen"  "Hongwu"  "Mithridates"  "Hadrian"  "Ptolemy"
      "Benito Juarez"  "Sun Yat-sen"  "Raja Raja Chola"  "Bolivar"  "Pius VII"
      "Shapur II"  "Taksin"  "Ram Khamhaeng"  "Hatshepsut"  "Alī"  "Matilda"
      "Ataturk"
  ==
::
++  glon
  |=  lag=lang
  ^-  (unit tape)
  ?+  lag  ~
    %aa  [~ "Afar"]
    %ab  [~ "Abkhazian"]
    %ae  [~ "Avestan"]
    %af  [~ "Afrikaans"]
    %ak  [~ "Akan"]
    %am  [~ "Amharic"]
    %an  [~ "Aragonese"]
    %ar  [~ "Arabic"]
    %as  [~ "Assamese"]
    %av  [~ "Avaric"]
    %ay  [~ "Aymara"]
    %az  [~ "Azerbaijani"]
    %ba  [~ "Bashkir"]
    %be  [~ "Belarusian"]
    %bg  [~ "Bulgarian"]
    %bh  [~ "Bihari"]
    %bi  [~ "Bislama"]
    %bm  [~ "Bambara"]
    %bn  [~ "Bengali"]
    %bo  [~ "Tibetan"]
    %br  [~ "Breton"]
    %bs  [~ "Bosnian"]
    %ca  [~ "Catalan"]
    %ce  [~ "Chechen"]
    %ch  [~ "Chamorro"]
    %co  [~ "Corsican"]
    %cr  [~ "Cree"]
    %cs  [~ "Czech"]
    %cu  [~ "Slavonic"]
    %cv  [~ "Chuvash"]
    %cy  [~ "Welsh"]
    %da  [~ "Danish"]
    %de  [~ "German"]
    %dv  [~ "Maldivian"]
    %dz  [~ "Dzongkha"]
    %ee  [~ "Ewe"]
    %el  [~ "Greek"]
    %en  [~ "English"]
    %eo  [~ "Esperanto"]
    %es  [~ "Spanish"]
    %et  [~ "Estonian"]
    %eu  [~ "Basque"]
    %fa  [~ "Persian"]
    %ff  [~ "Fulah"]
    %fi  [~ "Finnish"]
    %fj  [~ "Fijian"]
    %fo  [~ "Faroese"]
    %fr  [~ "French"]
    %fy  [~ "Frisian"]
    %ga  [~ "Irish Gaelic"]
    %gd  [~ "Scottish Gaelic"]
    %gl  [~ "Galician"]
    %gn  [~ "Guarani"]
    %gu  [~ "Gujarati"]
    %gv  [~ "Manx"]
    %ha  [~ "Hausa"]
    %he  [~ "Hebrew"]
    %hi  [~ "Hindi"]
    %ho  [~ "Hiri Motu"]
    %hr  [~ "Croatian"]
    %ht  [~ "Haitian Creole"]
    %hu  [~ "Hungarian"]
    %hy  [~ "Armenian"]
    %hz  [~ "Herero"]
    %ia  [~ "Interlingua"]
    %id  [~ "Indonesian"]
    %ie  [~ "Occidental"]
    %ig  [~ "Igbo"]
    %ii  [~ "Nuosu"]
    %ik  [~ "Inupiaq"]
    %io  [~ "Ido"]
    %is  [~ "Icelandic"]
    %it  [~ "Italian"]
    %iu  [~ "Inuktitut"]
    %ja  [~ "Japanese"]
    %jv  [~ "Javanese"]
    %ka  [~ "Georgian"]
    %kg  [~ "Kongo"]
    %ki  [~ "Kikuyu"]
    %kj  [~ "Kwanyama"]
    %kk  [~ "Kazakh"]
    %kl  [~ "Kalaallisut"]
    %km  [~ "Central Khmer"]
    %kn  [~ "Kannada"]
    %ko  [~ "Korean"]
    %kr  [~ "Kanuri"]
    %ks  [~ "Kashmiri"]
    %ku  [~ "Kurdish"]
    %kv  [~ "Komi"]
    %kw  [~ "Cornish"]
    %ky  [~ "Kyrgyz"]
    %la  [~ "Latin"]
    %lb  [~ "Luxembourgish"]
    %lg  [~ "Ganda"]
    %li  [~ "Limburgish"]
    %ln  [~ "Lingala"]
    %lo  [~ "Lao"]
    %lt  [~ "Lithuanian"]
    %lu  [~ "Luba-Katanga"]
    %lv  [~ "Latvian"]
    %mg  [~ "Malagasy"]
    %mh  [~ "Marshallese"]
    %mi  [~ "Maori"]
    %mk  [~ "Macedonian"]
    %ml  [~ "Malayalam"]
    %mn  [~ "Mongolian"]
    %mr  [~ "Marathi"]
    %ms  [~ "Malay"]
    %mt  [~ "Maltese"]
    %my  [~ "Burmese"]
    %na  [~ "Nauru"]
    %nb  [~ "Norwegian Bokmål"]
    %nd  [~ "North Ndebele"]
    %ne  [~ "Nepali"]
    %ng  [~ "Ndonga"]
    %nl  [~ "Dutch"]
    %nn  [~ "Norwegian Nynorsk"]
    %no  [~ "Norwegian"]
    %nr  [~ "South Ndebele"]
    %nv  [~ "Navajo"]
    %ny  [~ "Chichewa"]
    %oc  [~ "Occitan"]
    %oj  [~ "Ojibwa"]
    %om  [~ "Oromo"]
    %or  [~ "Oriya"]
    %os  [~ "Ossetian"]
    %pa  [~ "Punjabi"]
    %pi  [~ "Pali"]
    %pl  [~ "Polish"]
    %ps  [~ "Pashto"]
    %pt  [~ "Portuguese"]
    %qu  [~ "Quechua"]
    %rm  [~ "Romansh"]
    %rn  [~ "Rundi"]
    %ro  [~ "Romanian"]
    %ru  [~ "Russian"]
    %rw  [~ "Kinyarwanda"]
    %sa  [~ "Sanskrit"]
    %sc  [~ "Sardinian"]
    %sd  [~ "Sindhi"]
    %se  [~ "Northern Sami"]
    %sg  [~ "Sango"]
    %si  [~ "Sinhala"]
    %sk  [~ "Slovak"]
    %sl  [~ "Slovenian"]
    %sm  [~ "Samoan"]
    %sn  [~ "Shona"]
    %so  [~ "Somali"]
    %sq  [~ "Albanian"]
    %sr  [~ "Serbian"]
    %ss  [~ "Swati"]
    %st  [~ "Sotho"]
    %su  [~ "Sundanese"]
    %sv  [~ "Swedish"]
    %sw  [~ "Swahili"]
    %ta  [~ "Tamil"]
    %te  [~ "Telugu"]
    %tg  [~ "Tajik"]
    %th  [~ "Thai"]
    %ti  [~ "Tigrinya"]
    %tk  [~ "Turkmen"]
    %tl  [~ "Tagalog"]
    %tn  [~ "Tswana"]
    %to  [~ "Tonga"]
    %tr  [~ "Turkish"]
    %ts  [~ "Tsonga"]
    %tt  [~ "Tatar"]
    %tw  [~ "Twi"]
    %ty  [~ "Tahitian"]
    %ug  [~ "Uighur"]
    %uk  [~ "Ukrainian"]
    %ur  [~ "Urdu"]
    %uz  [~ "Uzbek"]
    %ve  [~ "Venda"]
    %vi  [~ "Vietnamese"]
    %vo  [~ "Volapük"]
    %wa  [~ "Walloon"]
    %wo  [~ "Wolof"]
    %xh  [~ "Xhosa"]
    %yi  [~ "Yiddish"]
    %yo  [~ "Yoruba"]
    %za  [~ "Zhuang"]
    %zh  [~ "Chinese"]
    %zu  [~ "Zulu"]
  ==
::
++  gnow
  |=  [who=@p gos=gcos]  ^-  @t
  ?-    -.gos
      %czar                 (rap 3 '|' (rap 3 (glam who)) '|' ~)
      %king                 (rap 3 '_' p.gos '_' ~)
      %earl                 (rap 3 ':' p.gos ':' ~)
      %pawn                 ?~(p.gos %$ (rap 3 '.' u.p.gos '.' ~))
      %duke
    ?:  ?=(%anon -.p.gos)  %$
    %+  rap  3
    ^-  (list ,@)
    ?-    -.p.gos
        %punk  ~['"' q.p.gos '"']
        ?(%lord %lady)
      =+  ^=  nad
          =+  nam=`name`s.p.p.gos
          %+  rap  3
          :~  p.nam
              ?~(q.nam 0 (cat 3 ' ' u.q.nam))
              ?~(r.nam 0 (rap 3 ' (' u.r.nam ')' ~))
              ' '
              s.nam
          ==
      ?:(=(%lord -.p.gos) ~['[' nad ']'] ~['(' nad ')'])
    ==
  ==
::
++  hunt
  |=  [one=(unit ,@da) two=(unit ,@da)]
  ^-  (unit ,@da)
  ?~  one  two
  ?~  two  one
  ?:((lth u.one u.two) one two)
::
++  meat
  |=  kit=kite
  ^-  path
  [(cat 3 'c' p.kit) (scot %p r.kit) s.kit (scot (dime q.kit)) t.kit]
::
++  numb
  |=  [him=@p now=@da]  ^-  @t
  =+  yow=(scot %p him)
  =+  woy=((hard ,@t) .^(%a yow %name (scot %da now) ~))
  ?:  =(%$ woy)  yow
  (cat 3 yow (cat 3 ' ' woy))
::
++  saxo                                                ::  autocanon
  |=  who=ship
  ^-  (list ship)
  ?:  (lth who 256)  [who ~]
  [who $(who (sein who))]
::
++  sein                                                ::  autodean
  |=  who=ship  ^-  ship
  =+  mir=(clan who)
  ?-  mir
    %czar  who
    %king  (end 3 1 who)
    %duke  (end 4 1 who)
    %earl  (end 5 1 who)
    %pawn  `@p`0
  ==
::
++  tame
  |=  hap=path
  ^-  (unit kite)
  ?.  ?=([@ @ @ *] hap)  ~
  =+  :*  hyr=(slay i.hap)
          fal=(slay i.t.hap)
          dyc=(slay i.t.t.hap)
          ved=(slay i.t.t.t.hap)
          ::  ved=(slay i.t.hap)
          ::  fal=(slay i.t.t.hap)
          ::  dyc=(slay i.t.t.t.hap)
          tyl=t.t.t.t.hap
      ==
  ?.  ?=([~ %$ %tas @] hyr)  ~
  ?.  ?=([~ %$ %p @] fal)  ~
  ?.  ?=([~ %$ %tas @] dyc)  ~
  ?.  ?=(^ ved)  ~
  =+  his=`@p`q.p.u.fal
  =+  [dis=(end 3 1 q.p.u.hyr) rem=(rsh 3 1 q.p.u.hyr)]
  ?.  ?&(?=(%c dis) ?=(?(%v %w %x %y %z) rem))  ~
  [~ rem (case p.u.ved) q.p.u.fal q.p.u.dyc tyl]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bF, Arvo models              ::
::
++  acro                                                ::  asym cryptosuite
          $_  ^?  |%                                    ::  opaque object
          ++  de  |+([a=@ b=@] *(unit ,@))              ::  symmetric de, soft
          ++  dy  |+([a=@ b=@] _@)                      ::  symmetric de, hard
          ++  en  |+([a=@ b=@] _@)                      ::  symmetric en
          ++  es  |+(a=@ _@)                            ::  step key to next
          ++  ex  ^?                                    ::  export
            |%  ++  fig  _@uvH                          ::  fingerprint
                ++  pac  _@uvG                          ::  default passcode
                ++  pub  *pass                          ::  public key
                ++  sec  *ring                          ::  private key
            --                                          ::
          ++  mx  _@                                    ::  max direct bytes
          ++  nu  ^?                                    ::  reconstructors
            |%  ++  pit  |=([a=@ b=@] ^?(..nu))         ::  from [width seed]
                ++  nol  |=(a=@ ^?(..nu))               ::  from naked ring
                ++  com  |=(a=@ ^?(..nu))               ::  from naked pass
            --                                          ::
          ++  pu  ^?                                    ::  public-key acts
            |%  ++  seal  |=([a=@ b=@] _@)              ::  encrypt
                ++  sure  |=([a=@ b=@] *(unit ,@))      ::  authenticate
            --                                          ::
          ++  se  ^?                                    ::  secret-key acts
            |%  ++  sign  |=([a=@ b=@] _@)              ::  certify
                ++  tear  |=(a=@ *(unit ,[p=@ q=@]))    ::  accept
            --                                          ::
          --                                            ::
++  agon  (map ,[p=ship q=disc] ,[p=@ud q=@ud])         ::  mergepts our/their
++  ankh                                                ::  fs node (new)
          $:  p=cash                                    ::  recursive hash
              q=(unit ,[p=cash q=*])                    ::  file
              r=(map ,@ta ankh)                         ::  folders
          ==                                            ::
++  apex  ,[p=@uvI q=(map ,@ta ,@uvI) r=(map ,@ta ,~)]  ::  node report (old)
++  arch  ,[p=@uvI q=(unit ,@uvI) r=(map ,@ta ,~)]      ::  node report (new)
++  ball  ,@uw                                          ::  statement payload
++  bait  ,[p=skin q=@ud r=dove]                        ::  fmt nrecvd spec
++  bath                                                ::  convo per client
          $:  sop=shed                                  ::  not stalled
              raz=(map path race)                       ::  statements inbound
              ryl=(map path rill)                       ::  statements outbound
          ==                                            ::
++  bead                                                ::  terminal control
          $:  $:  bul=@ud                               ::  buffer length
                  bus=@ud                               ::  cursor in buffer
                  but=(list ,@c)                        ::  buffer text
                  buy=prom                              ::  input style
              ==                                        ::
              $:  hiz=@ud                               ::  history depth
                  hux=path                              ::  history path
                  hym=(map ,@ud (list ,@c))             ::  history overlay
                  hyt=hist                              ::  history object
                  hyr=(unit (list ,@c))                 ::  history search
              ==                                        ::
              $:  pol=@ud                               ::  length of prompt
                  pot=tape                              ::  prompt text
              ==                                        ::
          ==                                            ::
++  beak  ,[p=(unit ,@ud) q=(map wire goal) r=boor]     ::  next/want/thread
++  bear  ,[p=(map path goal) q=boar]                   ::  thread with slips
++  beef                                                ::  raw product
          $:  p=(list gilt)                             ::  actions
              q=(list slip)                             ::  requests
              r=boar                                    ::  state
          ==                                            ::
++  bell  path                                          ::  label
++  bird                                                ::  packet in travel
          $:  gom=soap                                  ::  message identity
              mup=@ud                                   ::  pktno in msg
              nux=@ud                                   ::  xmission count
              lys=@da                                   ::  last sent
              pac=rock                                  ::  packet data
          ==                                            ::
++  belt                                                ::  raw console input
          $%  [%aro p=?(%d %l %r %u)]                   ::  arrow key
              [%bac ~]                                  ::  true backspace
              [%ctl p=@ud]                              ::  control-key
              [%del ~]                                  ::  true delete
              [%met p=@ud]                              ::  meta-key
              [%ret ~]                                  ::  return
              [%txt p=(list ,@c)]                       ::  utf32 text
          ==                                            ::
++  blew  ,[p=@ud q=@ud]                                ::  columns rows
++  blit                                                ::  raw console output
          $%  [%bel ~]                                  ::  make a noise
              [%clr ~]                                  ::  clear the screen
              [%hop p=@ud]                              ::  set cursor position
              [%lin p=(list ,@c)]                       ::  set current line
              [%mor ~]                                  ::  newline
              [%sav p=path q=@]                         ::  save to file
          ==                                            ::
++  blot                                                ::  kill ring
          $:  p=@ud                                     ::  length
              q=@ud                                     ::  depth
              r=(list (list ,@c))                       ::  kills
          ==                                            ::
++  blur  ,[p=@ud q=(unit bead) r=blot]                 ::  columns, prompt
++  boar                                                ::  execution instance
          $%  [%n p=(unit coal) q=claw r=lath]          ::  new/ready
              [%r p=(unit worm)]                        ::  running/done
              [%t p=coal]                               ::  simple filter
          ==                                            ::
++  boor                                                ::  new thread
          $:  p=(map ,@ud kite)                         ::  dependencies
              q=(qeu ,[p=wire q=card])                  ::  incoming cards
              r=(qeu ,[p=wire q=nose])                  ::  pending notes
              s=boar                                    ::  execution
          ==                                            ::
++  boat  ,[(list slip) tart]                           ::  user stage
++  boon                                                ::  fort output
          $%  [%beer p=ship q=@uvG]                     ::  gained ownership
              [%coke p=sock q=soap r=cape s=duct]       ::  message result
              [%mead p=lane q=rock]                     ::  accept packet
              [%milk p=sock q=soap r=*]                 ::  accept message
              [%ouzo p=lane q=rock]                     ::  transmit packet
              [%wine p=sock q=tape]                     ::  notify user
          ==                                            ::
++  bowl  ,[p=(list gift) q=(unit boat)]                ::  app product
++  brad                                                ::  shell state
          $:  who=ship                                  ::  identity
              fog=(list ,@ud)                           ::  virtual consoles
              hox=@ta                                   ::  identity text
              cwd=@tas                                  ::  working disc
              cws=path                                  ::  working spur
              way=(map ,@tas vase)                      ::  variables
              hit=[p=@ud q=(list ,@t)]                  ::  command history
              sur=[p=@ud q=(qeu vase)]                  ::  result history
              god=[p=@ud q=(map ,@ud task)]             ::  tasks
          ==                                            ::
++  bray  ,[p=life q=(unit life) r=ship s=@da]          ::  our parent us now
++  brow  ,[p=@da q=@tas]                               ::  browser version
++  buck  ,[p=mace q=will]                              ::  all security data
++  cake  ,[p=sock q=skin r=@]                          ::  top level packet
++  cape                                                ::  end-to-end result
          $?  %good                                     ::  delivered
              %dead                                     ::  rejected
          ==                                            ::
++  card                                                ::  event
          $%  [%bbye ~]                                 ::  reset prompt
              [%band p=ship q=(list rout)]              ::  internal http bind
              [%bind p=(unit ship) q=host]              ::  external http open
              [%belt p=belt]                            ::  terminal input
              [%blew p=blew]                            ::  terminal config
              [%blit p=(list blit)]                     ::  terminal output
              [%boot p=card]                            ::  christen terminal
              [%born ~]                                 ::  new unix process
              [%cash p=@p q=buck]                       ::  civil license
              [%crud p=@tas q=(list tank)]              ::  error with trace
              [%deem p=ship q=card]                     ::  external identity
              [%dire p=@tas q=dram]                     ::  apply directory
              [%dump p=(list ,@t)]                      ::  raw text lines
              [%ergo p=@p q=@tas r=@ud]                 ::  version update
              [%file p=@tas q=@]                        ::  apply atomic file
              [%fail p=tape]                            ::  report failure
              [%grab ~]                                 ::  collect grabage
              [%hail ~]                                 ::  refresh
              [%hear p=lane q=@]                        ::  receive packet
              [%hemp p=path]                            ::  cancel request
              [%helo p=path q=prod]                     ::  trigger prompt
              [%hole p=lane q=@]                        ::  packet failed
              [%hoop p=(unit)]                          ::  namespace response
              [%hope p=path]                            ::  namespace request
              [%info p=@p q=@tas r=nori]                ::  internal edit
              [%init p=@p]                              ::  report install
              [%into p=@p q=@tas r=nori]                ::  external edit
              [%flog p=card]                            ::  log to terminal
              [%junk p=@]                               ::  entropy
              [%kick p=@da]                             ::  wake up
              [%kill p=~]                               ::  kill a task
              [%lane p=lane]                            ::  set public route
              [%line p=@t]                              ::  source line
              [%limn ~]                                 ::  rotate ship
              [%ling ~]                                 ::  rotate interface
              [%load p=@tas q=path]                     ::  request atomic file
              [%loin p=@p q=chum]                       ::  name hashed-pass
              [%logo ~]                                 ::  logout
              [%loot p=@tas q=path]                     ::  request directory
              [%make p=(unit ,@t) q=@ud r=@]            ::  wild license
              [%mine p=@ud q=@t]                        ::  query matched line
              [%noop ~]                                 ::  no operation
              [%note p=@tD q=tank]                      ::  debug message
              [%nuke p=~]                               ::  kill all processes
              [%pace p=@ud]                             ::  compute background
              [%pipe p=(unit ,[p=typo q=(list)])]       ::  pipeline data
              [%pour p=path q=dram]                     ::  write directory
              [%pull p=ship q=disc r=(list disc)]       ::  pull remote desk
              [%pump ~]                                 ::  produce packets
              [%quid p=ship q=path r=(unit ,*)]         ::  delivery
              [%rein p=? q=path]                        ::  push/replace kernel
              [%rend ~]                                 ::  pop kernel
              [%rest ~]                                 ::  reset to factory
              [%save p=path q=@]                        ::  write atomic file
              [%send p=lane q=@]                        ::  transmit packet
              [%sith p=@p q=@uw]                        ::  imperial generator
              [%sync ~]                                 ::  reset soft state
              [%talk p=tank]                            ::  show on console
              [%tell p=(list ,@t)]                      ::  dump lines
              [%text p=tape]                            ::  talk leaf
              [%that p=@ud q=love]                      ::  cooked htresp
              [%thee p=@ud q=scab r=cred s=moth]        ::  cooked htreq
              [%them p=(unit hiss)]                     ::  internal client req
              [%they p=@ud q=httr]                      ::  response to %them
              [%this p=? q=@ud r=httq]                  ::  secure/session/req
              [%thou p=httr]                            ::  raw http response
              [%thug p=@p q=@p]                         ::  sign in client
              [%thus p=@ud q=(unit hiss)]               ::  http request
              [%tory p=(list ,@t)]                      ::  history dump
              [%veer p=@ta q=path r=@t]                 ::  install vane
              [%volt p=*]                               ::  upgrade kernel
              [%wait p=@da q=path]                      ::  timer wait
              [%wake ~]                                 ::  timer activate
              [%waft p=ship q=*]                        ::  response message
              [%want p=ship q=path r=*]                 ::  send message
              [%warn p=tape]                            ::  syslog
              [%warp p=ship q=riff]                     ::  file request
              [%wart p=ship q=@tas r=path s=*]          ::  service request
              [%went p=ship q=cape]                     ::  reaction message
              [%wipe ~]                                 ::  clean to sequence
              [%word p=chum]                            ::  set password
              [%writ p=riot]                            ::  response
          ==                                            ::
++  cart  ,[p=cash q=cash]                              ::  hash change
++  care  ?(%v %w %x %y %z)                             ::  clay submode
++  case                                                ::  modeshipdeskcasespur
          $%  [%da p=@da]                               ::  date
              [%tas p=@tas]                             ::  label
              [%ud p=@ud]                               ::  number
          ==                                            ::
++  cash  ,@uvH                                         ::  ankh hash
++  cask                                                ::  symmetric record
          $:  yed=(unit ,[p=hand q=code])               ::  outbound
              heg=(map hand code)                       ::  proposed
              qim=(map hand code)                       ::  inbound
          ==                                            ::
++  coal  ,*                                            ::  untyped vase
++  code  ,@uvI                                         ::  symmetric key
++  cone                                                ::  reconfiguration
          $%  [& p=twig]                                ::  transform
              [| p=(list ,@tas)]                        ::  alter
          ==                                            ::
++  chum  ,@uvI                                         ::  hashed passcode
++  claw  $:                                            ::  startup chain
              joy=(unit coal)                           ::  local context
              ran=(unit coal)                           ::  arguments
              pux=(unit path)                           ::  execution path
              jiv=(unit coal)                           ::  app configuration
              kyq=(unit coal)                           ::  app customization
              gam=(unit coal)                           ::  app image
          ==                                            ::
++  cred  ,[p=? q=logo q=oryx r=(unit ship)]            ::  client credentials
++  cult  (map duct rave)                               ::  subscriptions
++  deed  ,[p=@ q=step]                                 ::  signature, stage
++  dome                                                ::  project state
          $:  ang=agon                                  ::  pedigree
              ank=ankh                                  ::  new state
              let=@                                     ::  (lent hit)
              hit=(list frog)                           ::  changes in reverse
              lab=(map ,@tas ,@ud)                      ::  labels
          ==                                            ::
++  desk  ,[p=cult q=dome]                              ::  domestic desk state
++  disc  ,@ta                                          ::  modeshipdeskcasespur
++  door                                                ::  foreign contact
          $:  wod=road                                  ::  connection to
              wyl=will                                  ::  inferred mirror
              caq=cask                                  ::  symmetric key state
          ==                                            ::
++  dove  ,[p=@ud q=(map ,@ud ,@)]                      ::  count hash 13-blocks
++  flap  ,@uvH                                         ::  network packet id
++  flow                                                ::  packet connection
          $:  rtt=@dr                                   ::  decaying avg rtt
              wid=@ud                                   ::  logical wdow msgs
          ==                                            ::
++  fort                                                ::  formal state
          $:  hop=@da                                   ::  network boot date
              ton=town                                  ::  security
              zac=(map ship oven)                       ::  flows by server
          ==                                            ::
++  frog  ,[p=@da q=nori]                               ::  time and change
++  gift                                                ::  one-way effect
          $%  [%$ p=vase]                               ::  trivial output
              [%cc p=(unit case)]                       ::  change case
              [%ck p=@tas]                              ::  change desk
              [%cs p=path]                              ::  change spur
              [%de p=@ud q=tank]                        ::  debug/level
              [%ex p=(unit vase) q=lath]                ::  exec/patch
              [%ha p=tank]                              ::  single error
              [%ho p=(list tank)]                       ::  multiple error
              [%la p=tank]                              ::  single statement
              [%lo p=(list tank)]                       ::  multiple statement
              [%mu p=type q=(list)]                     ::  batch emit
              [%mx p=(list gift)]                       ::  batch gift
              [%ok p=disc q=nori]                       ::  save changes
              [%sc p=(unit skit)]                       ::  stack library
              [%sp p=(list lark)]                       ::  spawn task(s)
              [%sq p=ship q=@tas r=path s=*]            ::  send request
              [%sr p=ship q=path r=*]                   ::  send response
              [%te p=(list ,@t)]                        ::  dump lines
              [%th p=@ud q=love]                        ::  http response
              [%tq p=path q=hiss]                       ::  http request
              [%va p=@tas q=(unit vase)]                ::  set/clear variable
              [%xx p=card]                              ::  return card
              [%xy p=path q=card]                       ::  push card
          ==                                            ::
++  gilt  ,[@tas *]                                     ::  presumed gift
++  gens  ,[p=lang q=gcos]                              ::  general identity
++  germ  ?(%fine %that %this %mate)                    ::  merge style
++  gcos                                                ::  id description
          $%  [%czar ~]                                 ::  8-bit ship
              [%duke p=what]                            ::  32-bit ship
              [%earl p=@t]                              ::  64-bit ship
              [%king p=@t]                              ::  16-bit ship
              [%pawn p=(unit ,@t)]                      ::  128-bit ship
          ==                                            ::
++  goad                                                ::  common note
          $%  [%eg p=riot]                              ::  simple result
              [%hp p=httr]                              ::  http response
              [%ht p=@ud q=scab r=cred s=moth]          ::  http request
              [%it p=~]                                 ::  interrupt event
              [%lq p=ship q=path r=*]                   ::  client request
              [%ly p=newt q=tape]                       ::  lifecycle event
              [%ow p=cape]                              ::  one-way reaction
              [%rt p=(unit)]                            ::  roundtrip response
              [%up p=@t]                                ::  prompt response
              [%wa ~]                                   ::  alarm
          ==                                            ::
++  goal                                                ::  app request
          $%  [%$ p=type]                               ::  open for input
              [%do p=vase q=vase]                       ::  call gate sample
              [%eg p=kite]                              ::  single request
              [%es p=ship q=disc r=rave]                ::  subscription
              [%ht p=(list rout)]                       ::  http server
              [%hp ~]                                   ::  http response
              [%lq p=@tas]                              ::  listen for service
              [%ow ~]                                   ::  one-way reaction
              [%rt ~]                                   ::  roundtrip response
              [%up p=prod]                              ::  user prompt
              [%wa p=@da]                               ::  alarm
          ==                                            ::
++  govt  path                                          ::  country/postcode
++  gram  ,@uw                                          ::  physical datagram
++  gyro  ,[p=@ud q=wire r=prod]                        ::  live prompt
++  hand  ,@uvH                                         ::  hash of code
++  hate  ,[p=purl q=@p r=moth]                         ::  cooked request
++  hiss  ,[p=hart q=httq]                              ::  outbound request
++  hist  ,[p=@ud q=(list ,@t)]                         ::  depth texts
++  hook  path                                          ::  request origin
++  hart  ,[p=? q=(unit ,@ud) r=host]                   ::  http sec/port/host
++  hort  ,[p=(unit ,@ud) q=host]                       ::  http port/host
++  host  $%([& p=(list ,@t)] [| p=@if])                ::  http host
++  httq                                                ::  raw http request
          $:  p=?(%get %post)                           ::  method
              q=@t                                      ::  unparsed url
              r=(list ,[p=@t q=@t])                     ::  headers
              s=(unit octs)                             ::  body
          ==                                            ::
++  httr  ,[p=@ud q=mess r=(unit octs)]                 ::  raw http response
++  kite  ,[p=care q=case r=ship s=disc t=spur]         ::  parsed global name
++  json                                                ::  json top level
          $%  [%a p=(list jval)]                        ::  array
              [%o p=(map ,@t jval)]                     ::  object
          ==                                            ::
++  jval                                                ::  json value
          $|  ~                                         ::  null
          $?  json                                      ::
              $%  [%b p=?]                              ::  boolean
                  [%n p=@ta]                            ::  number
                  [%s p=@ta]                            ::  string
              ==                                        ::
          ==                                            ::
++  lamb                                                ::  short path
          $%  [& p=@tas]                                ::  auto
              [| p=twig]                                ::  manual
          ==                                            ::
++  lane                                                ::  packet route
          $%  [%if p=@ud q=@if]                         ::  IP4/public UDP/addr
              [%is p=@ud q=(unit lane) r=@is]           ::  IPv6 w/alternates
              [%ix p=@da q=@ud r=@if]                   ::  IPv4 provisional
          ==                                            ::
++  lang  ,@ta                                          ::  IETF lang as code
++  lark  ,[p=(unit ,@tas) q=lawn]                      ::  parsed command
++  lass  ?(%0 %1 %2)                                   ::  power increment
++  lath  $%                                            ::  pipeline stage
              [%0 p=lass q=lamb r=(list cone) s=twig]   ::  command
              [%1 p=twig]                               ::  generator
              [%2 p=twig]                               ::  filter
          ==                                            ::
++  lawn  (list lath)                                   ::
++  lens  ?(%z %y %x %w)                                ::  repository view
++  lice  ,[p=ship q=buck]                              ::  full license
++  life  ,@ud                                          ::  regime number
++  lint  (list rock)                                   ::  fragment array
++  logo  ,@uvI                                         ::  session identity
++  love  $%                                            ::  http response
              [%ham p=manx]                             ::  html node
              [%mid p=mime q=octs]                      ::  mime-typed data
              [%raw p=httr]                             ::  raw http response
          ==                                            ::
++  mace  (list ,[p=life q=ring])                       ::  private secrets
++  mane  $|(@tas [@tas @tas])                          ::  XML name/space
++  manx  ,[t=marx c=marl]                              ::  XML node
++  marl  (list manx)                                   ::  XML node list
++  mars  ,[t=[n=%$ a=[i=[n=%$ v=tape] t=~]] c=~]       ::  XML cdata
++  mart  (list ,[n=mane v=tape])                       ::  XML attributes
++  marv  ?(%da %tas %ud)                               ::  release form
++  marx  $|(@tas [n=mane a=mart])                      ::  XML tag
++  masu  ,[p=ship q=disc r=moar s=moar]                ::  sync square
++  math  (map ,@t (list ,@t))                          ::  semiparsed headers
++  meal                                                ::  payload
          $%  [%back p=cape q=flap r=@dr]               ::  acknowledgment
              [%bond p=life q=path r=@ud s=*]           ::  message
              [%carp p=@ q=@ud r=@ud s=flap t=@]        ::  skin/inx/cnt/hash
              [%fore p=ship q=(unit lane) r=@]          ::  forwarded packet
          ==                                            ::
++  mess  (list ,[p=@t q=@t])                           ::  raw http headers
++  meta                                                ::  path metadata
          $%  [& q=@uvI]                                ::  hash
              [| q=(list ,@ta)]                         ::  dir
          ==                                            ::
++  meth  ?(%get %post)                                 ::  http method
++  mime  (list ,@ta)                                   ::  mime type
++  miso                                                ::  ankh delta
          $%  [%del p=*]                                ::  delete
              [%ins p=*]                                ::  insert
              [%mut p=udon]                             ::  mutate
          ==                                            ::
++  moar  ,[p=@ud q=@ud]                                ::  normal change range
++  moat  ,[p=case q=case]                              ::  change range
++  mood  ,[p=care q=case r=path]                       ::  request in desk
++  moth  ,[p=meth q=math r=(unit octs)]                ::  http operation
++  name  ,[p=@t q=(unit ,@t) r=(unit ,@t) s=@t]        ::  first mid/nick last
++  newt  ?(%boot %kick %mess %slay %wake)              ::  lifecycle events
++  nose                                                ::  response, kernel
          $?  [%$ p=(unit ,[p=typo q=(list)])]          ::  standard input
              goad                                      ::
          ==                                            ::
++  note                                                ::  response, user
          $?  [%$ p=(unit ,[p=type q=(list)])]          ::  standard input
              [%do p=vase]                              ::  execution result
              goad                                      ::
          ==                                            ::
++  nori                                                ::  repository action
          $%  [& p=(unit masu) q=soba]                  ::  delta
              [| p=@tas]                                ::  label
          ==                                            ::
++  octs  ,[p=@ud q=@]                                  ::  octet-stream
++  oryx  ,@uvH                                         ::  CSRF secret
++  oven                                                ::  flow by server
          $:  hen=duct                                  ::  admin channel
              nys=(map flap bait)                       ::  packets incoming
              olz=(map flap cape)                       ::  packets completed
              wab=(map ship bath)                       ::  relationship
          ==                                            ::
++  pact  path                                          ::  routed path
++  pail  ?(%none %warm %cold)                          ::  connection status
++  plan                                                ::  conversation state
          $:  $=  sat                                   ::  statistics
              $:  nex=@da                               ::  next wakeup
                  wid=@ud                               ::  max outstanding
              ==                                        ::
          ==                                            ::
++  plea  ,[p=@ud q=[p=? q=@t]]                         ::  live prompt
++  pork  ,[p=(unit ,@ta) q=path]                       ::  fully parsed url
++  prod  ,[p=prom q=tape r=tape]                       ::  prompt
++  prom  ?(%text %pass %none)                          ::  format type
++  purl  ,[p=hart q=pork r=quay]                       ::  parsed url
++  putt                                                ::  outgoing message
          $:  ski=snow                                  ::  sequence acked/sent
              wyv=(list rock)                           ::  packet list XX gear
          ==                                            ::
++  pyre                                                ::  cascade stash
          $:  p=(map ,[p=path q=path r=coal] coal)      ::  by path
              q=(map ,[p=path q=@uvI r=coal] coal)      ::  by source hash
              r=(map ,[p=* q=coal] coal)                ::  by (soft) twig
          ==                                            ::
++  quay  (map ,@t ,@t)                                 ::  parsed url query
++  quri                                                ::  request-uri
          $%  [& p=purl]                                ::  absolute
              [| p=pork q=quay]                         ::  relative
          ==                                            ::
++  race                                                ::  inbound stream
          $:  did=@ud                                   ::  filled sequence
              bum=(map ,@ud ,%dead)                     ::
              mis=(map ,@ud ,[p=cape q=flap r=(unit)])  ::  misordered
          ==                                            ::
++  raft                                                ::  filesystem
          $:  las=@da                                   ::  last wakeup
              fat=(map ,@p room)                        ::  per host
          ==                                            ::
++  rank  ?(%czar %king %duke %earl %pawn)              ::  ship width class
++  rant                                                ::  namespace binding
          $:  p=[p=care q=case r=@tas]                  ::  clade release book
              q=path                                    ::  spur
              r=*                                       ::  data
          ==                                            ::
++  rave                                                ::  general request
          $%  [& p=mood]                                ::  single request
              [| p=moat]                                ::  change range
          ==                                            ::
++  rede                                                ::  universal project
          $:  lim=@da                                   ::  complete to
              qyx=cult                                  ::  subscribers
              ref=(unit rind)                           ::  outgoing requests
              dom=dome                                  ::  revision state
          ==                                            ::
++  riff  ,[p=disc q=(unit rave)]                       ::  request/desist
++  rill                                                ::  outbound stream
          $:  sed=@ud                                   ::  sent
              san=(map ,@ud duct)                       ::  outstanding
          ==                                            ::
++  rind                                                ::  request manager
          $:  nix=@ud                                   ::  request index
              bom=(map ,@ud ,[p=duct q=rave])           ::  outstanding
              fod=(map duct ,@ud)                       ::  current requests
              haw=(map mood (unit))                     ::  simple cache
          ==                                            ::
++  riot  (unit rant)                                   ::  response/complete
++  road                                                ::  secured oneway route
          $:  exp=@da                                   ::  expiration date
              lun=(unit lane)                           ::  route to friend
              lew=will                                  ::  will of friend
          ==                                            ::
++  room                                                ::  fs per ship (new)
          $:  hun=duct                                  ::  terminal duct
              hez=(unit duct)                           ::  sync duct
              dos=(map ,@tas ,[p=cult q=dome])          ::  native projects
              rid=(map ship (map ,@tas rede))           ::  neighbors
          ==                                            ::
++  rock  ,@uvO                                         ::  packet
++  rout  ,[p=(list host) q=path r=oryx s=path]         ::  http route (new)
++  rump  ,[p=care q=case r=@tas s=path]                ::  relative path
++  saba  ,[p=ship q=@tas r=moar s=(list nori)]         ::  patch/merge
++  safe                                                ::  domestic host
          $:  hoy=(list ship)                           ::  hierarchy
              val=wand                                  ::  private keys
              law=will                                  ::  server will
              seh=(map hand ,[p=ship q=@da])            ::  key cache
              hoc=(map ship door)                       ::  neighborhood
          ==                                            ::
++  salt  ,@uv                                          ::  entropy
++  scab                                                ::  logical request
          $:  p=scud                                    ::  routed url
              q=quay                                    ::  query
          ==                                            ::
++  scad  ,[p=@p q=@da r=@uw s=cred]                    ::  fab context, outer
++  scar                                                ::  logical url
          $:  p=hart                                    ::  scheme/host
              q=path                                    ::  trunk
              r=(unit ,@ta)                             ::  extension
              s=path                                    ::  detour
          ==                                            ::
++  scud  ,[p=pact q=scar]                              ::  processed dispatch
++  seam  ,[p=@ta q=pact r=scar]                        ::  service route
++  sect  ?(%black %blue %red %orange %white)           ::  banner
++  shed                                                ::  packet flow
          $:  $:  rtt=@dr                               ::  smoothed rtt
                  rto=@dr                               ::  retransmit timeout
                  rtn=(unit ,@da)                       ::  next timeout
                  rue=(unit ,@da)                       ::  last heard from
              ==                                        ::
              $:  nus=@ud                               ::  number sent
                  nif=@ud                               ::  number live
                  nep=@ud                               ::  next expected
                  caw=@ud                               ::  logical window
                  cag=@ud                               ::  congest thresh
              ==                                        ::
              $:  diq=(map flap ,@ud)                   ::  packets sent
                  pyz=(map soup ,@ud)                   ::  message/unacked
                  puq=(qeu ,[p=@ud q=soul])             ::  packet queue
              ==                                        ::
          ==                                            ::
++  skit  ,[p=(unit ,@ta) q=(list ,@ta) r=(list ,@ta)]  ::  tracking path
++  sink                                                ::  incoming per server
          $:  nes=(map flap ,[p=@da q=bait])            ::  fragment actions
          ==                                            ::
++  skin  ?(%none %open %fast %full)                    ::  encoding stem
++  slip  ,[p=path q=goal]                              ::  traceable request
++  sled  ,[p=* q=*]                                    ::  [data code]
++  snow  ,[p=@ud q=@ud r=(set ,@ud)]                   ::  window exceptions
++  soap  ,[p=[p=life q=life] q=path r=@ud]             ::  statement id
++  soup  ,[p=path q=@ud]                               ::  new statement id
++  soul                                                ::  packet in travel
          $:  gom=soup                                  ::  message identity
              nux=@ud                                   ::  xmission count
              liv=?                                     ::  deemed live
              lys=@da                                   ::  last sent
              pac=rock                                  ::  packet data
          ==                                            ::
++  soba  ,[p=cart q=(list tako)]                       ::  delta
++  sock  ,[p=ship q=ship]                              ::  from to
++  spur  path                                          ::  modeshipdeskcasespur
++  step  ,[p=bray q=gens r=pass]                       ::  identity stage
++  tako  ,[p=path q=miso]                              ::  change detail
++  tart  $+([@da path note] bowl)                     ::  process core
++  task                                                ::
          $:  paq=(qeu gyro)                            ::  prompt queue
              wip=[p=@ud q=(map ,@ud beak)]             ::  processes
          ==                                            ::
++  taxi  ,[p=lane q=rock]                              ::  routed packet
++  tick  ,@ud                                          ::  process id
++  town                                                ::  all security state
          $:  lit=@ud                                   ::  imperial modulus
              any=@                                     ::  entropy
              urb=(map ship safe)                       ::  all keys and routes
          ==                                            ::
++  tube  path                                          ::  message channel
++  typo  ,*                                            ::  presumed type
++  wand  (list ,[p=life q=ring r=acro])                ::  mace in action
++  what                                                ::  logical identity
          $%  [%anon ~]                                 ::  anonymous
              [%lady p=whom]                            ::  female person ()
              [%lord p=whom]                            ::  male person []
              [%punk p=sect q=@t]                       ::  opaque handle ""
          ==                                            ::
++  whom  ,[p=@ud q=govt r=sect s=name]                 ::  year/govt/id
++  will  (list deed)                                   ::  certificate
++  worm  ,*                                            ::  vase of tart
++  yard                                                ::  terminal state
          $:  p=?                                       ::  verbose
              q=blur                                    ::  display state
              r=(map path hist)                         ::  history
          ==                                            ::
--
