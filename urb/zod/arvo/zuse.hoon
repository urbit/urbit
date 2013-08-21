::
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
  =+  d=@
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
  =+  [mos=@ pon=*(unit ,[p=@ q=@ r=[p=@ q=@] s=_*fu])]
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
  [[%% [%% tep] ~] ~]
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
  ?:  ?=([%% [[%% *] ~]] t.mex)
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
++  cure                                                ::  invert tree patch
  |=  mis=miso
  ?-  -.mis
    %del  [%ins p.mis]
    %ins  [%del p.mis]
    %mut  [%mut (limp p.mis)]
  ==
::
++  cyst                                                ::  old external patch
  |=  [bus=arch arc=arch]
  ^-  soba
  doz:(dist:ka:(cu arc) %c bus)
::
++  cu 
  |=  arc=arch                                          ::  filesystem tree
  =|  doz=soba                                          ::  changes in reverse
  |%
  ++  abet
    ^-  [p=soba q=arch]
    [(flop doz) arc]
  ::
  ++  ka
    =|  ram=path                                        ::  reverse path
    |%
    ++  dare  ..ka                                      ::  retract
    ++  dash                                            ::  ascend
      |=  [lol=@ta rac=arch]
      ^+  +>
      ?>  &(?=(^ ram) =(lol i.ram))
      %=    +>
          ram  t.ram
          arc
        :-  %|
        ?>  ?=(| -.rac)
        ?:  =(arc [| ~])
          ?.  (~(has by p.rac) lol)  p.rac
          (~(del by p.rac) lol)
        (~(put by p.rac) lol arc)
      ==
    ::
    ++  deaf                                            ::  add ukaz
      |=  mis=miso
      ^+  +>
      +>(doz [[(flop ram) mis] doz])
    ::
    ++  dent                                            ::  descend
      |=  lol=@ta
      ^+  +>
      =+  you=?:(?=(& -.arc) ~ (~(get by p.arc) lol))
      +>.$(ram [lol ram], arc ?~(you [%| ~] u.you))
    ::
    ++  deny                                            ::  descend recursively
      |=  way=path
      ^+  +>
      ?~(way +> $(way t.way, +> (dent i.way)))
    ::
    ++  dest                                            ::  write over
      |=  [pum=umph val=*]
      ^+  +>
      ?-   -.arc
        |  (deaf:dirk %ins val)
        &  (deaf %mut ((diff pum) q.arc val))
      ==
    ::
    ++  dist                                            ::  modify to
      |=  [pum=umph bus=arch]
      ^+  +>
      ?-    -.bus
          &  ?:(&(?=(& -.arc) =(p.bus p.arc)) +> (dest pum q.bus))
          |
        =.  +>  ?.(?=(& -.arc) +> %*(. dirk arc [%| ~]))
        ?>  ?=(| -.arc)
        =+  [yeg=(~(tap by p.arc) ~) gey=(~(tap by p.bus) ~)]
        =>  .(arc `arch`arc)
        =.  +>.$
          |-  ^+  +>.^$
          ?~  yeg  +>.^$
          ?:  (~(has by p.bus) p.i.yeg)  $(yeg t.yeg)
          $(yeg t.yeg, doz doz:dirk(arc q.i.yeg, ram [p.i.yeg ram]))
        =.  +>.$
          |-  ^+  +>.^$
          ?~  gey  +>.^$
          $(gey t.gey, doz doz:^$(bus q.i.gey, +> (dent p.i.gey)))
        +>.$
      ==
    ::
    ++  dirk                                            ::  rm -r
      |-  ^+  +
      ?-    -.arc
          &  (deaf %del q.arc)
          |
        =+  dyr=(~(tap by p.arc) ~)
        =>  .(arc `arch`arc)
        |-  ^+  +.^$
        ?~  dyr  +.^$
        =.  +.^$  dirk:(dent p.i.dyr)
        $(dyr t.dyr)
      ==
    ::
    ++  drum                                            ::  apply change
      |=  [pax=path mis=miso]
      ^+  +>
      ?^  pax 
        ?>  ?=(| -.arc)
        (dash:$(pax t.pax, +> (dent i.pax)) i.pax arc)
      ?-    -.mis
          %del
        ?>  &(?=(& -.arc) =(q.arc p.mis))
        +>.$(arc [%| ~])
      ::
          %ins
        ?>  ?=([| ~] arc)
        +>.$(arc [%& (sham p.mis) p.mis])
      ::
          %mut
        ?>  ?=(& -.arc)
        =+  nex=(lump p.mis q.arc)
        +>.$(arc [%& (sham nex) nex])
      ==
    ::
    ++  durn                                            ::  apply forward
      |=  nyp=soba
      ^+  +>
      ?~  nyp  +>
      $(nyp t.nyp, +> (drum `path`p.i.nyp `miso`q.i.nyp))
    ::
    ++  dusk                                            ::  apply reverse
      |=  nyp=soba
      (durn (turn (flop nyp) |=([a=path b=miso] [a (cure b)])))
    -- 
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
      [~ [~(rent co %% %t i.rax) ~]]
    [+.p.u.q.vex [-.p.u.q.vex ~]]
  =+  pok=$(rax t.rax)
  :-  p.pok
  :_  q.pok
  ?:(((sane %tas) i.rax) i.rax ~(rent co %% %t i.rax))
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
    %+  cook  |*(a=[* *] [+.a -.a])
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
        |=(a=tape [[%% (crip a)] ~])
      (star pque)
    ==
  ++  zest                                              ::  2616 request-uri
    ;~  pose
      (stag %& (cook |=(a=purl a) auri))
      (stag %| ;~(plug apat yque))
    ==
  --
::
++  fade                                                ::  load cascade
  |=  [end=@tas hut=path sur=path]
  %-  flop
  |-  ^-  (list ,[p=path q=*])
  =+  ape=((hard apex) .^(%cy hut))
  ?.  (~(has by q.ape) end)  ~
  :-  [hut .^(%cx (weld hut `path`[end ~]))]
  ?~  sur  ~
  ?.  (~(has by r.ape) i.sur)  ~
  $(sur t.sur, hut (weld hut `path`[i.sur ~]))
::
++  feel                                                ::  apex load
  |=  [hut=path sur=path]  
  |-  ^-  (unit apex)
  =+  ape=((hard apex) .^(%cy hut))
  ?~  sur  [~ ape]
  =+  rug=(~(get by r.ape) i.sur)
  ?~  rug  ~
  $(sur t.sur, hut (weld hut `path`[i.sur ~]))
::
++  file                                                ::  file or not
  |=  pax=path
  ^-  (unit)
  ?~  pax  ~
  =+  lin=(dec (lent pax))
  =+  [pex=(scag lin `path`pax) poy=(slag lin `path`pax)]
  =+  ape=((hard apex) .^(%cy pex))
  ?.  (~(has by q.ape) -.poy)  ~
  [~ .^(%cx pax)]
::
++  glam 
  |=  zar=@p  ^-  tape
  %+  snag  zar
  :~  "Tianming"  "Pepin the Short"  "Haile Selassie"  "Alfred the Great"  
      "Tamerlane"  "Pericles"  "Talleyrand"  "Yongle"  "Seleucus"  
      "Uther Pendragon"  "Louis XVI"  "Ahmad Shāh Durrānī"  "Constantine"  
      "Wilhelm I"  "Akbar"  "Louis XIV"  "Nobunaga"  "Alexander VI"  
      "Philippe II"  "Julius II"  "David"  "Niall Noígíallach"  "Kublai Khan"  
      "Öz Beg Khan"  "Ozymandias"  "Ögedei Khan"  "Jiang Jieshi"  "Darius"  
      "Shivaji"  "Qianlong"  "Bolesław I Chrobry"  "Tigranes"  "Han Wudi"  
      "Charles X"  "Naresuan"  "Frederick II"  "Simeon"  "Kangxi"  
      "Suleiman the Magnificent"  "Pedro II"  "Genghis Khan"  "Đinh Bộ Lĩnh  
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
      %czar           (rap 3 (glam who))
      ?(%king %pawn)  p.gos
      ?(%duke %earl) 
    ?+    -.p.gos  p.p.p.gos
        %anon  %%
        %punk  p.p.gos
        ?(%lord %lady)  
      =+  nam=`name`s.p.p.gos
      %+  rap  3
      :~  p.nam
          ?~(q.nam 0 (cat 3 ' ' u.q.nam))
          ?~(r.nam 0 (rap 3 ' (' u.r.nam ')' ~))
          ' '
          s.nam
      ==
    ==
  ==
++  hunt
  |=  [one=(unit ,@da) two=(unit ,@da)]
  ^-  (unit ,@da)
  ?~  one  two
  ?~  two  one
  ?:((lth u.one u.two) one two)
::
++  juke                                                ::  remove role
  |=  pax=path  ^-  path
  ?>  ?=([@ @ @ @ *] pax)
  [i.pax i.t.pax i.t.t.pax t.t.t.t.pax]
::
++  jump                                                ::  insert role
  |=  [for=@tas pax=path]  ^-  path
  ?>  ?=([@ @ @ *] pax)
  [i.pax i.t.pax i.t.t.pax for t.t.t.pax]
::
++  meat
  |=  kit=kite
  ^-  path
  [(cat 3 'c' p.kit) (scot %p r.kit) s.kit (scot (dime q.kit)) t.kit]
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
  ?.  ?=([~ %% %tas @] hyr)  ~
  ?.  ?=([~ %% %p @] fal)  ~
  ?.  ?=([~ %% %tas @] dyc)  ~
  ?.  ?=(^ ved)  ~
  =+  his=`@p`q.p.u.fal
  =+  [dis=(end 3 1 q.p.u.hyr) rem=(rsh 3 1 q.p.u.hyr)]
  ?.  ?&(?=(%c dis) ?=(?(%w %x %y %z) rem))  ~
  [~ rem (case p.u.ved) q.p.u.fal q.p.u.dyc tyl]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bF, Arvo models              ::
::
++  acro                                                ::  asym cryptosuite
          $_  ^?  |%                                    ::  opaque object
          ++  de  |+([a=@ b=@] *(unit ,@))              ::  symmetric de, soft
          ++  dy  |+([a=@ b=@] @)                       ::  symmetric de, hard
          ++  en  |+([a=@ b=@] @)                       ::  symmetric en
          ++  es  |+(a=@ @)                             ::  step key to next
          ++  ex  ^?                                    ::  export
            |%  ++  fig  @uvH                           ::  fingerprint
                ++  pac  @uvG                           ::  default passcode
                ++  pub  *pass                          ::  public key
                ++  sec  *ring                          ::  private key
            --                                          ::
          ++  mx  @                                     ::  max direct bytes
          ++  nu  ^?                                    ::  reconstructors
            |%  ++  pit  |=([a=@ b=@] ^?(..nu))         ::  from [width seed]
                ++  nol  |=(a=@ ^?(..nu))               ::  from naked ring
                ++  com  |=(a=@ ^?(..nu))               ::  from naked pass
            --                                          ::
          ++  pu  ^?                                    ::  public-key acts
            |%  ++  seal  |=([a=@ b=@] @)               ::  encrypt
                ++  sure  |=([a=@ b=@] *(unit ,@))      ::  authenticate
            --                                          ::
          ++  se  ^?                                    ::  secret-key acts
            |%  ++  sign  |=([a=@ b=@] @)               ::  certify
                ++  tear  |=(a=@ *(unit ,[p=@ q=@]))    ::  accept
            --                                          ::
          --                                            ::
++  arch                                                ::  fs node (old)
          $%  [& p=@uvI q=*]                            ::  file, sham/data
              [| p=(map ,@ta arch)]                     ::  directory
          ==                                            ::
++  ankh                                                ::  fs node (new)
          $:  p=@uvI                                    ::  metahash
              q=(map ,@ta ,[p=@uvI q=*])                ::  files 
              r=(map ,@ta ankh)                         ::  folders
          ==                                            ::
++  apex  ,[p=@uvI q=(map ,@ta ,@uvI) r=(map ,@ta ,~)]  ::  node report
++  ball  ,@uw                                          ::  statement payload
++  bait  ,[p=skin q=@ud r=dove]                        ::  fmt nrecvd spec
++  bath                                                ::  convo per client
          $:  gay=?                                     ::  not stalled
              laz=(unit ,@da)                           ::  last heard
              ski=snow                                  ::  sequence state
              foy=flow                                  ::  flow statistics
              par=(map soap putt)                       ::  message by id
              maz=(qeu soap)                            ::  round-robin next
              air=(map flap ,@ud)                       ::  unacked by content
              sea=shed                                  ::  packet pump
              raz=(map ,@ta race)                       ::  statements inbound
              ryl=(map ,@ta rill)                       ::  statements outbound
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
              ==                                        ::
              $:  pol=@ud                               ::  length of prompt
                  pot=tape                              ::  prompt text
              ==                                        ::
          ==                                            ::
++  beak  ,[p=(unit ,@ud) q=(map wire goal) r=boor]     ::  next/want/process
++  bear  ,[p=(map path goal) q=boar]                   ::  process with slips
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
              dif=?                                     ::  deemed in flight
              pex=@da                                   ::  next expire
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
          $%  [%n p=claw q=lath]                        ::  new/ready
              [%r p=(unit worm)]                        ::  running/done
              [%t p=coal]                               ::  filter
          ==                                            ::
++  boor                                                ::  new process
          $:  p=(map ,@ud kite)                         ::  dependencies
              q=(qeu ,[p=wire q=card])                  ::  incoming cards 
              r=(qeu ,[p=wire q=nose])                  ::  pending notes
              s=boar                                    ::  execution
          ==                                            ::
++  boat  ,[(list slip) tart]                           ::  user stage
++  boon                                                ::  fort output
          $%  [%beer p=ship q=@uvG]                     ::  gained ownership
              [%coke p=sock q=cape r=soap s=duct]       ::  message result
              [%mead p=lane q=rock]                     ::  accept packet
              [%milk p=sock q=@tas r=@ud s=(unit ,*)]   ::  accept message
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
++  cake  ,[p=sock q=? r=skin s=@]                      ::  top level packet
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
              [%make p=@t q=@ud r=@]                    ::  wild license
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
              [%want p=ship q=@ta r=*]                  ::  send message
              [%warp p=ship q=riff]                     ::  request
              [%wart p=ship q=@ta r=@ud s=(unit ,*)]    ::  incoming response
              [%warn p=tape]                            ::  system message
              [%went p=ship q=cape r=soap]              ::  outgoing reaction
              [%wipe ~]                                 ::  clean to sequence
              [%word p=chum]                            ::  set password
              [%wort p=tape]                            ::  semantic failure
              [%writ p=riot]                            ::  response
          ==                                            ::
++  care  ?(%w %x %y %z)                                ::  clay submode
++  case                                                ::  modeshipdeskcasespur
          $%  [%da p=@da]                               ::  date
              [%tas p=@tas]                             ::  label
              [%ud p=@ud]                               ::  number
          ==                                            ::
++  cask                                                ::  symmetric record
          $:  yed=(unit ,[p=hand q=code])               ::  outbound
              heg=(map hand code)                       ::  proposed 
              qim=(map hand code)                       ::  inbound
          ==                                            ::
++  coal  ,*                                            ::  untyped vase
++  code  ,@uvI                                         ::  symmetric key
++  cone                                                ::  reconfiguration
          $%  [& p=gene]                                ::  transform
              [| p=(list ,@tas)]                        ::  alter
          ==                                            ::
++  corp  ,[p=@t q=@t r=govt]                           ::  name auth issuer
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
++  dock                                                ::  link record
          $:  for=ship                                  ::  host
              dys=@tas                                  ::  linked to desk
              kol=case                                  ::  last update case
              num=@ud                                   ::  update count
              cuz=(list ,[p=@ud q=dole])                ::  cumulative update
          ==                                            ::
++  dole  ,[p=(unit moar) q=(list maki)]                ::  flow trace
++  dome                                                ::  project state
          $:  arc=arch                                  ::  state
              let=@                                     ::  (lent hit)
              hit=(list frog)                           ::  changes in reverse
              lab=(map ,@tas ,@ud)                      ::  labels
          ==                                            ::
++  desk  ,[p=cult q=dome]                              ::  project state
++  disc  ,@ta                                          ::  modeshipdeskcasespur
++  door                                                ::  foreign contact
          $:  wod=road                                  ::  connection to
              wyl=will                                  ::  inferred mirror
              caq=cask                                  ::  symmetric key state
          ==                                            ::
++  dove  ,[p=@ud q=@uvH r=(map ,@ud ,@)]               ::  hash count 13-blocks
++  flap  ,@uvH                                         ::  network packet id
++  floe                                                ::  next gen stats
          $:  rtt=@dr                                   ::  decaying avg rtt
              wid=@ud                                   ::  logical wdow msgs
              maw=@ud                                   ::  max window size
              nif=@ud                                   ::  now in flight
              act=@da                                   ::  wait to send next
              nus=@ud                                   ::  number sent
          ==                                            ::
++  flow                                                ::  packet connection
          $:  rtt=@dr                                   ::  decaying avg rtt
              wid=@ud                                   ::  logical wdow msgs
          ==                                            ::
++  fort                                                ::  formal state
          $:  hop=@da                                   ::  network boot date
              ton=town                                  ::  security
              zac=(map ship oven)                       ::  flows by server
              rop=(map ,[p=@ud q=sock] riff)            ::  remote requests
          ==                                            ::
++  frog  ,[p=@da q=maki]                               ::  project change
++  gift                                                ::  one-way effect
          $%  [%% p=vase]                               ::  trivial output
              [%cd p=@tas]                              ::  change desk
              [%cs p=path]                              ::  change spur
              [%de p=@ud q=tank]                        ::  debug/level
              [%ha p=tank]                              ::  single error
              [%ho p=(list tank)]                       ::  multiple error
              [%la p=tank]                              ::  single statement
              [%lo p=(list tank)]                       ::  multiple statement
              [%mu p=type q=(list)]                     ::  batch emit
              [%mx p=(list gift)]                       ::  batch gift
              [%ok p=disc q=nori]                       ::  save changes
              [%sc p=(unit skit)]                       ::  sack library
              [%te p=(list ,@t)]                        ::  dump lines
              [%th p=@ud q=love]                        ::  http response
              [%va p=@tas q=(unit vase)]                ::  set/clear variable
              [%xx p=card]                              ::  apply card
              [%xy p=path q=card]                       ::  push card
          ==                                            ::
++  gilt  ,[@tas *]                                     ::  presumed gift
++  gens  ,[p=lang q=gcos]                              ::  general identity
++  gcos                                                ::  id description
          $%  [%czar ~]                                 ::  8-bit ship
              [%duke p=what]                            ::  32-bit ship
              [%earl p=@t]                              ::  64-bit ship
              [%king p=@t]                              ::  16-bit ship
              [%pawn p=@t]                              ::  128-bit ship
          ==                                            ::
++  goal                                                ::  app request
          $%  [%% p=type]                               ::  open for input
              [%eg p=kite]                              ::  single request
              [%es p=ship q=disc r=rave]                ::  subscription
              [%ht p=(list rout)]                       ::  http server
              [%oy p=@ta]                               ::  listen on channel
              [%up p=prod]                              ::  user prompt      
              [%wa p=@da]                               ::  alarm
              [%yo p=ship q=@ta r=*]                    ::  network message
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
              [| p=gene]                                ::  manual
          ==                                            ::
++  lane                                                ::  packet route
          $%  [%if p=@ud q=@if]                         ::  IP4/public UDP/addr
              [%is p=@ud q=(unit lane) r=@is]           ::  IPv6 w/alternates
              [%ix p=@da q=@ud r=@if]                   ::  IPv4 provisional
          ==                                            ::
++  lang  ,@ta                                          ::  IETF lang as code
++  lark  (list lath)                                   ::  parsed command
++  lass  ?(%0 %1 %2)                                   ::  power increment
++  lath  $%                                            ::  pipeline stage
              [%0 p=lass q=lamb r=(list cone) s=gene]   ::  command
              [%1 p=gene]                               ::  generator
              [%2 p=gene]                               ::  filter
          ==                                            ::
++  lens  ?(%z %y %x %w)                                ::  repository view
++  lice  ,[p=ship q=buck]                              ::  full license
++  life  ,@ud                                          ::  regime number
++  lint  (list rock)                                   ::  fragment array
++  link  ,[p=code q=sock]                              ::  connection
++  logo  ,@uvI                                         ::  session identity
++  love  $%                                            ::  http response
              [%ham p=manx]                             ::  html node
              [%mid p=mime q=octs]                      ::  mime-typed data
              [%raw p=httr]                             ::  raw http response
          ==                                            ::
++  mace  (list ,[p=life q=ring])                       ::  private secrets
++  maki                                                ::  general change
          $%  [& p=nori]                                ::  direct change
              [| p=saba]                                ::  metachange
          ==                                            ::
++  mane  $|(@tas [@tas @tas])                          ::  XML name/space
++  manx  ,[t=marx c=marl]                              ::  XML node
++  marl  (list manx)                                   ::  XML node list
++  mars  ,[t=[n=%% a=[i=[n=%% v=tape] t=~]] c=~]       ::  XML cdata
++  mart  (list ,[n=mane v=tape])                       ::  XML attributes
++  marv  ?(%da %tas %ud)                               ::  release form
++  marx  $|(@tas [n=mane a=mart])                      ::  XML tag
++  math  (map ,@t (list ,@t))                          ::  semiparsed headers
++  meal                                                ::  payload
          $%  [%back p=cape q=flap r=@dr]               ::  acknowledgment
              [%bond p=life q=@ta r=@ud s=*]            ::  message
              [%bonk p=life q=@ta r=@ud]                ::  message skip
              [%carp p=@ud q=flap r=@]                  ::  leaf fragment
              [%fore p=ship q=(unit lane) r=@]          ::  forwarded packet
          ==                                            ::
++  mess  (list ,[p=@t q=@t])                           ::  raw http headers
++  meta                                                ::  path metadata
          $%  [& q=@uvI]                                ::  hash
              [| q=(list ,@ta)]                         ::  dir
          ==                                            ::
++  meth  ?(%get %post)                                 ::  http method
++  mime  (list ,@ta)                                   ::  mime type
++  miso                                                ::  arch delta
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
++  nook                                                ::  common note
          $%  [%eg p=riot]                              ::  simple result
              [%ht p=@ud q=scab r=cred s=moth]          ::  http request
              [%ly p=newt q=tape]                       ::  lifecycle event
              [%up p=@t]                                ::  prompt response
              [%oy p=ship q=@ta r=@ud s=(unit ,*)]      ::  incoming request
              [%yo p=ship q=cape r=soap]                ::  request response
              [%wa ~]                                   ::  alarm
          ==                                            ::
++  nose                                                ::  response, kernel
          $?  [%% p=(unit ,[p=typo q=(list)])]          ::  standard input
              nook                                      ::  
          ==                                            ::
++  note                                                ::  response, kernel
          $?  [%% p=(unit ,[p=type q=(list)])]          ::  standard input
              nook                                      ::
          ==                                            ::
++  nori                                                ::  repository action
          $%  [& p=soba]                                ::  delta
              [| p=@tas]                                ::  label
          ==                                            ::
++  octs  ,[p=@ud q=@]                                  ::  octet-stream
++  oryx  ,@uvH                                         ::  CSRF secret
++  oven                                                ::  flow by server
          $:  hen=duct                                  ::  admin channel
              nys=(map flap bait)                       ::  packets incoming
              old=(set flap)                            ::  packets completed
              wab=(map ship bath)                       ::  relationship
          ==                                            ::
++  pact  path                                          ::  routed path
++  pail  ?(%none %warm %cold)                          ::  connection status
++  pith                                                ::  outgoing message
          $:  ski=snow                                  ::  sequence acked/sent
              wyv=(map ,@ud rock)                       ::  
          ==                                            ::
++  plan                                                ::  conversation state
          $:  ^=  sat                                   ::  statistics
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
              r=(map ,[p=* q=coal] coal)                ::  by (soft) gene
          ==                                            ::
++  quay  (map ,@t ,@t)                                 ::  parsed url query
++  quri                                                ::  request-uri
          $%  [& p=purl]                                ::  absolute
              [| p=pork q=quay]                         ::  relative
          ==                                            ::
++  race                                                ::  inbound stream
          $:  did=@ud                                   ::  filled sequence
              mis=(map ,@ud ,[p=flap q=(unit)])         ::  misordered
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
++  rede                                                ::  mirror
          $:  lim=@da                                   ::  updated to
              ask=(unit ,@da)                           ::  requested to
              pal=(list disc)                           ::  propagated to
              qyx=cult                                  ::  subscriptions
              dom=dome                                  ::  state
          ==                                            ::
++  riff  ,[p=disc q=(unit rave)]                       ::  request/desist
++  rill                                                ::  outbound stream
          $:  sed=@ud                                   ::  sent
              san=(map ,@ud duct)                       ::  outstanding
          ==                                            ::
++  rind                                                ::  request manager
          $:  nix=@ud                                   ::  request index
              bim=(map ,@ud ,[p=duct q=riff])           ::  outstanding
              fod=(map duct ,@ud)                       ::  current requests
          ==                                            ::
++  rink                                                ::  foreign state
          $:  hac=(map rump ,*)                         ::  cache
              mir=(map disc rede)                       ::  mirrors by desk
          ==                                            ::
++  riot  (unit rant)                                   ::  response/complete
++  road                                                ::  secured oneway route
          $:  exp=@da                                   ::  expiration date
              lun=(unit lane)                           ::  route to friend
              lew=will                                  ::  will of frien
          ==                                            ::
++  room                                                ::  fs per ship (new)
          $:  hun=duct                                  ::  terminal duct
              hez=(unit duct)                           ::  sync duct
              dos=(map ,@tas ,[p=cult q=dome])          ::  native projects 
              den=(map ,@tas dock)                      ::  links 
              rid=(map ship ,[p=rind q=rink])           ::  neighbors
          ==                                            ::
++  rock  ,@uvO                                         ::  packet
++  rout  ,[p=(list host) q=path r=oryx s=path]         ::  http route (new)
++  rump  ,[p=care q=case r=@tas s=path]                ::  relative path
++  saba  ,[p=ship q=@tas r=moar s=(list maki)]         ::  patch/merge
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
++  shed                                                ::  packet pump
          $:  $:  niq=@ud                               ::  count in queue
                  nif=@ud                               ::  count in flight
                  cop=@ud                               ::  count superlate
                  cux=@ud                               ::  total retransmits
              ==                                        ::
              puq=(qeu ,[p=@ud q=bird])                 ::  queue
          ==                                            ::
++  skit  ,[p=(unit ,@ta) q=(list ,@ta) r=(list ,@ta)]  ::  tracking path
++  sink                                                ::  incoming per server
          $:  nes=(map flap ,[p=@da q=bait])            ::  fragment actions
          ==                                            ::
++  skin  ?(%none %open %fast %full)                    ::  encoding stem
++  slip  ,[p=bell q=goal]                              ::  traceable request
++  sled  ,[p=* q=*]                                    ::  [data code]
++  snow  ,[p=@ud q=@ud r=(set ,@ud)]                   ::  window exceptions
++  soap  ,[p=[p=life q=life] q=@tas r=@ud]             ::  statement id
++  sock  ,[p=ship q=ship]                              ::  from to
++  spur  path                                          ::  modeshipdeskcasespur
++  step  ,[p=bray q=gens r=pass]                       ::  identity stage
++  tart  _|+([@da path note] *bowl)                    ::  process core
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
++  typo  ,*                                            ::  presumed type
++  soba  (list ,[p=path q=miso])                       ::  delta
++  wand  (list ,[p=life q=ring r=acro])                ::  mace in action
++  what                                                ::  logical identity
          $%  [%anon ~]                                 ::  anonymous
              [%crew p=corp]                            ::  business    <>
              [%dept p=corp]                            ::  government  <<>>
              [%fair p=corp]                            ::  nonprofit   ><
              [%home p=corp]                            ::  family      -()-
              [%holy p=corp]                            ::  religious   ||
              [%lady p=whom]                            ::  female person ()
              [%lord p=whom]                            ::  male person []
              [%punk p=@t]                              ::  opaque handle ""
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
