!:
::          %ford, encryption and misc libs.  This file is in the public domain.
::
=>
  |%
  ++  fo                                                ::  modulo prime
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
  ++  fu                                                ::  modulo (mul p q)
    |=  a=[p=@ q=@]
    =+  b=(~(inv fo p.a) (~(sit fo p.a) q.a))
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
    ++  out                                             ::  garner's formula
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
  ::::
  ::
  ++  egcd                                              ::  schneier's egcd
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
  --
=>
  |%
  ++  og                                                ::  shax-powered rng
    |_  a=@
    ++  rad                                             ::  random in range
      |=  b=@  ^-  @
      =+  c=(raw (met 0 b)) 
      ?:((lth c b) c $(a +(a)))
    ::
    ++  raw                                             ::  random bits
      |=  b=@  ^-  @
      %+  can
        0
      =+  c=(shas %og-a (mix b a))
      |-  ^-  (list ,[@ @])
      ?:  =(0 b)
        ~
      =+  d=(shas %og-b (mix b (mix a c)))
      ?:  (lte b 256)
        [[b (end 0 b d)] ~]
      [[256 d] $(c d, b (sub b 256))]
    --
  ::::
  ::
  ++  shad  |=(ruz=@ (shax (shax ruz)))                 ::  double sha-256
  ++  shaf                                              ::  half sha-256
    |=  [sal=@ ruz=@]
    =+  haz=(shas sal ruz)
    (mix (end 7 1 haz) (rsh 7 1 haz))
  ::
  ++  shak                                              ::  XX shd be PBKDF
    |=  [who=@p wud=@]
    (shas (mix %shak who) wud)
  ::
  ++  sham                                              ::  noun hash
    |=  yux=*  ^-  @uvI  ^-  @
    ?@  yux
      (shax yux)
    (mix (dec (lsh 8 1 1)) (jam yux))
  ::
  ++  shas                                              ::  salted hash
    |=  [sal=@ ruz=@] 
    (shax (mix sal (shax ruz)))
  ::
  ++  shaw                                              ::  hash to nbits
    |=  [sal=@ len=@ ruz=@]
    (~(raw og (shas sal (mix len ruz))) len)
  --
=>
  |%
  ++  pram                                              ::  r-m primality
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
  ++  ramp                                            ::  make r-m prime
    |=  [a=@ b=(list ,@) c=@]  ^-  @ux                ::  [bits snags seed]
    =>  .(c (shas %ramp c))
    =+  d=@
    |-
    ?:  =((mul 100 a) d)
      ~|(%ar-ramp !!)
    =+  e=(~(raw og c) a)
    ?:  &(|-(?~(b & &(!=(1 (mod e i.b)) $(b +.b)))) (pram e))
      e
    $(c +(c), d (shax d))
  --
=>
  |%
  ::::
  ::
  ++  crya                                              ::  cryptosuite A (RSA)
    ^-  acro
    =+  [mos=@ pon=*(unit ,[p=@ q=@ r=[p=@ q=@] s=_*fu])]
    =>  |%
        ++  dap                                         ::  OEAP decode
          |=  [wid=@ xar=@ dog=@]  ^-  [p=@ q=@]
          =+  pav=(sub wid xar)
          =+  qoy=(cut 0 [xar pav] dog)
          =+  dez=(mix (end 0 xar dog) (shaw %pad-b xar qoy))
          [dez (mix qoy (shaw %pad-a pav dez))]
        ::
        ++  pad                                         ::  OEAP encode
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
  ++  brew                                              ::  create keypair
    |=  [a=@ b=@]                                       ::  width seed
    ^-  acro
    (pit:nu:crya a b)
  ::
  ++  hail                                              ::  activate public key
    |=  a=pass
    ^-  acro
    =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
    ?>  =('a' mag)
    (com:nu:crya bod)
  ::
  ++  wear                                              ::  activate secret key
    |=  a=ring
    ^-  acro
    =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
    ?>  =('A' mag)
    (nol:nu:crya bod)
  ::::
  ::
  ++  test-rsa
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
  --  
=>  
  |%
  ++  moon                                              ::  mime type to text
    |=  myn=mime
    %+  rap
      3
    |-  ^-  tape
    ?~  myn  ~
    ?~  t.myn  (trip i.myn)
    (weld (trip i.myn) ['/' $(myn t.myn)])
  ::
  ++  tact                                              ::  tape to octstream
    |=  tep=tape  ^-  octs
    =+  buf=(rap 3 tep)
    [(met 3 buf) buf]
  ::
  ++  txml                                              ::  string to xml
    |=  tep=tape  ^-  manx
    [[%% [%% tep] ~] ~]
  ::
  ++  xmla                                              ::  attributes to tape
    |=  [tat=mart rez=tape]
    ^-  tape
    ?~  tat  rez
    %=    $
      tat  t.tat
      rez  ;:  weld
             (xmln n.i.tat)
             "=\""
             (xmle v.i.tat '"' ?~(t.tat rez [' ' rez]))
           ==
    ==
  ::
  ++  xmle                                              ::  escape for xml
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
  ++  xmln                                              ::  name to tape
    |=  man=mane  ^-  tape
    ?@  man  (trip man) 
    (weld (trip -.man) [':' (trip +.man)])
  ::
  ++  xmll                                              ::  nodes to tape
    |=  [lix=(list manx) rez=tape]
    =+  xil=(flop lix)
    |-  ^-  tape
    ?~  xil  rez
    $(xil t.xil, rez (xmlt i.xil rez))
  ::
  ++  xmlt                                              ::  node to tape
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
  --
=>
  |%
  ++  bag                                               ::  map of stacks
    |*  [a=_,* b=_,*]
    $|  ~ 
    $:  n=[p=a q=(list b)]
        l=(bag a b)
        r=(bag a b)
    ==
  ::
  ++  on
    |_  a=(bag)
    +-  ayl                                             ::  alter left
      |=  b=_a
      ^+  a
      ?~  b  [n.a ~ r.a]
      ?:  (vor p.n.a p.n.b)
        [n.a b r.a]
      [n.b l.b [n.a r.b r.a]]
    ::
    +-  ayr                                             ::  alter right
      |=  b=_a
      ?~  b  [n.a l.a ~]
      ?:  (vor p.n.a p.n.b)
        [n.a l.a b]
      [n.b [n.a l.a l.b] r.b]
    ::
    +-  get                                             ::  extract stack
      |=  b=_nam
      ^+  sac
      ?~  a  ~
      ?:  =(b p.n.a)  q.n.a
      ?:((gor b p.n.a) $(a l.a) $(a r.a))
    ::
    +-  nam  ?>(?=(^ a) p.n.a)
    +-  pop                                             ::  pop
      |=  b=_nam
      ^-  [(unit ,_val) _a]
      ?~  a  [~ ~]
      ?:  =(b p.n.a)
        ?>  ?=(^ q.n.a)
        [[~ i.q.n.a] [[b t.q.n.a] l.a r.a]]
      ?:  (gor b p.n.a)
        =+  new=$(a l.a)
        [-.new (ayl +.new)]
      =+  new=$(a r.a)
      [-.new (ayr +.new)]
    ::
    +-  put                                             ::  install stack
      |=  [b=_nam c=_val]
      ^+  a
      ?~  a  [[b [c ~]] ~ ~]
      ?:  =(b p.n.a)
        [[b [c q.n.a]] l.a r.a]
      ?:  (gor b p.n.a)
        (ayl $(a l.a))
      (ayr $(a r.a))
    ::
    +-  psh                                             ::  push
      |=  [b=_nam c=_val]
      ^+  a
      ?~  a  [[b [c ~]] ~ ~]
      ?:  =(b p.n.a)
        [[b [c q.n.a]] l.a r.a]
      ?:  (gor b p.n.a)
        (ayl $(a l.a))
      (ayr $(a r.a))
    ::
    +-  sac  ?>(?=(^ a) q.n.a)
    +-  val  ?>(?=(^ a) ?>(?=(^ q.n.a) i.q.n.a))
    -- 
  --
=>
  |%
  ++  deft                                              ::  import url path
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
  ++  epur                                              ::  url/header parser
    |%
    ++  apat  (cook deft ;~(pfix fas (more fas smeg)))  ::  2396 abs_path
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
    ++  bite                                            ::  cookies (ours)
      (most sem ;~(plug nuck:so ;~(pfix sem nuck:so))) 
    ++  dlab                                            ::  2396 domainlabel
      %+  sear
        |=  a=@ta
        ?.(=('-' (rsh 3 a (dec (met 3 a)))) [~ u=a] ~)
      %+  cook  cass
      ;~(plug aln (star alp))
    ::
    ++  fque  (cook crip (plus pquo))                   ::  normal query field
    ++  pcar  ;~(pose pure pesc psub col pat)           ::  2396 path char
    ++  pesc  ;~(pfix cen mes)                          ::  2396 escaped
    ++  pold  (cold ' ' (just '+'))                     ::  old space code
    ++  pque  ;~(pose pcar fas wut)                     ::  3986 query char
    ++  pquo  ;~(pose pure pesc pold)                   ::  normal query char
    ++  pure  ;~(pose aln hep dot cab sig)              ::  2396 unreserved
    ++  psub  ;~  pose                                  ::  3986 sub-delims
                zap  buc  pam  soq  pel  per 
                tar  lus  com  sem  tis
              ==
    ++  scem                                            ::  2396 scheme
      %+  cook  cass
      ;~(plug alf (star ;~(pose aln lus hep dot)))
    ::
    ++  smeg  (cook crip (plus pcar))                   ::  2396 segment
    ++  thor                                            ::  2396 host/port
      %+  cook  |*(a=[* *] [+.a -.a])
      ;~  plug
        thos
        ;~(pose (stag ~ ;~(pfix col dim:ag)) (easy ~))
      ==
    ++  thos                                            ::  2396 host, no local
      ;~  plug
        ;~  pose
          %+  stag  %&
          %+  sear                                      ::  LL parser weak here
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
    ++  yque                                            ::  query ending
      ;~  pose
        ;~(pfix wut yquy)
        (easy ~)
      ==
    ++  yquy                                            ::  query
      %+  cook
        |=  a=(list ,[p=@t q=@t])
        (~(gas by *(map ,@t ,@t)) a)
      ;~  pose                                          ::  proper query
        %+  more
          ;~(pose pam sem)
        ;~(plug fque ;~(pfix tis fque))
      ::
        %+  cook                                        ::  funky query
          |=(a=tape [[%% (crip a)] ~])
        (star pque)
      ==
    ++  zest                                            ::  2616 request-uri
      ;~  pose
        (stag %& (cook |=(a=purl a) auri))
        (stag %| ;~(plug apat yque))
      ==
    --
  ++  ergo                                              ::  eat headers
    |=  hed=(list ,[p=@t q=@t])
    =+  mah=*math
    |-  ^-  math
    ?~  hed  mah
    =+  cus=(cass (rip 3 p.i.hed))
    =+  zeb=(~(get by mah) cus)
    $(hed t.hed, mah (~(put by mah) cus ?~(zeb [q.i.hed ~] [q.i.hed u.zeb])))
  ::
  ++  hone                                              ::  host match
    |=  [fro=host too=host]  ^-  ?
    ?-    -.fro
        |  =(too fro)
        &
      ?&  ?=(& -.too)
          |-  ^-  ?
          ?~  p.too  &
          ?~  p.fro  |
          ?:  !=(i.p.too i.p.fro)  |
          $(p.too t.p.too, p.fro t.p.fro)
      ==
    ==
  ::
  ++  thin                                              ::  parse headers
    |=  [sec=? req=httq]
    ^-  hate
    ::  ~&  [%thin-quri (trip q.req)]
    =+  ryp=`quri`(rash q.req zest:epur)
    =+  mah=(ergo r.req)
    =+  ^=  pul  ^-  purl
        ?-  -.ryp
          &  ?>(=(sec p.p.p.ryp) p.ryp)
          |  =+  hot=(~(get by mah) %host)
             ?>  ?=([~ @ ~] hot)
             [[sec (rash i.u.hot thor:epur)] p.ryp q.ryp]
        ==
    [pul *cred [p.req mah s.req]]
  --
=>
  |%
  ++  adit                                              ::  hose privilege
    |=  hen=hose
    ^-  ?(%gold %iron %lead)
    ?~  hen
      %lead
    ?~  t.hen
      ?:  ?=([%gold *] i.hen)  %gold
      ?:  ?=([%iron *] i.hen)  %iron
      %lead
    $(hen t.hen)
  --
.
