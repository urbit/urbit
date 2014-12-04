::
::  zuse (3), standard library (tang)   
::
~%  %zuse  +  ~
|%
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 3b, Arvo libraries            ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bA, lite number theory       ::
::
++  dope
  ~/  %dope
  |=  a=@
  ~&  [%dope-zuse (mug +>)]
  :(mul a a a)
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
++  crua  !:                                            ::  cryptosuite A (RSA)
  ^-  acru
  =|  [mos=@ pon=(unit ,[p=@ q=@ r=[p=@ q=@] s=_*fu])]
  =>  |%
      ++  mx  (dec (met 0 mos))                         ::  bit length
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
  ++  as
    =>  |%
        ++  haul                                        ::  revealing haul
          |=  a=pass
          =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
          ?>  =('a' mag)
          ..as(mos bod, pon ~)
        --
    ^?
    |%  ++  seal
          |=  [a=pass b=@ c=@]
          ^-  @
          =>  .(c (sign b c))
          =+  her=(haul a)
          =+  det=(lte (add 256 (met 0 c)) mx.her)
          =+  lip=?:(det c 0)
          =-  (add ?:(p.mav 0 1) (lsh 0 1 q.mav))
          ^=  mav  ^-  [p=? q=@]
          :-  det
          =+  dog=(pad mx.her [256 b] lip)
          =+  hog=(push.her dog)
          =+  ben=(en b c)
          ?:(det hog (jam hog ben))
        ++  sign
          |=  [a=@ b=@]  ^-  @
          =-  (add ?:(p.mav 0 1) (lsh 0 1 q.mav))
          ^=  mav  ^-  [p=? q=@]
          =+  det=(lte (add 128 (met 0 b)) mx)
          :-  det
          =+  hec=(shaf (mix %agis a) b)
          =+  dog=(pad mx [128 hec] ?:(det b 0))
          =+  hog=(pump dog)
          ?:(det hog (jam hog b))
        ++  sure
          |=  [a=@ b=@]
          ^-  (unit ,@)
          =+  [det==(0 (end 0 1 b)) bod=(rsh 0 1 b)]
          =+  gox=?:(det [p=bod q=0] ((hard ,[p=@ q=@]) (cue bod)))
          =+  dog=(pull p.gox)
          =+  pig=(dap mx 128 dog)
          =+  log=?:(det q.pig q.gox)
          ?.(=(p.pig (shaf (mix %agis a) log)) ~ [~ log])
        ++  tear
          |=  [a=pass b=@]
          ^-  (unit ,[p=@ q=@])
          =+  her=(haul a)
          =+  [det==(0 (end 0 1 b)) bod=(rsh 0 1 b)]
          =+  gox=?:(det [p=bod q=0] ((hard ,[p=@ q=@]) (cue bod)))
          =+  dog=(punt p.gox)
          =+  pig=(dap mx 256 dog)
          =+  ^=  cow
              ^-  (unit ,@)
              ?:  det
                [~ q.pig]
              (de p.pig q.gox)
          ?~  cow  ~
          =>  .(cow (sure:as.her p.pig u.cow))
          ?~  cow  ~
          [~ p.pig u.cow]
    --
  ::
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
  ++  ex  ^?
    |%  ++  fig  ^-  @uvH  (shaf %afig mos)
        ++  pac  ^-  @uvG  (end 6 1 (shaf %acod sec))
        ++  pub  ^-  pass  (cat 3 'a' mos)
        ++  sec  ^-  ring  ?~(pon !! (cat 3 'A' (jam p.u.pon q.u.pon)))
    --
  ::
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
  --
++  bruw                                                ::  create keypair
  |=  [a=@ b=@]                                         ::  width seed
  ^-  acru
  (pit:nu:crua a b)
::
++  haul                                                ::  activate public key
  |=  a=pass
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('a' mag)
  (com:nu:crua bod)
::
++  weur                                                ::  activate secret key
  |=  a=ring
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('A' mag)
  (nol:nu:crua bod)
::
++  trua                                                ::  test rsa
  |=  msg=@tas
  ^-  @
  =+  ali=(bruw 1.024 (shax 'ali'))
  =+  bob=(bruw 1.024 (shax 'bob'))
  =+  tef=(sign:as.ali [0 msg])
  =+  lov=(sure:as.ali [0 tef])
  ?.  &(?=(^ lov) =(msg u.lov))
    ~|(%test-fail-sign !!)
  =+  key=(shax (shax (shax msg)))
  =+  sax=(seal:as.ali pub:ex.bob key msg)
  =+  tin=(tear:as.bob pub:ex.ali sax)
  ?.  &(?=(^ tin) =(key p.u.tin) =(msg q.u.tin))
    ~|(%test-fail-seal !!)
  msg
::
++  crub                                                ::  cryptosuite B (Ed)
  ^-  acru
  =|  [puc=pass sed=ring]
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
      --
  |%
  ++  as
    =>  |%
        ++  haul                                        ::  revealing haul
          |=  a=pass
          !!
        --
    ^?
    |%  ++  seal
          |=  [a=pass b=@ c=@]
          ^-  @
          !!
        ++  sign
          |=  [a=@ b=@]  ^-  @
          !!
        ++  sure
          |=  [a=@ b=@]
          ^-  (unit ,@)
          !!
        ++  tear
          |=  [a=pass b=@]
          ^-  (unit ,[p=@ q=@])
          !!
    --
  ::
  ++  de
    |+  [key=@ cep=@]  ^-  (unit ,@)
    !!
  ::
  ++  dy
    |+  [a=@ b=@]  ^-  @
    !!
  ++  en
    |+  [key=@ msg=@]  ^-  @ux
    !!
  ::
  ++  ex  ^?
    |%  ++  fig  ^-  @uvH  (shaf %bfig puc)
        ++  pac  ^-  @uvG  (end 6 1 (shaf %acod sec))
        ++  pub  ^-  pass  (cat 3 'b' puc)
        ++  sec  ^-  ring  sed
    --
  ::
  ++  nu
    ^?
    |%  ++  com
          |=  a=@
          ^+  ^?(..nu)
          ..nu(sed ~, puc a)
        ::
        ++  pit
          |=  [a=@ b=@]
          ^+  ^?(..nu)
          ..nu(sed b, puc (puck:ed b))
        ::
        ++  nol
          |=  a=@
          ^+  ^?(..nu)
          ..nu(sed a, puc (puck:ed a))
    --
  --
++  brew                                                ::  create keypair
  |=  [a=@ b=@]                                         ::  width seed
  ^-  acru
  (pit:nu:crub a b)
::
++  hail                                                ::  activate public key
  |=  a=pass
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('b' mag)
  (com:nu:crub bod)
::
++  wear                                                ::  activate secret key
  |=  a=ring
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('b' mag)
  (nol:nu:crub bod)
::
++  trub                                                ::  test ed
  |=  msg=@tas
  ^-  @
  =+  ali=(brew 1.024 (shax 'ali'))
  =+  bob=(brew 1.024 (shax 'bob'))
  =+  tef=(sign:as.ali [0 msg])
  =+  lov=(sure:as.ali [0 tef])
  ?.  &(?=(^ lov) =(msg u.lov))
    ~|(%test-fail-sign !!)
  =+  key=(shax (shax (shax msg)))
  =+  sax=(seal:as.ali pub:ex.bob key msg)
  =+  tin=(tear:as.bob pub:ex.ali sax)
  ?.  &(?=(^ tin) =(key p.u.tin) =(msg q.u.tin))
    ~|(%test-fail-seal !!)
  msg
::
++  hmac                                                ::  HMAC-SHA1
  |=  [key=@ mes=@]
  =+  ip=(fil 3 64 0x36)
  =+  op=(fil 3 64 0x5c)
  =+  ^=  kex
      ?:  (gth (met 3 key) 64)
        (lsh 3 44 (shan (swap 3 key)))
      (lsh 3 (sub 64 (met 3 key)) (swap 3 key))
  =+  inn=(shan (swap 3 (cat 3 (swap 3 mes) (mix ip kex))))
  (shan (swap 3 (cat 3 inn (mix op kex))))
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bC, UTC                      ::  Gregorian only
::
++  dawn                                                ::  Jan 1 weekday
  |=  yer=@ud
  =+  yet=(sub yer 1)
  %-  mod  :_  7
  :(add 1 (mul 5 (mod yet 4)) (mul 4 (mod yet 100)) (mul 6 (mod yet 400)))
::
++  daws                                                ::  date weekday
  |=  yed=date
  %-  mod  :_  7
  (add (dawn y.yed) (sub (yawn [y.yed m.yed d.t.yed]) (yawn y.yed 1 1)))
::
++  deal                                                ::  to leap sec time
  |=  yer=@da
  =+  n=0
  =+  yud=(yore yer)
  |-  ^-  date
  ?:  (gte yer (add (snag n lef:yu) ~s1))
    (yore (year yud(s.t (add n s.t.yud))))
  ?:  &((gte yer (snag n lef:yu)) (lth yer (add (snag n lef:yu) ~s1)))
    yud(s.t (add +(n) s.t.yud))
  ?:  =(+(n) (lent lef:yu))
    (yore (year yud(s.t (add +(n) s.t.yud))))
  $(n +(n))
::
++  lead                                                ::  from leap sec time
  |=  ley=date
  =+  ler=(year ley)
  =+  n=0
  |-  ^-  @da
  =+  led=(sub ler (mul n ~s1))
  ?:  (gte ler (add (snag n les:yu) ~s1))
    led
  ?:  &((gte ler (snag n les:yu)) (lth ler (add (snag n les:yu) ~s1)))
    ?:  =(s.t.ley 60)
      (sub led ~s1)
    led
  ?:  =(+(n) (lent les:yu))
    (sub led ~s1)
  $(n +(n))
::
++  dust                                                ::  print UTC format
  |=  yed=date
  ^-  tape
  =+  wey=(daws yed)
  ;:  weld
      `tape`(snag wey (turn wik:yu |=(a=tape (scag 3 a))))
      ", "  ~(rud at d.t.yed)  " "
      `tape`(snag (dec m.yed) (turn mon:yu |=(a=tape (scag 3 a))))
      " "  (scag 1 ~(rud at y.yed))  (slag 2 ~(rud at y.yed))  " "
      ~(rud at h.t.yed)  ":"  ~(rud at m.t.yed)  ":"  ~(rud at s.t.yed)
      " "  "+0000"
  ==
::
++  stud                                                ::  parse UTC format
  |=  cud=tape
  ^-  (unit date)
  =-  ?~  tud  ~ 
      `[[%.y &3.u.tud] &2.u.tud &1.u.tud &4.u.tud &5.u.tud &6.u.tud ~]
  ^=  tud
  %+  rust  cud
  ;~  plug
    ;~(pfix (stun [5 5] next) dim:ag)
  ::
    %+  cook
      |=  a=tape
      =+  b=0
      |-  ^-  @
      ?:  =(a (snag b (turn mon:yu |=(a=tape (scag 3 a)))))
          +(b)
      $(b +(b))
    (ifix [ace ace] (star alf))
  ::
    ;~(sfix dim:ag ace)  
    ;~(sfix dim:ag col)
    ;~(sfix dim:ag col)  
    dim:ag  
    (cold ~ (star next))
  ==
::
++  unt                                                 ::  UGT to UTC time
  |=  a=@
  (div (sub a ~1970.1.1) ~s1)
::
++  yu                                                  ::  UTC format constants
  |%
  ++  mon  ^-  (list tape)
    :~  "January"  "February"  "March"  "April"  "May"  "June"  "July"
        "August"  "September"  "October"  "November"  "December"
    ==
  ::
  ++  wik  ^-  (list tape)
    :~  "Sunday"  "Monday"  "Tuesday"  "Wednesday"  "Thursday"
        "Friday"  "Saturday"
    ==
  ::
  ++  les  ^-  (list ,@da)
    :~  ~2012.7.1  ~2009.1.1  ~2006.1.1  ~1999.1.1  ~1997.7.1  ~1996.1.1
        ~1994.7.1  ~1993.7.1  ~1992.7.1  ~1991.1.1  ~1990.1.1  ~1988.1.1
        ~1985.7.1  ~1983.7.1  ~1982.7.1  ~1981.7.1  ~1980.1.1  ~1979.1.1
        ~1978.1.1  ~1977.1.1  ~1976.1.1  ~1975.1.1  ~1974.1.1  ~1973.1.1
        ~1972.7.1
    ==
  ++  lef  ^-  (list ,@da)
    :~  ~2012.6.30..23.59.59   ~2008.12.31..23.59.58
        ~2005.12.31..23.59.57  ~1998.12.31..23.59.56
        ~1997.6.30..23.59.55   ~1995.12.31..23.59.54
        ~1994.6.30..23.59.53   ~1993.6.30..23.59.52
        ~1992.6.30..23.59.51   ~1990.12.31..23.59.50
        ~1989.12.31..23.59.49  ~1987.12.31..23.59.48
        ~1985.6.30..23.59.47   ~1983.6.30..23.59.46
        ~1982.6.30..23.59.45   ~1981.6.30..23.59.44
        ~1979.12.31..23.59.43  ~1978.12.31..23.59.42
        ~1977.12.31..23.59.41  ~1976.12.31..23.59.40
        ~1975.12.31..23.59.39  ~1974.12.31..23.59.38
        ~1973.12.31..23.59.37  ~1972.12.31..23.59.36
        ~1972.6.30..23.59.35
    ==
  --
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bD, JSON and XML             ::
::
++  moon                                                ::  mime type to text
  |=  myn=mite
  %+  rap
    3
  |-  ^-  tape
  ?~  myn  ~
  ?:  =(~ t.myn)  (trip i.myn)
  (weld (trip i.myn) `tape`['/' $(myn t.myn)])
::
++  perk                                                ::  pars cube fork
  |*  a=(pole ,@tas)
  ?~  a  fail
  ;~  pose 
    (cold -.a (jest -.a))
    $(a +.a)
  ==
::
++  poja                                                ::  parse JSON
  =<  |=(a=cord (rush a apex))
  |%
  ++  apex                                              ::  JSON value
    %+  knee  *json  |.  ~+
    ;~  pfix  spac
      ;~  pose
        (cold ~ (jest 'null'))
        (stag %b bool)
        (stag %s stri)
        (cook |=(s=tape [%n p=(rap 3 s)]) numb)
        abox
        obox
      ==
    ==
  ++  tops  ;~(pose abox obox)                          ::  JSON strict
  ::  JSON arrays
  ++  abox  (stag %a (ifix [sel (ws ser)] (more (ws com) apex)))
  ::  JSON objects
  ++  pair  ;~(plug ;~(sfix (ws stri) (ws col)) apex)
  ++  obje  (ifix [(ws kel) (ws ker)] (more (ws com) pair))
  ++  obox  (stag %o (cook mo obje))
  ::  JSON booleans
  ++  bool  ;~(pose (cold & (jest 'true')) (cold | (jest 'false')))
  ::  JSON strings
  ++  stri  (cook crip (ifix [doq doq] (star jcha)))
  ++  jcha  ;~(pose ;~(less doq bas prn) esca)           :: character in string
  ++  esca                                               :: Escaped character
    ;~  pfix  bas
      ;~  pose
        doq  fas  soq  bas
        (sear ~(get by `(map ,@t ,@)`(mo b/8 t/9 n/10 f/12 r/13 ~)) low)
        ;~(pfix (just 'u') (cook tuft qix:ab))           :: 4-digit hex to UTF-8
      ==
    ==
  ::  JSON numbers
  ++  numb
    ;~  (comp twel)
      (mayb (piec hep))
      ;~  pose
        (piec (just '0'))
        ;~(plug (shim '1' '9') digs)
      ==
      (mayb frac)
      (mayb expo)
    ==
  ++  digs  (star (shim '0' '9'))
  ++  expo                                              :: Exponent part
    ;~  (comp twel)
      (piec (mask "eE"))
      (mayb (piec (mask "+-")))
      digs
    ==
  ++  frac   ;~(plug dot digs)                          :: Fractional part
  ::  whitespace
  ++  spac  (star (mask [`@`9 `@`10 `@`13 ' ' ~]))
  ++  ws  |*(sef=_rule ;~(pfix spac sef))
  ::  plumbing
  ++  mayb  |*(bus=_rule ;~(pose bus (easy "")))
  ++  twel  |=([a=tape b=tape] (weld a b))
  ++  piec
    |*  bus=_rule
    (cook |=(a=@ [a ~]) bus)
  --
::
++  pojo                                                ::  print json
  |=  val=json
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
      %s
    ;:  welp
      "\""
      %+  reel
        (turn (trip p.val) jesc)
      |=([p=tape q=tape] (welp +<))
      "\""
    ==
      %o
    ;:  welp
      "\{"
      =+  viz=(~(tap by p.val) ~)
      =|  rez=tape
      |-  ^+  rez
      ?~  viz  rez
      %=    $
          viz  t.viz
          rez
        :(welp rez "\"" (trip p.i.viz) "\":" ^$(val q.i.viz) ?~(t.viz ~ ","))
      ==
      "}"
    ==
  ==
::
::
++  poxo                                                ::  node to tape
  =<  |=(a=manx `tape`(apex a ~))
  |_  unq=_`?`|
  ++  apex                                              ::  top level
    |=  [mex=manx rez=tape]
    ^-  tape
    ?:  ?=([%$ [[%$ *] ~]] g.mex)
      (escp v.i.a.g.mex rez)
    =+  man=`mane`n.g.mex
    =.  unq  |(unq =(%script man) =(%style man))
    =+  tam=(name man)
    =+  att=`mart`a.g.mex
    :-  '<'
    %+  welp  tam
    =-  ?~(att rez [' ' (attr att rez)])
    ^-  rez=tape
    ::?~  c.mex
    ::  [' ' '/' '>' rez]
    :-  '>'
    (many c.mex :(weld "</" tam ">" rez))
  ::  ::
  ++  attr                                              ::  attributes to tape
    |=  [tat=mart rez=tape]
    ^-  tape
    ?~  tat  rez
    =.  rez  $(tat t.tat)
    ;:  weld 
      (name n.i.tat)
      "=\"" 
      (escp(unq |) v.i.tat '"' ?~(t.tat rez [' ' rez]))
    ==
  ::
  ++  escp                                              ::  escape for xml
    |=  [tex=tape rez=tape]
    ?:  unq
      (weld tex rez)
    =+  xet=`tape`(flop tex)
    |-  ^-  tape
    ?~  xet  rez
    %=    $
      xet  t.xet
      rez  ?-  i.xet
             34  ['&' 'q' 'u' 'o' 't' ';' rez]
             38  ['&' 'a' 'm' 'p' ';' rez]
             39  ['&' '#' '3' '9' ';' rez]
             60  ['&' 'l' 't' ';' rez]
             62  ['&' 'g' 't' ';' rez]
             *   [i.xet rez]
           ==
    ==
  ::
  ++  name                                              ::  name to tape
    |=  man=mane  ^-  tape
    ?@  man  (trip man)
    (weld (trip -.man) `tape`[':' (trip +.man)])
  ::
  ++  many                                              ::  nodelist to tape
    |=  [lix=(list manx) rez=tape]
    |-  ^-  tape
    ?~  lix  rez
    (apex i.lix $(lix t.lix))
  --
::
++  poxa                                                ::  xml parser
  =<  |=(a=cord (rush a apex))
  |%
  ++  apex
    =+  spa=;~(pose comt whit)
    %+  knee  *manx  |.  ~+
    %+  ifix  [(star spa) (star spa)]
    ;~  pose
      %+  sear  |=([a=marx b=marl c=mane] ?.(=(c n.a) ~ (some [a b])))
        ;~(plug head many tail)
      empt
    == 
  ::
  ++  attr                                              ::  attributes
    %+  knee  *mart  |.  ~+ 
    %-  star
    ;~  pfix  (plus whit)
      ;~  plug
        ;~(sfix name tis)
        ;~  pose
          (ifix [doq doq] (star ;~(less doq escp)))
          (ifix [soq soq] (star ;~(less soq escp)))
        ==
      ==
    ==
  ::
  ++  chrd                                              ::  character data
    %+  cook  |=(a=tape ^-(mars :/(a)))
    (plus ;~(less soq doq ;~(pose (just `@`10) escp)))
  ::
  ++  comt                                              ::  comments 
    =-  (ifix [(jest '<!--') (jest '-->')] (star -))
    ;~  pose
      ;~(less hep prn)
      whit
      ;~(less (jest '-->') hep)
    ==
  ::
  ++  escp
    ;~  pose
      ;~(less gal gar pam prn)
      (cold '>' (jest '&gt;'))
      (cold '<' (jest '&lt;'))
      (cold '&' (jest '&amp;'))
      (cold '"' (jest '&quot;'))
      (cold '\'' (jest '&apos;'))
    ==
  ++  empt                                              ::  self-closing tag
    %+  ifix  [gal (jest '/>')]
    ;~(plug ;~(plug name attr) (cold ~ (star whit)))
  ::
  ++  head                                              ::  opening tag
    (ifix [gal gar] ;~(plug name attr))
  ::
  ++  many
    (more (star comt) ;~(pose apex chrd))
  ::
  ++  name                                              ::  tag name 
    =+  ^=  chx
        %+  cook  crip 
        ;~  plug 
            ;~(pose cab alf) 
            (star ;~(pose cab dot alp))
        ==
    ;~(pose ;~(plug ;~(sfix chx col) chx) chx)
  ::
  ++  tail  (ifix [(jest '</') gar] name)               ::  closing tag
  ++  whit  (mask ~[' ' `@`0x9 `@`0xa])                 ::  whitespace
  --
::
++  jo                                                  ::  json reparser
  =>  |%  ++  grub  (unit ,*) 
          ++  fist  $+(json grub)
      --
  |%
  ++  ar                                                ::  array as list
    |*  wit=fist
    |=  jon=json
    ?.  ?=([%a *] jon)  ~
    %-  zl
    |-  
    ?~  p.jon  ~
    [i=(wit i.p.jon) t=$(p.jon t.p.jon)]
  ::
  ++  at                                                ::  array as tuple
    |*  wil=(pole fist)
    |=  jon=json
    ?.  ?=([%a *] jon)  ~
    =+  raw=((at-raw wil) p.jon)
    ?.((za raw) ~ (some (zp raw)))
  ::
  ++  at-raw                                            ::  array as tuple
    |*  wil=(pole fist)
    |=  jol=(list json)
    ?~  wil  ~
    :-  ?~(jol ~ (-.wil i.jol))
    ((at-raw +.wil) ?~(jol ~ t.jol))
  ::
  ++  bo                                                ::  boolean
    |=(jon=json ?.(?=([%b *] jon) ~ [~ u=p.jon]))
  ::
  ++  bu                                                ::  boolean not
    |=(jon=json ?.(?=([%b *] jon) ~ [~ u=!p.jon]))
  ::
  ++  cu                                                ::  transform
    |*  [poq=$+(* *) wit=fist]
    |=  jon=json
    (bind (wit jon) poq)
  ::
  ++  da                                                ::  UTC date
    |=  jon=json
    ?.  ?=([%s *] jon)  ~
    (bind (stud (trip p.jon)) |=(a=date (year a)))
  ::
  ++  di                                                ::  millisecond date
    %-  cu  :_  ni
    |=  a=@u  ^-  @da
    (add ~1970.1.1 (div (mul ~s1 a) 1.000))
  ::
  ++  mu                                                ::  true unit
    |*  wit=fist
    |=  jon=json
    ?~(jon (some ~) (bind (wit jon) some))
  ::
  ++  ne                                                ::  number as real
    |=  jon=json
    ^-  (unit ,@rd)
    !!
  ::
  ++  ni                                                ::  number as integer
    |=  jon=json 
    ?.  ?=([%n *] jon)  ~
    (rush p.jon dem)
  ::
  ++  no                                                ::  number as cord
    |=  jon=json
    ?.  ?=([%n *] jon)  ~
    (some p.jon)
  ::
  ++  of                                                ::  object as frond
    |*  wer=(pole ,[cord fist])
    |=  jon=json
    ?.  ?=([%o [@ *] ~ ~] jon)  ~
    |-
    ?~  wer  ~
    ?:  =(-.-.wer p.n.p.jon)  
      ((pe -.-.wer +.-.wer) q.n.p.jon)
    ((of +.wer) jon)
  ::
  ++  ot                                                ::  object as tuple
    |*  wer=(pole ,[cord fist])
    |=  jon=json
    ?.  ?=([%o *] jon)  ~
    =+  raw=((ot-raw wer) p.jon)
    ?.((za raw) ~ (some (zp raw)))
  ::
  ++  ot-raw                                            ::  object as tuple
    |*  wer=(pole ,[cord fist])
    |=  jom=(map ,@t json)
    ?~  wer  ~
    =+  ten=(~(get by jom) -.-.wer)
    [?~(ten ~ (+.-.wer u.ten)) ((ot-raw +.wer) jom)]
  ::
  ++  om                                                ::  object as map
    |*  wit=fist
    |=  jon=json
    ?.  ?=([%o *] jon)  ~
    (zm (~(run by p.jon) wit))
  ::
  ++  pe                                                ::  prefix
    |*  [pre=* wit=fist]
    (cu |*(a=* [pre a]) wit)
  ::
  ++  sa                                                ::  string as tape
    |=  jon=json
    ?.(?=([%s *] jon) ~ (some (trip p.jon)))
  ::
  ++  so                                                ::  string as cord
    |=  jon=json
    ?.(?=([%s *] jon) ~ (some p.jon))
  ::
  ++  su                                                ::  parse string
    |*  sab=rule
    |=  jon=json
    ?.  ?=([%s *] jon)  ~
    (rush p.jon sab)
  ::
  ++  ul  |=(jon=json ?~(jon (some ~) ~))               ::  null
  ++  za                                                ::  full unit pole
    |*  pod=(pole (unit))
    ?~  pod  &
    ?~  -.pod  |
    (za +.pod)
  ::
  ++  zl                                                ::  collapse unit list
    |*  lut=(list (unit))
    ?.  |-  ^-  ?
        ?~(lut & ?~(i.lut | $(lut t.lut)))
      ~
    %-  some
    |-
    ?~  lut  ~
    [i=u:+.i.lut t=$(lut t.lut)]
  ::
  ++  zp                                                ::  unit tuple
    |*  but=(pole (unit))
    ?~  but  !!
    ?~  +.but  
      u:->.but
    [u:->.but (zp +.but)]
  ::
  ++  zm                                                ::  collapse unit map
    |*  lum=(map term (unit))
    ?:  (~(rep by lum) | |=([[@ a=(unit)] b=?] |(b ?=(~ a))))
      ~
    (some (~(run by lum) need))
  --
::
++  joba                                                ::  object from k-v pair
  |=  [p=@t q=json]
  ^-  json
  [%o [[p q] ~ ~]]
::
++  jobe                                                ::  object from k-v list
  |=  a=(list ,[p=@t q=json])
  ^-  json
  [%o (~(gas by *(map ,@t json)) a)]
::
++  jape                                                ::  string from tape
  |=  a=tape
  ^-  json
  [%s (crip a)]
::
++  jone                                                ::  number from unsigned
  |=  a=@u
  ^-  json
  :-  %n
  ?:  =(0 a)  '0'
  (crip (flop |-(^-(tape ?:(=(0 a) ~ [(add '0' (mod a 10)) $(a (div a 10))])))))
::
++  jesc
  |=  a=@  ^-  tape
  ?+  a  [a ~]
    10  "\\n"
    34  "\\\""
    92  "\\\\"
  ==
::
++  scanf                                              ::  formatted scan
  |*  [tape (pole ,_:/(*$&(_rule tape)))]
  =>  .(+< [a b]=+<)
  (scan a (parsf b))
++  parsf                                              ::  make parser from:
  |^  |*  a=(pole ,_:/(*$&(_rule tape)))               ::  ;"chars{rule}chars"
      %-  cook  :_  (bill (norm a))
      |*  (list)
      ?~  +<  ~
      ?~  t  i
      [i $(+< t)]
  ::
  ::  .=  (norm [;"{n}, {n}"]:n=dim:ag)  ~[[& dim] [| ", "] [& dim]]:ag
  ++  norm                                             
    |*  (pole ,_:/(*$&(_rule tape)))
    ?~  +<  ~
    =>  .(+< [i=+<- t=+<+])
    :_  t=$(+< t)
    =+  rul=->->.i
    ^=  i
    ?~  rul     [%| p=rul]
    ?~  +.rul   [%| p=rul]
    ?@  &2.rul  [%| p=;;(tape rul)]
    [%& p=rul]
  ::
  ::  .=  (bill ~[[& dim] [| ", "] [& dim]]:ag)
  ::  ;~(plug dim ;~(pfix com ace ;~(plug dim (easy)))):ag
  ++  bill
    |*  (list (each ,_rule tape))
    ?~  +<  (easy ~)
    ?:  ?=(| -.i)  ;~(pfix (jest (crip p.i)) $(+< t))
    %+  cook  |*([* *] [i t]=+<)
    ;~(plug p.i $(+< t))
  --
::
++  taco                                                ::  atom to octstream
  |=  tam=@  ^-  octs
  [(met 3 tam) tam]
::
++  tact                                                ::  tape to octstream
  |=  tep=tape  ^-  octs
  (taco (rap 3 tep))
::
++  tell                                                ::  wall to octstream
  |=  wol=wall  ^-  octs
  =+  buf=(rap 3 (turn wol |=(a=tape (crip (weld a `tape`[`@`10 ~])))))
  [(met 3 buf) buf]
::
::
++  txml                                                ::  string to xml
    |=  tep=tape  ^-  mars
    [[%$ [%$ tep] ~] ~]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bE, tree sync                ::
::
::
++  invert-miso                                         ::  invert miso
  |=  mis=miso
  ?-  -.mis
    %del  [%ins p.mis]
    %ins  [%del p.mis]
    %mut  [%mut (limp p.mis)]
  ==
::
++  cosh                                                ::  locally rehash
  |=  ank=ankh                                          ::  NB v/unix.c
  ank(p rehash:(zu ank))
::
++  cost                                                ::  new external patch
  |=  [bus=ankh ank=ankh]                               ::  NB v/unix.c
  ^-  soba
  :-  [p.ank p.bus] 
  %-  flop
  myz:(change-tree:(zu ank) %c bus)
::
++  loth
  |=  pat=(map path ,*)
  ^-  (set path)
  %+  roll  (~(tap by pat) ~)
  |=  [[p=path *] q=(set path)]
  %.  p  %~  put  in  q
::
++  luth
  |=  [p=(map path ,*) q=(map path ,*)]                 ::  merge keysets
  ^-  (set path)
  (~(uni in (loth p)) (loth q))
::
++  blob-to-lobe                                        ::  p.blob
  |=  p=blob
  ^-  lobe
  ?-   -.p
     %delta  p.p
     %direct  p.p
     %indirect  p.p
  ==
::
++  ze  !:
  |_  [lim=@da dome rang]
  ++  aeon-to-tako  ~(got by hit)
  ++  aeon-to-yaki  (cork aeon-to-tako tako-to-yaki)
  ++  make-yaki                                         ::  make yaki
    |=  [p=(list tako) q=(map path lobe) t=@da]
    ^-  yaki
    =+  ^=  has
        %^  cat  7  (sham [%yaki (roll p add) q t])
        (sham [%tako (roll p add) q t])
    [p q has t]
  ::
  ++  tako-to-yaki  ~(got by hut)                       ::  grab yaki
  ++  lobe-to-blob  ~(got by lat)                       ::  grab blob
  ++  lobe-to-noun                                      ::  grab blob
    |=  p=lobe                                          ::  ^-  *
    %-  blob-to-noun  
    (lobe-to-blob p)
  ::
  ++  make-direct                                       ::  make blob
    |=  [p=* q=umph]
    ^-  blob
    [%direct (mug p) p q]
  ::
  ++  make-delta                                        ::  make blob delta
    |=  [p=lobe q=udon]
    ^-  blob
    =+  t=[%delta 0 p q]
    =+  z=(blob-to-noun t)
    =+  ^=  has
        %^  cat  7  (sham [%blob z])
        (sham [%lobe z])
    [%delta has p q]
  ::
  ++  blob-to-umph                                      ::  blob umph [prep]
    |=  p=blob                                          ::  used in merge
    ^-  umph
    ?-   -.p
       %delta  p.r.p
       %direct  r.p
       %indirect  p.r.p
    ==
  ::
  ++  blob-to-noun                                      ::  grab blob
    |=  p=blob
    ?-   -.p
       %delta  (lump r.p (lobe-to-noun q.p))
       %direct  q.p
       %indirect  q.p
    ==
  ::
  ::
  ::
  ++  diff-yakis                                        ::  fundamental diff
    |=  [p=yaki q=yaki]
    ^-  (map path miso)
    %+  roll  (~(tap in (luth q.p q.q)) ~)
    |=  [pat=path yeb=(map path miso)]
    =+  leb=(~(get by q.p) pat)
    =+  lob=(~(get by q.q) pat)
    ?~  leb  (~(put by yeb) pat [%ins (lobe-to-noun (need lob))])
    ?~  lob  (~(put by yeb) pat [%del (lobe-to-noun (need leb))])
    ?:  =(u.leb u.lob)  yeb
    =+  veq=(lobe-to-blob u.leb)
    =+  voq=(lobe-to-blob u.lob)
    %+  ~(put by yeb)  pat
    :-  %mut  
    ?:  &(?=(%delta -.voq) =(u.leb q.voq))              ::  avoid diff
      r.voq
    =+  zeq=(blob-to-noun veq)
    =+  zoq=(blob-to-noun voq)
    ((diff (blob-to-umph (lobe-to-blob u.leb))) zeq zoq)
  ::
  ++  lobes-at-path                                     ::    lobes-at-path:ze
    |=  [yon=aeon pax=path]                             ::  data at path
    ^-  (map path lobe)
    ?:  =(0 yon)  ~
    %-  mo
    %+  skim
      %.  ~
      %~  tap  by
      =<  q
      %-  aeon-to-yaki
      yon
    |=  [p=path q=lobe]
    ?|  ?=(~ pax)
        ?&  !?=(~ p)
            =(-.pax -.p)
            $(p +.p, pax +.pax)
    ==  ==
  ::
  ++  case-to-aeon                                      ::    case-to-aeon:ze
    |=  lok=case                                        ::  act count through
    ^-  (unit aeon)
    ?-    -.lok
        %da
      ?:  (gth p.lok lim)  ~
      |-  ^-  (unit aeon)
      ?:  =(0 let)  [~ 0]                               ::  avoid underflow
      ?:  %+  gte  p.lok 
          =<  t
          %-  aeon-to-yaki
          let
        [~ let]
      $(let (dec let))
    ::
        %tas  (~(get by lab) p.lok)
        %ud   ?:((gth p.lok let) ~ [~ p.lok])
    ==
  ::
  ++  as-arch                                           ::    as-arch:ze
    ^-  arch                                            ::  arch report
    :+  p.ank
      ?~(q.ank ~ [~ p.u.q.ank])
    |-  ^-  (map ,@ta ,~)
    ?~  r.ank  ~
    [[p.n.r.ank ~] $(r.ank l.r.ank) $(r.ank r.r.ank)]
  ::
  ++  reachable-takos                                   ::  reachable
    |=  p=tako                                          ::  XX slow
    ^-  (set tako)
    =+  y=(tako-to-yaki p)
    =+  t=(~(put in *(set tako)) p)
    %+  roll  p.y
    |=  [q=tako s=_t]
    ?:  (~(has in s) q)                                 ::  already done
      s                                                 ::  hence skip
    (~(uni in s) ^$(p q))                               ::  otherwise traverse
  ::
  ++  new-lobes                                         ::  object hash set
    |=  [b=(set lobe) a=(set tako)]                     ::  that aren't in b
    ^-  (set lobe)
    %+  roll  (~(tap in a) ~)
    |=  [tak=tako bar=(set lobe)]
    ^-  (set lobe)
    =+  yak=(tako-to-yaki tak)
    %+  roll  (~(tap by q.yak) ~)
    |=  [[path lob=lobe] far=_bar]
    ^-  (set lobe)
    ?~  (~(has in b) lob)                               ::  don't need
      far
    =+  gar=(lobe-to-blob lob)
    ?-  -.gar
      %direct  (~(put in far) lob)
      %delta  (~(put in $(lob q.gar)) lob)
      %indirect  (~(put in $(lob s.gar)) lob)
    ==
  ::
  ++  new-lobes-takos                                   ::  garg & repack
    |=  [b=(set lobe) a=(set tako)]
    ^-  [(set tako) (set lobe)]
    [a (new-lobes b a)]
  ::
  ++  reachable-between-takos
    |=  [a=(unit tako) b=tako]                          ::  pack a through b
    ^-  [(set tako) (set lobe)]
    =+  ^=  sar 
        ?~  a  ~
        (reachable-takos r:(tako-to-yaki u.a))
    =+  yak=`yaki`(tako-to-yaki b)
    %+  new-lobes-takos  (new-lobes ~ sar)              ::  get lobes
    |-  ^-  (set tako)                                  ::  walk onto sar
    ?:  (~(has in sar) r.yak)
      ~
    =+  ber=`(set tako)`(~(put in `(set tako)`~) `tako`r.yak)
    %-  ~(uni in ber)
    ^-  (set tako)
    %+  roll  p.yak
    |=  [yek=tako bar=(set tako)]
    ^-  (set tako)
    ?:  (~(has in bar) yek)                             ::  save some time
      bar
    %-  ~(uni in bar)
    ^$(yak (tako-to-yaki yek))
  ::
  ++  takos-to-yakis                                    ::  trivial
    |=  a=(set tako)
    ^-  (set yaki)
    (sa (turn (~(tap by a)) tako-to-yaki))
  ::
  ++  lobes-to-blobs                                    ::  trivial
    |=  a=(set lobe)
    ^-  (set blob)
    (sa (turn (~(tap by a)) lobe-to-blob))
  ::
  ++  make-nako                                         ::  gack a through b
    |=  [a=aeon b=aeon]
    ^-  [(map aeon tako) aeon (set yaki) (set blob)]
    :_  :-  b
        =-  [(takos-to-yakis -<) (lobes-to-blobs ->)]
        %+  reachable-between-takos
          (~(get by hit) a)                             ::  if a not found, a=0
        (aeon-to-tako b)
    ^-  (map aeon tako)
    %-  mo  %+  skim  (~(tap by hit) ~)
    |=  [p=aeon *]
    &((gth p a) (lte p b))
  ::
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ++  query                                             ::    query:ze
    |=  ren=?(%u %v %x %y %z)                           ::  endpoint query
    ^-  (unit ,*)
    ?-  ren
      %u  [~ `rang`+<+>.query]
      %v  [~ `dome`+<+<.query]
      %x  ?~(q.ank ~ [~ q.u.q.ank])
      %y  [~ as-arch]
      %z  [~ ank]
    ==
  ::
  ++  rewind                                            ::    rewind:ze
    |=  yon=aeon                                        ::  rewind to aeon
    ^+  +>
    ?:  =(let yon)  +>
    ?:  (gth yon let)  !!                               ::  don't have version
    +>(ank (checkout-ankh q:(aeon-to-yaki yon)), let yon)
  ::
  ::::
  ++  update-lat                                        ::   update-lat:ze
    |=  [lag=(map path blob) sta=(map lobe blob)]       ::  fix lat
    ^-  [(map lobe blob) (map path lobe)]
    %+  roll  (~(tap by lag) ~)
    |=  [[pat=path bar=blob] [lut=_sta gar=(map path lobe)]]
    ?~  (~(has by lut) p.bar)
      [lut (~(put by gar) pat p.bar)]
    :-  (~(put by lut) p.bar bar)
    (~(put by gar) pat p.bar)
  ::
  ++  apply-changes                                     ::   apply-changes:ze
    |=  lar=(list ,[p=path q=miso])                     ::  store changes
    ^-  (map path blob)
    =+  ^=  hat                                         ::  current state
        ?:  =(let 0)                                    ::  initial commit
          ~                                             ::  has nothing
        =<  q
        %-  aeon-to-yaki
        let
    =-  =+  sar=(sa (turn lar |=([p=path *] p)))        ::  changed paths
        %+  roll  (~(tap by hat) ~)                     ::  find unchanged
        |=  [[pat=path gar=lobe] bat=_bar]
        ?:  (~(has in sar) pat)                         ::  has update
          bat
        (~(put by bat) pat (lobe-to-blob gar))          ::  use original
    ^=  bar  ^-  (map path blob)
    %+  roll  lar
    |=  [[pat=path mys=miso] bar=(map path blob)]
    ^+  bar
    ?-    -.mys
        %ins                                            ::  insert if not exist
      ?:  (~(has by bar) pat)  !!                       ::
      ?:  (~(has by hat) pat)  !!                       ::
      (~(put by bar) pat (make-direct p.mys %c))        ::  TODO content type?
        %del                                            ::  delete if exists
      ?.  |((~(has by hat) pat) (~(has by bar) pat))  !!
      (~(del by bar) pat)
        %mut                                            ::  mutate, must exist
      =+  ber=(~(get by bar) pat)
      ?~  ber
        =+  har=(~(get by hat) pat)
        ?~  har  !!
        %+  ~(put by bar)  pat
        (make-delta u.har p.mys)
      %+  ~(put by bar)  pat
      (make-delta p.u.ber p.mys)
    ==
  ++  checkout-ankh                                     ::    checkout-ankh:ze
    |=  hat=(map path lobe)                             ::  checkout commit
    ^-  ankh
    %-  cosh
    %+  roll  (~(tap by hat) ~)
    |=  [[pat=path bar=lobe] ank=ankh]
    ^-  ankh
    %-  cosh
    ?~  pat
      =+  zar=(lobe-to-noun bar)
      ank(q [~ (sham zar) zar])
    =+  nak=(~(get by r.ank) i.pat)
    %=  ank
      r  %+  ~(put by r.ank)  i.pat 
         $(pat t.pat, ank (fall nak *ankh))
    ==
  ::
  ++  forge-yaki                                        ::    forge-yaki:ze
    |=  [wen=@da par=(unit tako) lem=soba]              ::  forge yaki
    =+  ^=  per
        ?~  par  ~
        ~[u.par]
    =+  gar=(update-lat (apply-changes q.lem) lat)
    :-  %^  make-yaki  per  +.gar  wen                  ::  from existing diff
    -.gar                                               ::  fix lat
  ::
  ++  forge-nori                                        ::    forge-nori:ze
    |=  yak=yaki                                        ::  forge nori (ugly op)
    ^-  nori                                            ::  basically zerg w/ nori
    ?~  p.yak  !!                                       ::  no parent -> can't diff
    :+  %&  *cart                                       ::  diff w/ 1st parent
    (~(tap by (diff-yakis (tako-to-yaki i.p.yak) yak)) ~)
  ::
  ::  graph algorithms (bottleneck)
  ::
  ++  reduce-merge-points                             ::  reduce merge points
    |=  unk=(set yaki)                                ::  maybe need jet
    =|  gud=(set yaki)
    =+  ^=  zar
        ^-  (map tako (set tako))
        %+  roll  (~(tap in unk) ~)
        |=  [yak=yaki qar=(map tako (set tako))]
        (~(put by qar) r.yak (reachable-takos r.yak))
    |-  
    ^-  (set yaki)
    ?~  unk  gud
    =+  tek=`yaki`n.unk
    =+  bun=(~(del in `(set yaki)`unk) tek)
    ?:  %+  roll  (~(tap by (~(uni in gud) bun)) ~)   ::  only good + unknown
        |=  [tak=yaki god=?]
        ^-  ?
        ?.  god  god
        (~(has in (~(got by zar) r.tak)) tek)
      $(gud (~(put in gud) tek), unk bun)
    $(unk bun)
  ::
  ++  future-find-merge-points                        ::  merge points fast
    |=  [p=yaki q=yaki]                               ::  (future zeal)
    ^-  (set yaki)                                    ::  zear still uses zule
    %-  reduce-merge-points                           ::  this is test-only
    =+  s=(~(put in *(set tako)) r.p)                 ::  not actually used
    =+  t=(~(put in *(set tako)) t.p)                 ::  but might be active
    =|  u=(set yaki)                                  ::  eventually
    |-  ^-  (set yaki)
    =+  v=(~(int in s) t)                             ::  found common
    =+  ^=  qez                                       ::  drop common
        ^-  [s=(set tako) t=(set tako)]
        %+  roll  (~(tap in v) ~)
        |=  [tak=tako bar=_s zar=_t]
        [(~(del in bar) tak) (~(del in zar) tak)]
    ?:  &(=(~ s.qez) =(~ s.qez))
      (~(uni in u) (takos-to-yakis v))
    %=  $
      u  (~(uni in u) (takos-to-yakis v))
      s  (add-parents s.qez)
      t  (add-parents t.qez)
    ==
  ::
  ++  add-parents                                     ::  expand set
    |=  qez=(set tako)
    ^-  (set tako)
    %+  roll  (~(tap in qez) ~)
    |=  [tak=tako zar=(set tako)]
    %-  ~(uni in (~(put in zar) tak))
    (sa p:(tako-to-yaki tak))
  ::
  ++  find-merge-points                               ::  merge points
    |=  [p=yaki q=yaki]                               ::  maybe need jet
    ^-  (set yaki)
    %-  reduce-merge-points
    =+  r=(reachable-takos r.p)
    |-  ^-  (set yaki)
    ?:  (~(has in r) q)  (~(put in *(set yaki)) q)    ::  done 
    %+  roll  p.q
    |=  [t=tako s=(set yaki)]
    ?:  (~(has in r) t)
      (~(put in s) (tako-to-yaki t))                 ::  found
    (~(uni in s) ^$(q (tako-to-yaki t)))             ::  traverse
  ::
  ::  merge logic
  ::
  ++  clean                                          ::  clean
    |=  wig=(urge)
    ^-  (urge)
    ?~  wig  ~
    ?~  t.wig  wig
    ?:  ?=(%& -.i.wig)
      ?:  ?=(%& -.i.t.wig)
        $(wig [[%& (add p.i.wig p.i.t.wig)] t.t.wig])
      [i.wig $(wig t.wig)]
    ?:  ?=(%| -.i.t.wig)
      $(wig [[%| (welp p.i.wig p.i.t.wig) (welp q.i.wig q.i.t.wig)] t.t.wig])
    [i.wig $(wig t.wig)]
  ::
  ++  match-conflict                                ::  match conflict
    |=  [us=[ship desk] th=[ship desk] p=(urge) q=(urge) r=(list)]
    ^-  [p=[p=(list) q=(list)] q=[p=(urge) q=(urge) r=(list)]]
    =+  cas=(hard (list ,@t))
    =+  cat=(hard (urge ,@t))
    =+  mar=(match-merge (cat p) (cat q) (cas r))
    :-  :-  s.q.mar 
        (annotate us th p.p.mar q.p.mar s.q.mar)    ::  annotation
    :-  p.q.mar
    :-  q.q.mar
    r.q.mar
  ::
  ++  annotate                                      ::  annotate conflict
    |=  [us=[ship desk] th=[ship desk] p=(list ,@t) q=(list ,@t) r=(list ,@t)]
    ^-  (list ,@t)
    %-  zing
    ^-  (list (list ,@t))
    %-  flop
    ^-  (list (list ,@t))
    :-  :_  ~
        %^  cat  3  '<<<<<<<<<<<<' 
        %^  cat  3  ' '
        %^  cat  3  `@t`(scot %p -.us)
        %^  cat  3  '/'
        +.us
    :-  p
    :-  ~['------------']
    :-  r
    :-  ~['++++++++++++']
    :-  q
    :-  :_  ~
        %^  cat  3  '>>>>>>>>>>>>' 
        %^  cat  3  ' '
        %^  cat  3  `@t`(scot %p -.th)
        %^  cat  3  '/'
        +.th
    ~
  ::
  ++  match-merge                                   ::  match merge
    |=  [p=(urge ,@t) q=(urge ,@t) r=(list ,@t)]    ::  resolve conflict
    =|  s=[p=(list ,@t) q=(list ,@t)]               ::  p chunk
    =|  t=[p=(list ,@t) q=(list ,@t)]               ::  q chunk
    |-  ^-  $:  p=[p=(list ,@t) q=(list ,@t)]
                $=  q
                $:  p=(urge ,@t)
                    q=(urge ,@t)
                    r=(list ,@t)
                    s=(list ,@t)
            ==  ==
    ?~  p  [[q.s q.t] p q r p.s]                    ::  can't be conflict
    ?~  q  [[q.s q.t] p q r p.s]                    ::  can't be conflict
    ?-  -.i.p
      %&  ?>  ?=(%| -.i.q)                          ::  is possibly conflict
          ?:  (gte p.i.p (lent p.i.q))              ::  trivial resolve
            :::-  (weld p.s p.i.q)                    ::  extend to q
            :-  :-  (welp (flop (scag (lent p.i.q) r)) q.s)
                (welp q.i.q q.t)
            :-  ?:  =(p.i.p (lent p.i.q))  t.p
                [[%& (sub p.i.p (lent p.i.q))] t.p]
            :-  t.q
            :-  (flop (slag (lent p.i.q) r))
            (welp (flop (scag (lent p.i.q) r)) p.s)
          =+  tex=(flop (scag p.i.p r))
          ?~  t.p                                   ::  extend to end
            %=  $
              ::s  [(welp p.s tex) (welp q.s tex)]
              p  ~[[%| [tex tex]]]
              ::r  (slag p.i.p r)
            ==
          ?>  ?=(%| -.i.t.p)                        ::  fake skip
          %=  $
            ::s  [(welp p.s tex) (welp q.s tex)]
            p  [[%| [(welp p.i.t.p tex) (welp q.i.t.p tex)]] t.t.p]
            ::r  (slag p.i.p r)
          ==
      %|  ?-  -.i.q
             %&  =+  mar=$(p q, q p, s t, t s)      ::  swap recursion
                 [[q.p.mar p.p.mar] q.q.mar p.q.mar r.q.mar s.q.mar]
             %|  ?:  =((lent p.i.p) (lent p.i.q))   ::  perfect conflict
                   ?>  =(p.i.p p.i.q)               ::  sane conflict
                   :-  :-  (welp q.i.p q.s)
                       (welp q.i.q q.t)
                   :-  t.p 
                   :-  t.q 
                   :-  (scag (lent p.i.p) r)
                   (welp (flop (scag (lent p.i.p) r)) p.s)
                 ?.  (lth (lent p.i.p) (lent p.i.q))
                   =+  mar=$(p q, q p, s t, t s)    ::  swap recursion
                   [[q.p.mar p.p.mar] q.q.mar p.q.mar r.q.mar s.q.mar]
                 ?>  .=  p.i.p                      ::  sane conflict
                     (slag (sub (lent p.i.q) (lent p.i.p)) p.i.q)
                 %=  $                              ::  extend p
                   p  t.p
                   p.s  (welp p.i.p p.s)
                   q.s  (welp q.i.p q.s)
                   p.t  (welp p.i.p p.s)            ::  subset of q
                   q.t  (welp q.i.q q.s)            ::  just consume all out
                   q  [[%| (scag (sub (lent p.i.q) (lent p.i.p)) p.i.q) ~] t.q]
                   r  (slag (lent p.i.p) r)
                 ==
             ==
      ==
  ++  qeal                                          ::  merge p,q
    |*  [us=[ship desk] th=[ship desk] pat=path p=miso q=miso r=(list) con=?]
    ^-  miso                                        ::  in case of conflict
    ~|  %qeal-fail
    ?>  ?=(%mut -.p)
    ?>  ?=(%mut -.q)
    ?>  ?=(%c -.q.p.p)
    ?>  ?=(%c -.q.p.q)
    =+  s=(clean p.q.p.p)
    =+  t=(clean p.q.p.q)
    :-  %mut
    :-  %c  ::  todo is this p.p.p?
    :-  %c
    |-  ^-  (urge)
    ::?~  s  ?:  (qual t)  t
    ::       ~|  %qail-conflict  !!
    ::?~  t  ?:  (qual s)  s
    ::       ~|  %qail-conflict  !!
    ?~  s  t
    ?~  t  s
    ?-    -.i.s
        %&
      ?-    -.i.t
          %&
        ?:  =(p.i.s p.i.t)
          [i.s $(s t.s, t t.t, r (slag p.i.s r))]
        ?:  (gth p.i.s p.i.t)
          [i.t $(t t.t, p.i.s (sub p.i.s p.i.t), r (slag p.i.t r))]
        [i.s $(s t.s, p.i.t (sub p.i.t p.i.s), r (slag p.i.s r))]
          %|
        ?:  =(p.i.s (lent p.i.t))
          [i.t $(s t.s, t t.t, r (slag p.i.s r))]
        ?:  (gth p.i.s (lent p.i.t))
          :-  i.t 
          $(t t.t, p.i.s (sub p.i.s (lent p.i.t)), r (slag (lent p.i.t) r))
        ?.  con  ~|  %quil-conflict  !!           ::  conflict
        ~&  [%quil-conflict-soft pat]
        =+  mar=(match-conflict us th s t r)
        [[%| p.mar] $(s p.q.mar, t q.q.mar, r r.q.mar)]
      ==
        %|
      ?-    -.i.t
          %|
        ?.  con  ~|  %quil-conflict  !!
        ~&  [%quil-conflict-soft pat]
        =+  mar=(match-conflict us th s t r)
        [[%| p.mar] $(s p.q.mar, t q.q.mar, r r.q.mar)]
          %&
        ?:  =(p.i.t (lent p.i.s))
          [i.s $(s t.s, t t.t, r (slag p.i.t r))]
        ?:  (gth p.i.t (lent p.i.s))
          :-  i.s
          $(s t.s, p.i.t (sub p.i.t (lent p.i.s)), r (slag (lent p.i.s) r))
        ?.  con  ~|  %quil-conflict  !!
        ~&  [%quil-conflict-soft pat]
        =+  mar=(match-conflict us th s t r)
        [[%| p.mar] $(s p.q.mar, t q.q.mar, r r.q.mar)]
      ==
    ==
  ++  quil                                          ::  merge p,q
    |=  $:  us=[ship desk]
            th=[ship desk]
            pat=path
            p=(unit miso)
            q=(unit miso)
            r=(unit (list))
            con=?
        ==
    ^-  (unit miso)
    ?~  p  q                                        ::  trivial
    ?~  q  p                                        ::  trivial
    ?-  -.u.p
      %ins  ?>  ?=(%ins -.u.q)
            ?.  con  !!
            %-  some
            :-  %ins
            %-  role
            %-  annotate
            :-  us 
            :-  th
            :-  (lore ((hard ,@) p.u.p)) 
            :-  (lore ((hard ,@) p.u.q))
            ~
      %del  p
      %mut  ?>  ?=(%mut -.u.q)
            %-  some
            %^  qeal  us  th
            :^  pat  u.p  u.q                       ::  merge p,q
            :-  %-  need  r
            con
    ==
  ::
  ++  meld                                          ::  merge p,q from r
    |=  [p=yaki q=yaki r=yaki con=? us=[ship desk] th=[ship desk]]
    ^-  (map path blob)
    =+  s=(diff-yakis r p)
    =+  t=(diff-yakis r q)
    =+  lut=(luth s t)
    %-  |=  res=(map path blob)                        ::  add old
        ^-  (map path blob)
        %-  ~(uni by res)
        %-  mo
        %+  turn  
          %+  skip  (~(tap by q.r) ~)                  ::  loop through old
          |=  [pat=path bar=lobe]  ^-  ?
          (~(has in lut) pat)                          ::  skip updated
        |=  [pat=path bar=lobe]  ^-  [path blob]
        [pat (lobe-to-blob bar)]                       ::  lookup objects
    %+  roll  (~(tap in (luth s t)) ~)
    |=  [pat=path res=(map path blob)]
    =+  ^=  v
        %-  need
        %^  quil  us  th
        :-  pat
        :+  (~(get by s) pat)
          (~(get by t) pat)
        :_  con
        %-  %-  lift  lore
        %-  %-  lift  %-  hard  ,@                     ::  for %c
        %-  %-  lift  lobe-to-noun
        %-  ~(get by q.r)
        pat
    ?-    -.v
        %del  res                                      ::  no longer exists
        %ins                                           ::  new file
      %+  ~(put by res)  pat 
      %+  make-direct  p.v  %c                         ::  TODO content type?
        %mut                                           ::  patch from r
      %+  ~(put by res)  pat
      %-  make-direct
      :_  %c
      %+  lump  p.v
      %-  lobe-to-noun
      %-  ~(got by q.r)  pat
    ==
  ::
  ::  merge types
  ::
  ++  mate                                          ::  merge p,q
    |=  con=?                                       ::  %mate, %meld
    |=  [p=yaki q=yaki us=[ship desk] th=[ship desk]]
    ^-  (map path blob)
    =+  r=(~(tap in (find-merge-points p q)) ~)
    ?~  r
      ~|(%mate-no-ancestor !!)
    ?:  =(1 (lent r))
      (meld p q i.r con us th)
    ~|(%mate-criss-cross !!)
  ::
  ++  keep                                          ::  %this
    |=  [p=yaki q=yaki [ship desk] [ship desk]]
    ^-  (map path blob)
    %+  roll  (~(tap by q.p) ~)
    |=  [[pat=path lob=lobe] zar=(map path blob)]
    ^-  (map path blob)
    (~(put by zar) pat (lobe-to-blob lob))
  ::
  ++  drop                                          ::  %that
    |=  [p=yaki q=yaki r=[ship desk] s=[ship desk]]
    ^-  (map path blob)
    (keep q p r s)
  ::
  ++  forge                                         ::  %forge
    |=  [p=yaki q=yaki s=[ship desk] t=[ship desk]]
    ^-  (map path blob)
    =+  r=(~(tap in (find-merge-points p q)) ~)
    ?~  r
      ~|(%forge-no-ancestor !!)
    %-  |=  [r=yaki lut=(map lobe blob) hat=(map tako yaki)]
        =.  lat  lut
        =.  hut  hat
        (meld p q r & s t)                          ::  fake merge
    %+  roll  t.r                                   ::  fake ancestor
    |=  [par=yaki [for=_i.r lut=_lat hat=_hut]]
    =.  lat  lut
    =+  ^=  far
        ^-  (map path lobe)
        %-  ~(urn by (forge par for s t))
        |=  [k=path v=blob]  (blob-to-lobe v)
    =+  u=(make-yaki [r.par r.for ~] far `@da`0)    ::  fake yaki
    :-  u
    :_  (~(put by hat) r.u u)
    =<  -
    %-  update-lat
    :_  ~
    %-  ~(urn by q.u)
    |=  [path k=lobe]
    (lobe-to-blob k)
  ::
  ::  actual merge
  ::
  ++  merge
    |=  [us=[ship desk] th=[ship desk]]
    |=  [p=yaki q=yaki r=@da s=$+([yaki yaki [ship desk] [ship desk]] (map path blob))]
    ^-  [yaki (map path blob)]
    =+  u=(s p q us th)
    =+  ^=  t
        ^-  (map path lobe)
        %+  roll  (~(tap by u) ~)
        |=  [[pat=path bar=blob] yeb=(map path lobe)]
        (~(put by yeb) pat (blob-to-lobe bar))
    :_  u
    (make-yaki [r.p r.q ~] t r)
  ::
  ++  strategy                                          ::  merge strategy
    |=  gem=?(%meld %mate %that %this)
    ?-  gem
      %meld  (mate %.y)
      %mate  (mate %.n)
      %this  keep
      %that  drop
    ==
  ::
  ++  construct-merge                                   ::    construct-merge:ze
    |=  [gem=germ who=ship des=desk sab=saba now=@da]   ::  construct merge
    ^-  (unit (unit mizu))                              ::::::
    =+  for=s.sab                                       ::  foreign dome
    =+  mer=(merge [who des] [p.sab q.sab])
    ?-  gem
        %init                                           ::  force fine
          ?.  =(let 0)                                  ::  hell no
            !!
          =+  hot=(~(put by *(map aeon tako)) 1 (~(got by hit.for) let.for))
          [~ [~ [1 hot hut lat]]]                       ::  trivial
        %fine
          =+  der=(~(got by hit.for) let.for)
          =+  owr=(~(got by hit) let)
          ?:  =(der owr)
            [~ ~]
          ?:  (~(has in (reachable-takos owr)) der)
            [~ ~]
          ?.  (~(has in (reachable-takos der)) owr)
            ~                                          ::  not a fast forward
          ~&  [%merge-fine p.sab q.sab]
          [~ [~ [+(let) (~(put by hit) +(let) der) hut lat]]]
        ?(%mate %that %this %meld)
          =+  foreign-head=(tako-to-yaki (~(got by hit.for) let.for))
          =+  our-head=(tako-to-yaki (~(got by hit) let))
          ?:  =(r.foreign-head r.our-head)
            [~ ~]                                      ::  up to date
          ?:  (~(has in (reachable-takos r.our-head)) r.foreign-head)
            [~ ~]                                      ::  up to date
          ?:  ?&  |(=(gem %mate) =(gem %meld))
                  (~(has in (reachable-takos r.foreign-head)) r.our-head)
              ==
            $(gem %fine)                               ::  use fast forward
          =+  gar=(mer our-head foreign-head now (strategy gem))
          =+  yak=-.gar
          =+  hek=+.gar
          =.  lat  -:(update-lat hek ~)                ::  add new blobs
          =.  hut  (~(put by *(map tako yaki)) r.yak yak)
          =.  let  +(let)
          =.  hit  (~(put by *(map aeon tako)) let r.yak)
          [~ [~ [let hit hut lat]]]
    ==
  ::
  ++  read                                              ::    read:ze
    |=  mun=mood                                        ::  read at point
    ^-  (unit)
    ?:  ?=(%v p.mun)
      [~ `dome`+<+<.read]
    ?:  &(?=(%w p.mun) !?=(%ud -.q.mun))
      ?^(r.mun ~ [~ let])
    ?:  ?=(%w p.mun)
      =+  ^=  yak
          %-  aeon-to-yaki
          let
      ?^(r.mun ~ [~ [t.yak (forge-nori yak)]])
      ::?>  ?=(^ hit)  ?^(r.mun ~ [~ i.hit])     ::  what do?? need [@da nori]
    (query(ank ank:(descend-path:(zu ank) r.mun)) p.mun)
  ::
  ++  read-at-aeon                                      ::    read-at-aeon:ze
    |=  [yon=aeon mun=mood]                             ::  seek and read
    ^-  (unit)
    ?:  &(?=(%w p.mun) !?=(%ud -.q.mun))                ::  NB only for speed
      ?^(r.mun ~ [~ yon])
    (read:(rewind yon) mun)
  ::
  ++  equiv                                             ::  test paths
    |=  [p=(map path lobe) q=(map path lobe)]
    ^-  ?
    =-  ?.  qat  %.n
        %+  levy  (~(tap by q) ~)
        |=  [pat=path lob=lobe]
        (~(has by p) pat)
    ^=  qat
    %+  levy  (~(tap by p) ~)
    |=  [pat=path lob=lobe]
    =+  zat=(~(get by q) pat)
    ?~  zat  %.n
    =((lobe-to-noun u.zat) (lobe-to-noun lob))
  ::
  ++  edit                                              ::    edit:ze
    |=  [wen=@da lem=nori]                              ::  edit
    ^+  +>
    ?-  -.lem
      &  =^  yak  lat                                   ::  merge objects
             %+  forge-yaki  wen
             ?:  =(let 0)                               ::  initial import
               [~ q.lem]
             [(some r:(aeon-to-yaki let)) q.lem]
         ?.  ?|  =(0 let)
                 !=((lent p.yak) 1)
                 !(equiv q.yak q:(aeon-to-yaki let))
             ==
           +>.$                                         ::  silently ignore
         =:  let  +(let)
             hit  (~(put by hit) +(let) r.yak)
             hut  (~(put by hut) r.yak yak)
         ==
         +>.$(ank (checkout-ankh q.yak))
      |  +>.$(lab ?<((~(has by lab) p.lem) (~(put by lab) p.lem let)))
    ==
  --
::
++  zu  !:                                              ::  filesystem
  |=  ank=ankh                                          ::  filesystem state
  =|  myz=(list ,[p=path q=miso])                       ::  changes in reverse
  =|  ram=path                                          ::  reverse path into
  |%
  ++  rehash                                            ::  local rehash
    ^-  cash
    %+  mix  ?~(q.ank 0 p.u.q.ank)
    =+  axe=1
    |-  ^-  cash
    ?~  r.ank  *@
    ;:  mix
      (shaf %dash (mix axe (shaf %dush (mix p.n.r.ank p.q.n.r.ank))))
      $(r.ank l.r.ank, axe (peg axe 2))
      $(r.ank r.r.ank, axe (peg axe 3))
    ==
  ::
  ++  update-hash  %_(. p.ank rehash)                   ::  rehash and save
  ++  ascend                                            ::  ascend
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
  ++  push-change                                       ::  add change
    |=  mis=miso
    ^+  +>
    +>(myz [[(flop ram) mis] myz])
  ::
  ++  descend                                           ::  descend
    |=  lol=@ta
    ^+  +>
    =+  you=(~(get by r.ank) lol)
    +>.$(ram [lol ram], ank ?~(you [*cash ~ ~] u.you))
  ::
  ++  descend-path                                      ::  descend recursively
    |=  way=path
    ^+  +>
    ?~(way +> $(way t.way, +> (descend i.way)))
  ::
  ++  overwrite                                         ::  write over
    |=  [pum=umph val=(unit ,[p=cash q=*])]
    ^+  +>
    ?~  q.ank
      ?~  val  +>
      (push-change %ins q.u.val)
    ?~  val
      (push-change %del q.u.q.ank)
    ?:  =(q.u.val q.u.q.ank)  +>
    (push-change %mut ((diff pum) q.u.q.ank q.u.val))
  ::
  ++  change-tree                                       ::  modify tree
    |=  [pum=umph bus=ankh]
    ^+  +>
    =.  +>  (overwrite pum q.bus)
    =+  [yeg=(~(tap by r.ank) ~) gey=(~(tap by r.bus) ~)]
    =.  +>.$
      |-  ^+  +>.^$
      ?~  yeg  +>.^$
      ?:  (~(has by r.bus) p.i.yeg)  $(yeg t.yeg)
      $(yeg t.yeg, myz myz:rm-r(ank q.i.yeg, ram [p.i.yeg ram]))
    |-  ^+  +>.^$
    ?~  gey  +>.^$
    $(gey t.gey, myz myz:^$(bus q.i.gey, +> (descend p.i.gey)))
  ::
  ++  rm-r                                              ::  rm -r
    |-  ^+  +
    =.  +  ?~(q.ank + (push-change %del q.u.q.ank))
    =+  dyr=(~(tap by r.ank) ~)
    |-  ^+  +.^$
    ?~  dyr  +.^$
    =.  +.^$  rm-r:(descend p.i.dyr)
    $(dyr t.dyr)
  ::
  ++  drum                                              ::  apply effect
    |=  [pax=path mis=miso]                             ::  XX unused (++dune)
    ^+  +>
    ?^  pax
      update-hash:(ascend:$(pax t.pax, +> (descend i.pax)) i.pax ank)
    ~|  %clay-fail
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
    |-  ^+  +                                           ::  XX unused (++durn)
    ?~  myz  +
    =>  .(+ (drum p.i.myz q.i.myz))
    $(myz ?>(?=(^ myz) t.myz))
  ::
  ++  durn                                              ::  apply forward
    |=  nyp=soba                                        ::  XX unused
    ^+  +>
    ?:  =([0 0] p.nyp)
      dune(myz q.nyp)
    =>  ?:  =(p.ank p.p.nyp)  .
        ~&  [%durn-in-wrong p.ank p.p.nyp]
        .
    =.  +>  dune(myz q.nyp)
    =>  ?:  =(p.ank q.p.nyp)  .
        ~&  [%durn-out-wrong p.ank q.p.nyp]
        .
    +>
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bF, filesystem interface     ::
::
++  feel                                                ::  simple file write
  |=  [pax=path val=*]
  ^-  miso
  =+  dir=((hard arch) .^(%cy pax))
  ?~  q.dir  [%ins val]
  :-  %mut
  ^-  udon
  [%a %a .^(%cx pax) val]
::
++  file                                                ::  simple file load
  |=  pax=path
  ^-  (unit)
  =+  dir=((hard arch) .^(%cy pax))
  ?~(q.dir ~ [~ .^(%cx pax)])
::
++  foal                                                ::  high-level write
  |=  [pax=path val=*]
  ^-  toro
  ?>  ?=([* * * *] pax)
  [i.t.pax [%& [*cart [[t.t.t.pax (feel pax val)] ~]]]]
::
++  fray                                                ::  high-level delete
  |=  pax=path
  ^-  toro
  ?>  ?=([* * * *] pax)
  [i.t.pax [%& [*cart [[t.t.t.pax [%del .^(%cx pax)]] ~]]]]
::
++  furl                                                ::  unify changes
  |=  [one=toro two=toro]
  ^-  toro
  ~|  %furl
  ?>  ?&  =(p.one p.two)                                ::  same path
          &(?=(& -.q.one) ?=(& -.q.two))                ::  both deltas
      ==
  [p.one [%& [*cart (weld q.q.q.one q.q.q.two)]]]
::
++  meat                                                ::  kite to .^ path
  |=  kit=kite
  ^-  path
  [(cat 3 'c' p.kit) (scot %p r.kit) s.kit (scot `dime`q.kit) t.kit]
::
++  tame                                                ::  parse kite path
  |=  hap=path
  ^-  (unit kite)
  ?.  ?=([@ @ @ @ *] hap)  ~
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
  ?.  ?=([~ %$ case] ved)  ~
  =+  his=`@p`q.p.u.fal
  =+  [dis=(end 3 1 q.p.u.hyr) rem=(rsh 3 1 q.p.u.hyr)]
  ?.  ?&(?=(%c dis) ?=(?(%v %w %x %y %z) rem))  ~
  [~ rem p.u.ved q.p.u.fal q.p.u.dyc tyl]
::
++  tome                                                ::  parse path to beam
  |=  pax=path
  ^-  (unit beam)
  ?.  ?=([* * * *] pax)  ~
  %+  biff  (slaw %p i.pax)
  |=  who=ship
  %+  biff  (slaw %tas i.t.pax)
  |=  dex=desk
  %+  biff  (slay i.t.t.pax)
  |=  cis=coin
  ?.  ?=([%$ case] cis)  ~
  `(unit beam)`[~ [who dex `case`p.cis] (flop t.t.t.pax)]
::
++  tope                                                ::  beam to path
  |=  bem=beam
  ^-  path
  [(scot %p p.bem) q.bem (scot r.bem) (flop s.bem)]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bG, URL handling             ::
::
++  deft                                                ::  parse url extension
  |=  rax=(list ,@t)
  |-  ^-  pork
  ?~  rax
    [~ ~]
  ?^  t.rax
    [p.pok [i.rax q.pok]]:[pok=$(rax t.rax) .]
  =+  ^-  raf=(unit ,[p=term q=(unit term)])
      %+  rush  i.rax
      ;~(plug sym ;~((bend) (easy ~) ;~(pfix dot sym)))
  ?~  raf
    [~ [i.rax ~]]
  [q.u.raf [p.u.raf ~]]
::
++  fain                                                ::  path restructure
  |=  [hom=path raw=path]
  =+  bem=(need (tome raw))
  =+  [mer=(flop s.bem) moh=(flop hom)]
  |-  ^-  (pair beam path)
  ?~  moh
    [bem(s hom) (flop mer)]
  ?>  &(?=(^ mer) =(i.mer i.moh))
  $(mer t.mer, moh t.moh)
::
++  fuel                                                ::  parse fcgi
  |=  [bem=beam but=path]
  ^-  epic
  ?>  ?=([%web @ *] but)
  =+  dyb=(slay i.t.but)
  ?>  ?&  ?=([~ %many *] dyb)
          ?=([* * *] p.u.dyb)
          ::  ?=([%$ %tas *] i.p.u.dyb)
          ?=([%many *] i.p.u.dyb)
          ?=([%blob *] i.t.p.u.dyb)
      ==
  =+  ced=((hard cred) p.i.t.p.u.dyb)
  ::  =+  nep=q.p.i.p.u.dyb
  =+  ^=  nyp  ^-  path
      %+  turn  p.i.p.u.dyb
      |=  a=coin  ^-  @ta
      ?>  ?=([%$ %ta @] a)
      ?>(((sane %ta) q.p.a) q.p.a)
  =+  ^=  gut  ^-  (list ,@t)
      %+  turn  t.t.p.u.dyb
      |=  a=coin  ^-  @t
      ?>  ?=([%$ %t @] a)
      ?>(((sane %t) q.p.a) q.p.a)
  =+  ^=  quy
      |-  ^-  (list ,[p=@t q=@t])
      ?~  gut  ~
      ?>  ?=(^ t.gut)
      [[i.gut i.t.gut] $(gut t.t.gut)]
  :*  (~(gas by *(map cord cord)) quy)
      ced
      bem
      t.t.but
      nyp
  ==
::
++  sifo                                                ::  64-bit encode
  |=  tig=@
  ^-  tape
  =+  poc=(~(dif fo 3) 0 (met 3 tig))
  =+  pad=(lsh 3 poc (swap 3 tig))
  =+  ^=  cha
  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  =+  ^=  sif
      |-  ^-  tape
      ?~  pad
        ~
      =+  d=(end 0 6 pad)
      [(cut 3 [d 1] cha) $(pad (rsh 0 6 pad))]
  (weld (flop (slag poc sif)) (trip (fil 3 poc '=')))
::
++  urle                                                ::  URL encode
  |=  tep=tape
  ^-  tape
  %-  zing
  %+  turn  tep
  |=  tap=char
  =+  xen=|=(tig=@ ?:((gte tig 10) (add tig 55) (add tig '0')))
  ?:  ?|  &((gte tap 'a') (lte tap 'z'))
          &((gte tap 'A') (lte tap 'Z'))
          &((gte tap '0') (lte tap '9'))
          =('.' tap)
          =('-' tap)
          =('~' tap)
          =('_' tap)
      ==
    [tap ~]
  ['%' (xen (rsh 0 4 tap)) (xen (end 0 4 tap)) ~]
::
++  urld                                                ::  URL decode
  |=  tep=tape
  ^-  (unit tape)
  ?~  tep  [~ ~]
  ?:  =('%' i.tep)
    ?.  ?=([@ @ *] t.tep)  ~
    =+  nag=(mix i.t.tep (lsh 3 1 i.t.t.tep))
    =+  val=(rush nag hex:ag)
    ?~  val  ~
    =+  nex=$(tep t.t.t.tep)
    ?~(nex ~ [~ [`@`u.val u.nex]])
  =+  nex=$(tep t.tep)
  ?~(nex ~ [~ i.tep u.nex])
::
++  earl                                                ::  localize purl
  |=  [who=@p pul=purl]
  ^-  purl
  pul(q.q [(rsh 3 1 (scot %p who)) q.q.pul])
::
++  earn                                                ::  purl to tape
  |^  |=  pul=purl
      ^-  tape
      :(weld (head p.pul) "/" (body q.pul) (tail r.pul))
  ::
  ++  body
    |=  pok=pork  ^-  tape
    ?~  q.pok  ~
    |-
    =+  seg=(trip i.q.pok)
    ?~  t.q.pok
      ?~(p.pok seg (welp seg '.' (trip u.p.pok)))
    (welp seg '/' $(q.pok t.q.pok))
  ::
  ++  head
    |=  har=hart
    ^-  tape
    ;:  weld
      ?:(&(p.har !=([& /localhost] r.har)) "https://" "http://")
    ::
      ?-  -.r.har
        |  (trip (rsh 3 1 (scot %if p.r.har)))
        &  =+  rit=(flop p.r.har)
           |-  ^-  tape
           ?~(rit ~ (weld (trip i.rit) ?~(t.rit "" `tape`['.' $(rit t.rit)])))
      ==
    ::
      ?~(q.har ~ `tape`[':' (trip (rsh 3 2 (scot %ui u.q.har)))])
    ==
  ::
  ++  tail
    |=  kay=quay
    ^-  tape
    ?:  =(~ kay)  ~
    :-  '?'
    |-  ^-  tape
    ?~  kay  ~
    ;:  weld
      (urle (trip p.i.kay))
      "="
      (urle (trip q.i.kay))
      ?~(t.kay ~ `tape`['&' $(kay t.kay)])
    ==
  --
::
++  epur                                                ::  url/header parser
  =<  |=(a=cord (rush a auri))
  |%
  ++  apat                                              ::  2396 abs_path
    %+  cook  deft
    (ifix [fas ;~(pose fas (easy ~))] (more fas smeg))
  ++  auri                                              ::  2396 URL
    %+  cook
      |=  a=purl
      ?.(=([& /localhost] r.p.a) a a(p.p &))
    ;~  plug
      ;~  plug
        %+  sear
          |=  a=@t
          ^-  (unit ,?)
          ?+(a ~ %http [~ %|], %https [~ %&])
        ;~(sfix scem ;~(plug col fas fas))
        thor
      ==
      ;~(plug ;~(pose apat (easy *pork)) yque)
    ==
  ++  cock                                              ::  cookie
    (most ;~(plug sem ace) ;~(plug toke ;~(pfix tis tosk)))
  ++  dlab                                              ::  2396 domainlabel
    %+  sear
      |=  a=@ta
      ?.(=('-' (rsh 3 (dec (met 3 a)) a)) [~ u=a] ~)
    %+  cook  cass
    ;~(plug aln (star alp))
  ::
  ++  fque  (cook crip (plus pquo))                     ::  normal query field
  ++  fquu  (cook crip (star pquo))                     ::  optional field
  ++  pcar  ;~(pose pure pesc psub col pat)             ::  2396 path char
  ++  pcok  ;~(less bas sem com doq prn)                ::  cookie char
  ++  pesc  ;~(pfix cen mes)                            ::  2396 escaped
  ++  pold  (cold ' ' (just '+'))                       ::  old space code
  ++  pque  ;~(pose pcar fas wut)                       ::  3986 query char
  ++  pquo  ;~(pose pure pesc pold)                     ::  normal query char
  ++  pure  ;~(pose aln hep dot cab sig)                ::  2396 unreserved
  ++  psub  ;~  pose                                    ::  3986 sub-delims
              zap  buc  pam  soq  pel  per
              tar  lus  com  sem  tis
            ==
  ++  ptok  ;~  pose                                    ::  2616 token
              aln  zap  hax  buc  cen  pam  soq  tar  lus
              hep  dot  ket  cab  tec  bar  sig
            ==
  ++  scem                                              ::  2396 scheme
    %+  cook  cass
    ;~(plug alf (star ;~(pose aln lus hep dot)))
  ::
  ++  smeg  (cook crip (plus pcar))                     ::  2396 segment
  ++  tock  (cook crip (plus pcok))                     ::  6265 cookie-value
  ++  tosk  ;~(pose tock (ifix [doq doq] tock))         ::  6265 cookie-value
  ++  toke  (cook crip (plus ptok))                     ::  2616 token
  ++  thor                                              ::  2396 host/port
    %+  cook  |*(a=[* *] [+.a -.a])
    ;~  plug
      thos
      ;~((bend) (easy ~) ;~(pfix col dim:ag))
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
    ;~  pose                                            ::  proper query
      %+  more
        ;~(pose pam sem)
      ;~(plug fque ;~(pose ;~(pfix tis fquu) (easy '')))
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
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bH, names etc                ::
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
++  glam                                                ::  carrier names
  |=  zar=@pD  ^-  tape
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
      "Suleiman the Magnificent"  "Pedro II"  "Genghis Khan"  "Laozi"
      "Porfirio Díaz"  "Pakal"  "Wu Zetian"  "Garibaldi"  "Matthias Corvinus"
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
      "Ivan the Terrible"  "Yao"  "Vercingetorix"  "Geronimo"  "Rurik"
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
++  glon                                                ::  ISO language codes
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
++  gnom                                                ::  ship display name
  |=  [[our=@p now=@da] him=@p]  ^-  @t
  =+  yow=(scot %p him)
  =+  pax=[(scot %p our) %name (scot %da now) yow ~]
  =+  woy=((hard ,@t) .^(%a pax))
  ?:  =(%$ woy)  yow
  (rap 3 yow ' ' woy ~)
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
++  hunt                                                ::  first of unit dates
  |=  [one=(unit ,@da) two=(unit ,@da)]
  ^-  (unit ,@da)
  ?~  one  two
  ?~  two  one
  ?:((lth u.one u.two) one two)
::
++  mojo                                                ::  compiling load
  |=  [pax=path src=*]
  ^-  (each twig (list tank))
  ?.  ?=(@ src)
    [%| ~[[leaf/"musk: malformed: {<pax>}"]]]
  =+  ^=  mud
      %-  mule  |.
      ((full vest) [1 1] (trip src))
  ?:  ?=(| -.mud)  mud
  ?~  q.p.mud
    :~  %|
        leaf/"musk: syntax error: {<pax>}"
        leaf/"musk: line {<p.p.p.mud>}, column {<q.p.p.mud>}"
    ==
  [%& p.u.q.p.mud]
::
++  mole                                                ::  new to old sky
  |=  ska=$+(* (unit (unit)))
  |=  a=*
  ^-  (unit)
  =+  b=(ska a)
  ?~  b  ~
  ?~  u.b  ~
  [~ u.u.b]
::
++  much                                                ::  constructing load
  |=  [pax=path src=*]
  ^-  gank
   =+  moj=(mojo pax src)
  ?:  ?=(| -.moj)  moj
  (mule |.((slap !>(+>.$) `twig`p.moj)))
::
++  musk                                                ::  compiling apply
  |=  [pax=path src=* sam=vase]
  ^-  gank
  =+  mud=(much pax src)
  ?:  ?=(| -.mud)  mud
  (mule |.((slam p.mud sam)))
::
++  saxo                                                ::  autocanon
  |=  who=ship
  ^-  (list ship)
  ?:  (lth who 256)  [who ~]
  [who $(who (sein who))]
::
++  sein                                                ::  autoboss
  |=  who=ship  ^-  ship
  =+  mir=(clan who)
  ?-  mir
    %czar  who
    %king  (end 3 1 who)
    %duke  (end 4 1 who)
    %earl  (end 5 1 who)
    %pawn  `@p`0
  ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bI, Arvo models              ::
::
++  acru                                                ::  asym cryptosuite
          $_  ^?  |%                                    ::  opaque object
          ++  as  ^?                                    ::  asym ops
            |%  ++  seal  |=([a=pass b=@ c=@] *@)       ::  encrypt to a
                ++  sign  |=([a=@ b=@] *@)              ::  certify as us
                ++  sure  |=([a=@ b=@] *(unit ,@))      ::  authenticate from us
                ++  tear  |=  [a=pass b=@]              ::  accept from a 
                          *(unit ,[p=@ q=@])            ::
            --                                          ::
          ++  de  |+([a=@ b=@] *(unit ,@))              ::  symmetric de, soft
          ++  dy  |+([a=@ b=@] *@)                      ::  symmetric de, hard
          ++  en  |+([a=@ b=@] *@)                      ::  symmetric en
          ++  ex  ^?                                    ::  export
            |%  ++  fig  *@uvH                          ::  fingerprint
                ++  pac  *@uvG                          ::  default passcode
                ++  pub  *pass                          ::  public key
                ++  sec  *ring                          ::  private key
            --                                          ::
          ++  nu  ^?                                    ::  reconstructors
             |%  ++  pit  |=([a=@ b=@] ^?(..nu))        ::  from [width seed]
                 ++  nol  |=(a=@ ^?(..nu))              ::  from naked ring
                 ++  com  |=(a=@ ^?(..nu))              ::  from naked pass
            --                                          ::
          --                                            ::
++  aeon  ,@ud                                          ::
++  agon  (map ,[p=ship q=desk] ,[p=@ud q=@ud r=waks])  ::  mergepts
++  ankh                                                ::  fs node (new)
          $:  p=cash                                    ::  recursive hash
              q=(unit ,[p=cash q=*])                    ::  file
              r=(map ,@ta ankh)                         ::  folders
          ==                                            ::
++  ankz  ,[p=@ (map ,@ta ankz)]                        ::  trimmed ankh
++  apex  ,[p=@uvI q=(map ,@ta ,@uvI) r=(map ,@ta ,~)]  ::  node report (old)
++  ares  (unit ,[p=term q=(list tank)])                ::  possible error
++  ball  ,@uw                                          ::  statement payload
++  bait  ,[p=skin q=@ud r=dove]                        ::  fmt nrecvd spec
++  bath                                                ::  convo per client
          $:  sop=shed                                  ::  not stalled
              raz=(map path race)                       ::  statements inbound
              ryl=(map path rill)                       ::  statements outbound
          ==                                            ::
++  beam  ,[[p=ship q=desk r=case] s=path]              ::  global name
++  beak  ,[p=ship q=desk r=case]                       ::  garnish with beak
++  bird                                                ::  packet in travel
          $:  gom=soap                                  ::  message identity
              mup=@ud                                   ::  pktno in msg
              nux=@ud                                   ::  xmission count
              lys=@da                                   ::  last sent
              pac=rock                                  ::  packet data
          ==                                            ::
++  blob  $%  [%delta p=lobe q=lobe r=udon]             ::  delta on q
              [%direct p=lobe q=* r=umph]               ::
              [%indirect p=lobe q=* r=udon s=lobe]      ::
          ==                                            ::
++  boat  ,[(list slip) tart]                           ::  user stage
++  boon                                                ::  fort output
          $%  [%beer p=ship q=@uvG]                     ::  gained ownership
              [%cake p=sock q=soap r=coop s=duct]       ::  e2e message result
              [%coke p=sock q=soap r=cape s=duct]       ::  message result
              [%mead p=lane q=rock]                     ::  accept packet
              [%milk p=sock q=soap r=*]                 ::  accept message
              [%mulk p=sock q=soap r=*]                 ::  e2e pass message
              [%ouzo p=lane q=rock]                     ::  transmit packet
              [%wine p=sock q=tape]                     ::  notify user
          ==                                            ::
++  bowl  ,[p=(list gift) q=(unit boat)]                ::  app product
++  bray  ,[p=life q=(unit life) r=ship s=@da]          ::  our parent us now
++  brow  ,[p=@da q=@tas]                               ::  browser version
++  buck  ,[p=mace q=will]                              ::  all security data
++  cake  ,[p=sock q=skin r=@]                          ::  top level packet
++  cape                                                ::  end-to-end result
          $?  %good                                     ::  delivered
              %dead                                     ::  rejected
          ==                                            ::
++  cart  ,[p=cash q=cash]                              ::  hash change
++  care  ?(%u %v %w %x %y %z)                          ::  clay submode
++  case                                                ::  ship desk case spur
          $%  [%da p=@da]                               ::  date
              [%tas p=@tas]                             ::  label
              [%ud p=@ud]                               ::  number
          ==                                            ::
++  cash  ,@uvH                                         ::  ankh hash
++  chum  ,@uvI                                         ::  hashed passcode
++  clot                                                ::  symmetric record
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
++  claw                                                ::  startup chain
          $:  joy=(unit coal)                           ::  local context
              ran=(unit coal)                           ::  arguments
              pux=(unit path)                           ::  execution path
              jiv=(unit coal)                           ::  app configuration
              kyq=(unit coal)                           ::  app customization
              gam=(unit coal)                           ::  app image
          ==                                            ::
++  clip  (each ,@if ,@is)                              ::  client IP
++  coal  ,*                                            ::  untyped vase
++  code  ,@uvI                                         ::  symmetric key
++  cone                                                ::  reconfiguration
          $%  [& p=twig]                                ::  transform
              [| p=(list ,@tas)]                        ::  alter
          ==                                            ::
++  coop  (unit ares)                                   ::  e2e ack
++  corn                                                ::  flow by server
          $:  hen=duct                                  ::  admin channel
              nys=(map flap bait)                       ::  packets incoming
              olz=(map flap cape)                       ::  packets completed
              wab=(map ship bath)                       ::  relationship
          ==                                            ::
++  cred                                                ::  credential
          $:  hut=hoot                                  ::  client host
              aut=(jug ,@tas ,@t)                       ::  client identities
              orx=oryx                                  ::  CSRF secret
              acl=(unit ,@t)                            ::  accept-language
              cip=(each ,@if ,@is)                      ::  client IP
              cum=(map ,@tas ,*)                        ::  custom dirt
          ==                                            ::
++  cuff                                                ::  permissions
          $:  p=(unit (set monk))                       ::  readers
              q=(set monk)                              ::  authors
          ==                                            ::
++  deed  ,[p=@ q=step r=?]                             ::  sig, stage, fake?
++  dome                                                ::  project state
          $:  ang=agon                                  ::  pedigree
              ank=ankh                                  ::  state
              let=@ud                                   ::  top id
              hit=(map ,@ud tako)                       ::  changes by id
              lab=(map ,@tas ,@ud)                      ::  labels
          ==                                            ::
++  dore                                                ::  foreign contact
          $:  wod=road                                  ::  connection to
              wyl=will                                  ::  inferred mirror
              caq=clot                                  ::  symmetric key state
          ==                                            ::
++  dove  ,[p=@ud q=(map ,@ud ,@)]                      ::  count hash 13-blocks
++  epic                                                ::  FCGI parameters
          $:  qix=(map ,@t ,@t)                         ::  query
              ced=cred                                  ::  client credentials
              bem=beam                                  ::  original path
              but=path                                  ::  ending
              nyp=path                                  ::  request model
          ==                                            ::
++  flap  ,@uvH                                         ::  network packet id
++  flow                                                ::  packet connection
          $:  rtt=@dr                                   ::  decaying avg rtt
              wid=@ud                                   ::  logical wdow msgs
          ==                                            ::
++  fort                                                ::  formal state
          $:  %0                                        ::  version
              gad=duct                                  ::  client interface
              hop=@da                                   ::  network boot date
              ton=town                                  ::  security
              zac=(map ship corn)                       ::  flows by server
          ==                                            ::
++  frog  ,[p=@da q=nori]                               ::  time and change
++  gank  (each vase (list tank))                       ::  abstract result
++  gift                                                ::  one-way effect
          $%  [%$ p=vase]                               ::  trivial output
              [%cc p=(unit case)]                       ::  change case
              [%ck p=@tas]                              ::  change desk
              [%cs p=path]                              ::  change spur
              [%de p=@ud q=tank]                        ::  debug/level
              [%ex p=(unit vase) q=lath]                ::  exec/patch
            ::[%fd p=vase]                              ::  fundamental down
            ::[%fo p=vase]                              ::  fundamental forward
            ::[%fu p=vase]                              ::  fundamental up
              [%ha p=tank]                              ::  single error
              [%ho p=(list tank)]                       ::  multiple error
              [%la p=tank]                              ::  single statement
              [%lo p=(list tank)]                       ::  multiple statement
              [%mu p=type q=(list)]                     ::  batch emit
              [%mx p=(list gift)]                       ::  batch gift
              [%ok p=@ta q=nori]                        ::  save changes
              [%og p=@ta q=mizu]                        ::  save direct
              [%sc p=(unit skit)]                       ::  stack library
              [%sp p=(list lark)]                       ::  spawn task(s)
              [%sq p=ship q=@tas r=path s=*]            ::  send request
              [%sr p=ship q=path r=*]                   ::  send response
              [%te p=(list ,@t)]                        ::  dump lines
              [%th p=@ud q=love]                        ::  http response
              [%tq p=path q=hiss]                       ::  http request
              [%va p=@tas q=(unit vase)]                ::  set/clear variable
              [%xx p=curd]                              ::  return card
              [%xy p=path q=curd]                       ::  push card
              [%xz p=[p=ship q=path] q=ship r=mark s=zang]
              [%zz p=path q=path r=curd]                ::
          ==                                            ::
++  zang                                                ::  XX evil hack
          $%  [%backlog p=path q=?(%da %dr %ud) r=@]    ::
              [%hola p=path]                            ::
              $:  %mess  p=path                         ::
                $=  q                                   ::
              $%  [%do p=@t]                            ::  act
                  [%exp p=@t q=tank]                    ::  code
                  [%say p=@t]                           ::  speak
              ==  ==                                    ::
              [%tint p=ship]                            ::
          ==                                            ::
++  gilt  ,[@tas *]                                     ::  presumed gift
++  gens  ,[p=lang q=gcos]                              ::  general identity
++  germ  ?(%init %fine %that %this %mate %meld)        ::  merge style
++  gcos                                                ::  id description
          $%  [%czar ~]                                 ::  8-bit ship
              [%duke p=what]                            ::  32-bit ship
              [%earl p=@t]                              ::  64-bit ship
              [%king p=@t]                              ::  16-bit ship
              [%pawn p=(unit ,@t)]                      ::  128-bit ship
          ==                                            ::
++  goad                                                ::  common note
          $%  [%eg p=riot]                              ::  simple result
              [%gr p=mark q=*]                          ::  gall rush/rust
              [%hp p=httr]                              ::  http response
              ::  [%ht p=@ud q=scab r=cred s=moth]          ::  http request
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
              [%es p=ship q=desk r=rave]                ::  subscription
              [%gr ~]                                   ::  gall response
              [%ht p=(list rout)]                       ::  http server
              [%hp ~]                                   ::  http response
              [%lq p=@tas]                              ::  listen for service
              [%ow ~]                                   ::  one-way reaction
              [%rt ~]                                   ::  roundtrip response
              [%up p=prod]                              ::  user prompt
              [%wa p=@da]                               ::  alarm
          ==                                            ::
++  govt  path                                          ::  country/postcode
++  hand  ,@uvH                                         ::  hash of code
++  hart  ,[p=? q=(unit ,@ud) r=host]                   ::  http sec/port/host
++  hate  ,[p=purl q=@p r=moth]                         ::  semi-cooked request
++  heir  ,[p=@ud q=mess r=(unit love)]                 ::  status/headers/data
++  hiss  ,[p=purl q=moth]                              ::  outbound request
++  hist  ,[p=@ud q=(list ,@t)]                         ::  depth texts
++  hole  ,@t                                           ::  session identity
++  hoot  ,[p=? q=(unit ,@ud) r=host]                   ::  secure/port/host
++  hort  ,[p=(unit ,@ud) q=host]                       ::  http port/host
++  host  $%([& p=(list ,@t)] [| p=@if])                ::  http host
++  httq                                                ::  raw http request
          $:  p=meth                                    ::  method
              q=@t                                      ::  unparsed url
              r=(list ,[p=@t q=@t])                     ::  headers
              s=(unit octs)                             ::  body
          ==                                            ::
++  httr  ,[p=@ud q=mess r=(unit octs)]                 ::  raw http response
++  httx                                                ::  encapsulated http
          $:  p=?                                       ::  https?
              q=clip                                    ::  source IP
              r=httq                                    ::
          ==                                            ::
++  kite  ,[p=care q=case r=ship s=desk t=spur]         ::  parsed global name
++  json                                                ::  normal json value
          $|  ~                                         ::  null
          $%  [%a p=(list json)]                        ::  array
              [%b p=?]                                  ::  boolean
              [%o p=(map ,@t json)]                     ::  object
              [%n p=@ta]                                ::  number
              [%s p=@ta]                                ::  string
          ==                                            ::
++  jsot                                                ::  strict json top
          $%  [%a p=(list json)]                        ::  array
              [%o p=(map ,@t json)]                     ::  object
          ==                                            ::
++  lamb                                                ::  short path
          $%  [& p=@tas]                                ::  auto
              [| p=twig]                                ::  manual
          ==                                            ::
++  lane                                                ::  packet route
          $%  [%if p=@da q=@ud r=@if]                   ::  IP4/public UDP/addr
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
++  lice  ,[p=ship q=buck]                              ::  full license
++  life  ,@ud                                          ::  regime number
++  lint  (list rock)                                   ::  fragment array
++  lobe  ,@                                            ::  blob ref
++  love  $%                                            ::  http response
              [%ham p=manx]                             ::  html node
              [%mid p=mite q=octs]                      ::  mime-typed data
              [%raw p=httr]                             ::  raw http response
              [%wan p=wain]                             ::  text lines
              [%zap p=@ud q=(list tank)]                ::  status/error
          ==                                            ::
++  luge  ,[p=mark q=*]                                 ::  fully typed content
++  maki  ,[p=@ta q=@ta r=@ta s=path]                   ::
++  mace  (list ,[p=life q=ring])                       ::  private secrets
++  marv  ?(%da %tas %ud)                               ::  release form
++  math  (map ,@t (list ,@t))                          ::  semiparsed headers
++  meal                                                ::  payload
          $%  [%back p=cape q=flap r=@dr]               ::  acknowledgment
              [%buck p=coop q=flap r=@dr]               ::  e2e ack
              [%bond p=life q=path r=@ud s=*]           ::  message
              [%bund p=life q=path r=@ud s=*]           ::  e2e message
              [%carp p=@ q=@ud r=@ud s=flap t=@]        ::  skin/inx/cnt/hash
              [%fore p=ship q=(unit lane) r=@]          ::  forwarded packet
          ==                                            ::
++  mess  (list ,[p=@t q=@t])                           ::  raw http headers
++  meta                                                ::  path metadata
          $%  [& q=@uvI]                                ::  hash
              [| q=(list ,@ta)]                         ::  dir
          ==                                            ::
++  meth                                                ::  http methods
          $?  %conn                                     ::  CONNECT
              %delt                                     ::  DELETE
              %get                                      ::  GET
              %head                                     ::  HEAD
              %opts                                     ::  OPTIONS
              %post                                     ::  POST
              %put                                      ::  PUT
              %trac                                     ::  TRACE
          ==                                            ::
++  mite  (list ,@ta)                                   ::  mime type
++  miso                                                ::  ankh delta
          $%  [%del p=*]                                ::  delete
              [%ins p=*]                                ::  insert
              [%mut p=udon]                             ::  mutate
          ==                                            ::
++  mizu  ,[p=@u q=(map ,@ud tako) r=rang]              ::  new state
++  moar  ,[p=@ud q=@ud]                                ::  normal change range
++  moat  ,[p=case q=case r=path]                       ::  change range
++  mood  ,[p=care q=case r=path]                       ::  request in desk
++  moth  ,[p=meth q=math r=(unit octs)]                ::  http operation
++  name  ,[p=@t q=(unit ,@t) r=(unit ,@t) s=@t]        ::  first mid/nick last
++  newt  ?(%boot %kick %mess %slay %wake)              ::  lifecycle events
++  nose                                                ::  response, kernel
          $?  [%$ p=(unit ,[p=tutu q=(list)])]          ::  standard input
              goad                                      ::
          ==                                            ::
++  note                                                ::  response, user
          $?  [%$ p=(unit ,[p=type q=(list)])]          ::  standard input
              [%do p=vase]                              ::  execution result
              goad                                      ::
          ==                                            ::
++  nori                                                ::  repository action
          $%  [& q=soba]                                ::  delta
              [| p=@tas]                                ::  label
          ==                                            ::
++  octs  ,[p=@ud q=@]                                  ::  octet-stream
++  oryx  ,@t                                           ::  CSRF secret
++  pact  path                                          ::  routed path
++  pail  ?(%none %warm %cold)                          ::  connection status
++  plan  (trel view (pair ,@da (unit ,@dr)) path)      ::  subscription
++  plea  ,[p=@ud q=[p=? q=@t]]                         ::  live prompt
++  pork  ,[p=(unit ,@ta) q=(list ,@t)]                 ::  fully parsed url
++  pred  ,[p=@ta q=@tas r=@ta ~]                       ::  proto-path
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
++  quay  (list ,[p=@t q=@t])                           ::  parsed url query
++  quri                                                ::  request-uri
          $%  [& p=purl]                                ::  absolute
              [| p=pork q=quay]                         ::  relative
          ==                                            ::
++  race                                                ::  inbound stream
          $:  did=@ud                                   ::  filled sequence
              dod=?                                     ::  not processing
              bum=(map ,@ud ares)                       ::  nacks
              mis=(map ,@ud ,[p=cape q=lane r=flap s=(unit)]) ::  misordered
          ==                                            ::
++  rank  ?(%czar %king %duke %earl %pawn)              ::  ship width class
++  rang  $:  hut=(map tako yaki)                       ::
              lat=(map lobe blob)                       ::
          ==                                            ::
++  rant                                                ::  namespace binding
          $:  p=[p=care q=case r=@tas]                  ::  clade release book
              q=path                                    ::  spur
              r=*                                       ::  data
          ==                                            ::
++  rave                                                ::  general request
          $%  [& p=mood]                                ::  single request
              [| p=moat]                                ::  change range
          ==                                            ::
++  rill                                                ::  outbound stream
          $:  sed=@ud                                   ::  sent
              san=(map ,@ud duct)                       ::  outstanding
          ==                                            ::
++  riot  (unit rant)                                   ::  response/complete
++  road                                                ::  secured oneway route
          $:  exp=@da                                   ::  expiration date
              lun=(unit lane)                           ::  route to friend
              lew=will                                  ::  will of friend
          ==                                            ::
++  rock  ,@uvO                                         ::  packet
++  rout  ,[p=(list host) q=path r=oryx s=path]         ::  http route (new)
++  rump  ,[p=care q=case r=@tas s=path]                ::  relative path
++  saba  ,[p=ship q=@tas r=moar s=dome]                ::  patch/merge
++  sack  ,[p=ship q=ship]                              ::  incoming [our his]
++  sufi                                                ::  domestic host
          $:  hoy=(list ship)                           ::  hierarchy
              val=wund                                  ::  private keys
              law=will                                  ::  server will
              seh=(map hand ,[p=ship q=@da])            ::  key cache
              hoc=(map ship dore)                       ::  neighborhood
          ==                                            ::
++  salt  ,@uv                                          ::  entropy
++  seal                                                ::  auth conversation
          $:  whu=(unit ship)                           ::  client identity
              pul=purl                                  ::  destination url
              wit=?                                     ::  wait for partner
              foy=(unit ,[p=ship q=hole])               ::  partner to notify
              pus=(unit ,@ta)                           ::  password
          ==                                            ::
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
++  skin  ?(%none %open %fast %full)                    ::  encoding stem
++  slip  ,[p=path q=goal]                              ::  traceable request
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
++  soba  ,[p=cart q=(list ,[p=path q=miso])]           ::  delta
++  sock  ,[p=ship q=ship]                              ::  outgoing [from to]
++  spur  path                                          ::  ship desk case spur
++  step  ,[p=bray q=gens r=pass]                       ::  identity stage
++  tako  ,@                                            ::  yaki ref
++  tart  $+([@da path note] bowl)                      ::  process core
++  taxi  ,[p=lane q=rock]                              ::  routed packet
++  tick  ,@ud                                          ::  process id
++  toro  ,[p=@ta q=nori]                               ::  general change
++  town                                                ::  all security state
          $:  lit=@ud                                   ::  imperial modulus
              any=@                                     ::  entropy
              urb=(map ship sufi)                       ::  all keys and routes
              fak=?                                     ::
          ==                                            ::
++  tube  ,[p=@ta q=@ta r=@ta s=path]                   ::  canonical path
++  tutu  ,*                                            ::  presumed type
++  yaki  ,[p=(list tako) q=(map path lobe) r=tako t=@da] ::  commit
++  view  ?(%u %v %w %x %y %z)                          ::  view mode
++  waks  (map path woof)                               ::  list file states
++  what                                                ::  logical identity
          $%  [%anon ~]                                 ::  anonymous
              [%lady p=whom]                            ::  female person ()
              [%lord p=whom]                            ::  male person []
              [%punk p=sect q=@t]                       ::  opaque handle ""
          ==                                            ::
++  whom  ,[p=@ud q=govt r=sect s=name]                 ::  year/govt/id
++  woof  $|  %know                                     ::  udon transform
              [%chan (list $|(@ud [p=@ud q=@ud]))]      ::
++  wund  (list ,[p=life q=ring r=acru])                ::  mace in action
++  will  (list deed)                                   ::  certificate
++  worm  ,*                                            ::  vase of tart
++  zuse  %314                                          ::  hoon/zuse kelvin
--
