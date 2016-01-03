::
::  zuse (3), standard library (tang)   
::
~%  %zuse  +  ~
!:
|%
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 3b, Arvo libraries            ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bA, lite number theory       ::
::
++  dope
  ~/  %dope
  |=  a/@
  ~&  [%dope-zuse (mug +>)]
  :(mul a a a)
::
++  fu                                                  ::  modulo (mul p q)
  |=  a/{p/@ q/@}
  =+  b=?:(=([0 0] a) 0 (~(inv fo p.a) (~(sit fo p.a) q.a)))
  |%
  ++  dif
    |=  {c/{@ @} d/{@ @}}
    [(~(dif fo p.a) -.c -.d) (~(dif fo q.a) +.c +.d)]
  ::
  ++  exp
    |=  {c/@ d/{@ @}}
    :-  (~(exp fo p.a) (mod c (dec p.a)) -.d)
    (~(exp fo q.a) (mod c (dec q.a)) +.d)
  ::
  ++  out                                               ::  garner's formula
    |=  c/{@ @}
    %+  add
      +.c
    (mul q.a (~(pro fo p.a) b (~(dif fo p.a) -.c (~(sit fo p.a) +.c))))
  ::
  ++  pro
    |=  {c/{@ @} d/{@ @}}
    [(~(pro fo p.a) -.c -.d) (~(pro fo q.a) +.c +.d)]
  ::
  ++  sum
    |=  {c/{@ @} d/{@ @}}
    [(~(sum fo p.a) -.c -.d) (~(sum fo q.a) +.c +.d)]
  ::
  ++  sit
    |=  c/@
    [(mod c p.a) (mod c q.a)]
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bB, cryptosuites             ::
::
++  crua  !:                                            ::  cryptosuite A (RSA)
  ^-  acru
  =|  {mos/@ pon/(unit {p/@ q/@ r/{p/@ q/@} s/_*fu})}
  =>  |%
      ++  mx  (dec (met 0 mos))                         ::  bit length
      ++  dap                                           ::  OEAP decode
        |=  {wid/@ xar/@ dog/@}  ^-  {p/@ q/@}
        =+  pav=(sub wid xar)
        =+  qoy=(cut 0 [xar pav] dog)
        =+  dez=(mix (end 0 xar dog) (shaw %pad-b xar qoy))
        [dez (mix qoy (shaw %pad-a pav dez))]
      ::
      ++  pad                                           ::  OEAP encode
        |=  {wid/@ rax/{p/@ q/@} meg/@}  ^-  @
        =+  pav=(sub wid p.rax)
        ?>  (gte pav (met 0 meg))
        ^-  @
        =+  qoy=(mix meg (shaw %pad-a pav q.rax))
        =+  dez=(mix q.rax (shaw %pad-b p.rax qoy))
        (can 0 [p.rax dez] [pav qoy] ~)
      ::
      ++  pull  |=(a/@ (~(exp fo mos) 3 a))
      ++  push  |=(a/@ (~(exp fo mos) 5 a))
      ++  pump
        |=  a/@  ^-  @
        ?~  pon  !!
        (out.s.u.pon (exp.s.u.pon p.r.u.pon (sit.s.u.pon a)))
      ::
      ++  punt
        |=  a/@  ^-  @
        ?~  pon  !!
        (out.s.u.pon (exp.s.u.pon q.r.u.pon (sit.s.u.pon a)))
      --
  |%                                                    ::  opaque object
  ++  as
    =>  |%
        ++  haul                                        ::  revealing haul
          |=  a/pass
          =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
          ?>  =('a' mag)
          ..as(mos bod, pon ~)
        --
    ^?
    |%  ++  seal
          |=  {a/pass b/@ c/@}
          ^-  @
          =>  .(c (sign b c))
          =+  her=(haul a)
          =+  det=(lte (add 256 (met 0 c)) mx.her)
          =+  lip=?:(det c 0)
          =-  (add ?:(p.mav 0 1) (lsh 0 1 q.mav))
          ^=  mav  ^-  {p/? q/@}
          :-  det
          =+  dog=(pad mx.her [256 b] lip)
          =+  hog=(push.her dog)
          =+  ben=(en b c)
          ?:(det hog (jam hog ben))
        ++  sign
          |=  {a/@ b/@}  ^-  @
          =-  (add ?:(p.mav 0 1) (lsh 0 1 q.mav))
          ^=  mav  ^-  {p/? q/@}
          =+  det=(lte (add 128 (met 0 b)) mx)
          :-  det
          =+  hec=(shaf (mix %agis a) b)
          =+  dog=(pad mx [128 hec] ?:(det b 0))
          =+  hog=(pump dog)
          ?:(det hog (jam hog b))
        ++  sure
          |=  {a/@ b/@}
          ^-  (unit @)
          =+  [det==(0 (end 0 1 b)) bod=(rsh 0 1 b)]
          =+  gox=?:(det [p=bod q=0] ((hard {p/@ q/@}) (cue bod)))
          =+  dog=(pull p.gox)
          =+  pig=(dap mx 128 dog)
          =+  log=?:(det q.pig q.gox)
          ?.(=(p.pig (shaf (mix %agis a) log)) ~ [~ log])
        ++  tear
          |=  {a/pass b/@}
          ^-  (unit {p/@ q/@})
          =+  her=(haul a)
          =+  [det==(0 (end 0 1 b)) bod=(rsh 0 1 b)]
          =+  gox=?:(det [p=bod q=0] ((hard {p/@ q/@}) (cue bod)))
          =+  dog=(punt p.gox)
          =+  pig=(dap mx 256 dog)
          =+  ^=  cow
              ^-  (unit @)
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
    |+  {key/@ cep/@}  ^-  (unit @)
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
  ++  dy  |+({a/@ b/@} (need (de a b)))
  ++  en
    |+  {key/@ msg/@}  ^-  @ux
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
          |=  {a/@ b/@}
          (div (mul a b) d:(egcd a b))
        ::
        ++  eldm
          |=  {a/@ b/@ c/@}
          (~(inv fo (elcm (dec b) (dec c))) a)
        ::
        ++  ersa
          |=  {a/@ b/@}
          [a b [(eldm 3 a b) (eldm 5 a b)] (fu a b)]
        --
    ^?
    |%  ++  com
          |=  a/@
          ^+  ^?(..nu)
          ..nu(mos a, pon ~)
        ::
        ++  pit
          |=  {a/@ b/@}
          =+  c=(rsh 0 1 a)
          =+  [d=(ramp c [3 5 ~] b) e=(ramp c [3 5 ~] +(b))]
          ^+  ^?(..nu)
          ..nu(mos (mul d e), pon [~ (ersa d e)])
        ::
        ++  nol
          |=  a/@
          ^+  ^?(..nu)
          =+  b=((hard {p/@ q/@}) (cue a))
          ..nu(mos (mul p.b q.b), pon [~ (ersa p.b q.b)])
    --
  --                                                    ::
:: 
++  bruw                                                ::  create keypair
  |=  {a/@ b/@}                                         ::  width seed
  ^-  acru
  (pit:nu:crua a b)
::
++  haul                                                ::  activate public key
  |=  a/pass
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('a' mag)
  (com:nu:crua bod)
::
++  weur                                                ::  activate secret key
  |=  a/ring
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('A' mag)
  (nol:nu:crua bod)
::
++  trua                                                ::  test rsa
  |=  msg/@tas
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
  =|  {puc/pass sed/ring}
  =>  |%
      ++  dap                                           ::  OEAP decode
        |=  {wid/@ xar/@ dog/@}  ^-  {p/@ q/@}
        =+  pav=(sub wid xar)
        =+  qoy=(cut 0 [xar pav] dog)
        =+  dez=(mix (end 0 xar dog) (shaw %pad-b xar qoy))
        [dez (mix qoy (shaw %pad-a pav dez))]
      ::
      ++  pad                                           ::  OEAP encode
        |=  {wid/@ rax/{p/@ q/@} meg/@}  ^-  @
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
          |=  a/pass
          !!
        --
    ^?
    |%  ++  seal
          |=  {a/pass b/@ c/@}
          ^-  @
          !!
        ++  sign
          |=  {a/@ b/@}  ^-  @
          !!
        ++  sure
          |=  {a/@ b/@}
          ^-  (unit @)
          !!
        ++  tear
          |=  {a/pass b/@}
          ^-  (unit {p/@ q/@})
          !!
    --
  ::
  ++  de
    |+  {key/@ cep/@}  ^-  (unit @)
    !!
  ::
  ++  dy
    |+  {a/@ b/@}  ^-  @
    !!
  ++  en
    |+  {key/@ msg/@}  ^-  @ux
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
          |=  a/@
          ^+  ^?(..nu)
          ..nu(sed ~, puc a)
        ::
        ++  pit
          |=  {a/@ b/@}
          ^+  ^?(..nu)
          ..nu(sed b, puc (puck:ed b))
        ::
        ++  nol
          |=  a/@
          ^+  ^?(..nu)
          ..nu(sed a, puc (puck:ed a))
    --
  --
++  brew                                                ::  create keypair
  |=  {a/@ b/@}                                         ::  width seed
  ^-  acru
  (pit:nu:crub a b)
::
++  hail                                                ::  activate public key
  |=  a/pass
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('b' mag)
  (com:nu:crub bod)
::
++  wear                                                ::  activate secret key
  |=  a/ring
  ^-  acru
  =+  [mag=(end 3 1 a) bod=(rsh 3 1 a)]
  ?>  =('b' mag)
  (nol:nu:crub bod)
::
++  trub                                                ::  test ed
  |=  msg/@tas
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
  |=  {key/@ mes/@}
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
  |=  yer/@ud
  =+  yet=(sub yer 1)
  %-  mod  :_  7
  :(add 1 (mul 5 (mod yet 4)) (mul 4 (mod yet 100)) (mul 6 (mod yet 400)))
::
++  daws                                                ::  date weekday
  |=  yed/date
  %-  mod  :_  7
  (add (dawn y.yed) (sub (yawn [y.yed m.yed d.t.yed]) (yawn y.yed 1 1)))
::
++  deal                                                ::  to leap sec time
  |=  yer/@da
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
  |=  ley/date
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
  |=  yed/date
  ^-  tape
  =+  wey=(daws yed)
  ;:  weld
      `tape`(snag wey (turn wik:yu |=(a/tape (scag 3 a))))
      ", "  ~(rud at d.t.yed)  " "
      `tape`(snag (dec m.yed) (turn mon:yu |=(a/tape (scag 3 a))))
      " "  (scag 1 ~(rud at y.yed))  (slag 2 ~(rud at y.yed))  " "
      ~(rud at h.t.yed)  ":"  ~(rud at m.t.yed)  ":"  ~(rud at s.t.yed)
      " "  "+0000"
  ==
::
++  stud    !:                                          ::  parse UTC format
  =<  |=  a/cord                                        ::  expose parsers
      %+  biff  (rush a (more sepa elem))
      |=  b/(list _(wonk *elem))  ^-  (unit date)
      =-  ?.((za:jo -) ~ (some (zp:jo -)))
      ^+  =+  [*date u=unit]
          *{(u _[a y]) (u _m) (u _d.t) (u _+.t) $~}
      :~                                                ::  XX  types
          |-(?~(b ~ ?.(?=($y -.i.b) $(b t.b) `+.i.b)))
          |-(?~(b ~ ?.(?=($m -.i.b) $(b t.b) `+.i.b)))
          |-(?~(b ~ ?.(?=($d -.i.b) $(b t.b) `+.i.b)))
          |-(?~(b ~ ?.(?=($t -.i.b) $(b t.b) `+.i.b)))
      ==
  |%
  ::
  ++  snug
    |=  a/wall
    |=  b/tape
    =+  [pos=1 len=(lent b)]
    |-  ^-  (unit @u)
    ?~  a  ~
    ?:  =(b (scag len i.a))
      `pos
    $(pos +(pos), a t.a)
  ::
  ::
  ++  sepa  ;~(pose ;~(plug com (star ace)) (plus ace))
  ++  elem
    ;~  pose 
      (stag %t t)  (stag %y y)  (stag %m m)  (stag %d d)
      (stag %w w)  (stag %z z)
    == 
  ::
  ++  y  (stag %& (bass 10 (stun 3^4 dit)))
  ++  m  (sear (snug mon:yu) (plus alf))
  ++  d  (bass 10 (stun 1^2 dit))
  ++  t  [;~(plug - - + (easy ~))]:[;~(sfix d col) d]
  ++  w  (sear (snug wik:yu) (plus alf))
  ++  z  [;~(plug (mask "-+") . .)]:(bass 10 (stun 2^2 dit))
  --
::
++  unt                                                 ::  UGT to UTC time
  |=  a/@
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
  ++  les  ^-  (list @da)
    :~  ~2015.7.1  ~2012.7.1  ~2009.1.1  ~2006.1.1  ~1999.1.1  ~1997.7.1
        ~1996.1.1  ~1994.7.1  ~1993.7.1  ~1992.7.1  ~1991.1.1  ~1990.1.1
        ~1988.1.1  ~1985.7.1  ~1983.7.1  ~1982.7.1  ~1981.7.1  ~1980.1.1
        ~1979.1.1  ~1978.1.1  ~1977.1.1  ~1976.1.1  ~1975.1.1  ~1974.1.1
        ~1973.1.1  ~1972.7.1
    ==
  ++  lef  ^-  (list @da)
    :~  ~2015.6.30..23.59.59   ~2012.6.30..23.59.59
        ~2008.12.31..23.59.58  ~2005.12.31..23.59.57
        ~1998.12.31..23.59.56  ~1997.6.30..23.59.55
        ~1995.12.31..23.59.54  ~1994.6.30..23.59.53
        ~1993.6.30..23.59.52   ~1992.6.30..23.59.51
        ~1990.12.31..23.59.50  ~1989.12.31..23.59.49
        ~1987.12.31..23.59.48  ~1985.6.30..23.59.47
        ~1983.6.30..23.59.46   ~1982.6.30..23.59.45
        ~1981.6.30..23.59.44   ~1979.12.31..23.59.43
        ~1978.12.31..23.59.42  ~1977.12.31..23.59.41
        ~1976.12.31..23.59.40  ~1975.12.31..23.59.39
        ~1974.12.31..23.59.38  ~1973.12.31..23.59.37
        ~1972.12.31..23.59.36  ~1972.6.30..23.59.35
    ==
  --
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bD, JSON and XML             ::
::
++  moon                                                ::  mime type to text
  |=  myn/mite
  %-  crip
  |-  ^-  tape
  ?~  myn  ~
  ?:  =(~ t.myn)  (trip i.myn)
  (weld (trip i.myn) `tape`['/' $(myn t.myn)])
::
++  perk                                                ::  parse cube fork 
  |*  a/(pole @tas)
  ?~  a  fail
  ;~  pose 
    (cold -.a (jest -.a))
    $(a +.a)
  ==
::
++  poja                                                ::  parse JSON
  =<  |=(a/cord `(unit json)`(rush a apex))
  |%
  ++  apex                                              ::  JSON value
    %+  knee  *json  |.  ~+
    %+  ifix  [spac spac]
    ;~  pose
      (cold ~ (jest 'null'))
      (stag %b bool)
      (stag %s stri)
      (cook |=(s/tape [%n p=(rap 3 s)]) numb)
      abox
      obox
    ==
  ++  tops  ;~(pose abox obox)                          ::  JSON strict
  ::  JSON arrays
  ++  abox  (stag %a (ifix [sel (ws ser)] (more (ws com) apex)))
  ::  JSON objects
  ++  pair  ;~(plug ;~(sfix (ws stri) (ws col)) apex)
  ++  obje  (ifix [(ws kel) (ws ker)] (more (ws com) pair))
  ++  obox  (stag %o (cook malt obje))
  ::  JSON booleans
  ++  bool  ;~(pose (cold & (jest 'true')) (cold | (jest 'false')))
  ::  JSON strings
  ++  stri  (cook crip (ifix [doq doq] (star jcha)))
  ++  jcha  ;~(pose ;~(less doq bas prn) esca)           :: character in string
  ++  esca                                               :: Escaped character
    ;~  pfix  bas
      ;~  pose
        doq  fas  soq  bas
        (sear ~(get by `(map @t @)`(my b+8 t+9 n+10 f+12 r+13 ~)) low)
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
  ++  ws  |*(sef/rule ;~(pfix spac sef))
  ::  plumbing
  ++  mayb  |*(bus/rule ;~(pose bus (easy "")))
  ++  twel  |=({a/tape b/tape} (weld a b))
  ++  piec
    |*  bus/rule
    (cook |=(a/@ [a ~]) bus)
  --
::
++  pojo                                                ::  print json
  =|  rez/tape
  |=  val/json
  ^-  tape
  ?~  val  (weld "null" rez)
  ?-    -.val
      $a
    :-  '['
    =.  rez  [']' rez]
    !.
    ?~  p.val  rez
    |-
    ?~  t.p.val  ^$(val i.p.val)
    ^$(val i.p.val, rez [',' $(p.val t.p.val)])
 ::
      $b  (weld ?:(p.val "true" "false") rez)
      $n  (weld (trip p.val) rez)
      $s
    :-  '"'
    =.  rez  ['"' rez]
    =+  viz=(trip p.val)
    !.
    |-  ^-  tape
    ?~  viz  rez
    =+  hed=(jesc i.viz)
    ?:  ?=({@ $~} hed)                 :: common case
      [i.hed $(viz t.viz)]            :: cons-and-tail
    (weld hed $(viz t.viz))
 ::
      $o
    :-  '{'
    =.  rez  ['}' rez]
    =+  viz=(~(tap by p.val))
    ?~  viz  rez
    !.
    |-  ^+  rez
    ?~  t.viz  ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
    =.  rez  [',' $(viz t.viz)]
    ^$(val [%s p.i.viz], rez [':' ^$(val q.i.viz)])
  ==
::
++  poxo                                                ::  node to tape
  =<  |=(a/manx `tape`(apex a ~))
  |_  _[unq=`?`| cot=`?`|]                           ::  self-close all tags
  ++  apex                                              ::  top level
    |=  {mex/manx rez/tape}
    ^-  tape
    ?:  ?=({$$ {{$$ *} $~}} g.mex)
      (escp v.i.a.g.mex rez)
    =+  man=`mane`n.g.mex
    =.  unq  |(unq =(%script man) =(%style man))
    =+  tam=(name man)
    =+  att=`mart`a.g.mex
    :-  '<'
    %+  welp  tam
    =-  ?~(att rez [' ' (attr att rez)])
    ^-  rez/tape
    ?:  &(?=($~ c.mex) |(cot (clot man)))
      [' ' '/' '>' rez]
    :-  '>'
    (many c.mex :(weld "</" tam ">" rez))
  ::
  ++  attr                                              ::  attributes to tape
    |=  {tat/mart rez/tape}
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
    |=  {tex/tape rez/tape}
    ?:  unq
      (weld tex rez)
    =+  xet=`tape`(flop tex)
    !.
    |-  ^-  tape
    ?~  xet  rez
    %=    $
      xet  t.xet
      rez  ?-  i.xet
             $34  ['&' 'q' 'u' 'o' 't' ';' rez]
             $38  ['&' 'a' 'm' 'p' ';' rez]
             $39  ['&' '#' '3' '9' ';' rez]
             $60  ['&' 'l' 't' ';' rez]
             $62  ['&' 'g' 't' ';' rez]
             *    [i.xet rez]
           ==
    ==
  ::
  ++  many                                              ::  nodelist to tape
    |=  {lix/(list manx) rez/tape}
    |-  ^-  tape
    ?~  lix  rez
    (apex i.lix $(lix t.lix))
  ::
  ++  name                                              ::  name to tape
    |=  man/mane  ^-  tape
    ?@  man  (trip man)
    (weld (trip -.man) `tape`[':' (trip +.man)])
  ::
  ++  clot  ~+                                          ::  self-closing tags
    %~  has  in
    %-  silt  ^-  (list term)  :~
      %area  %base  %br  %col  %command  %embed  %hr  %img  %input  %keygen
      %link  %meta  %param     %source   %track  %wbr 
    ==
  --
::
++  poxa                                                ::  xml parser
  =<  |=(a/cord (rush a apex))
  |_  ent/_`(map term @t)`[[%apos '\''] ~ ~]
  ++  apex
    =+  spa=;~(pose comt whit)
    %+  knee  *manx  |.  ~+
    %+  ifix  [(star spa) (star spa)]
    ;~  pose
      %+  sear  |=({a/marx b/marl c/mane} ?.(=(c n.a) ~ (some [a b])))
        ;~(plug head many tail)
      empt
    == 
  ::
  ++  attr                                              ::  attributes
    %+  knee  *mart  |.  ~+ 
    %-  star
    ;~  plug  
      ;~(pfix (plus whit) name)
      ;~  pose
        (ifix [;~(plug tis doq) doq] (star ;~(less doq escp)))
        (ifix [;~(plug tis soq) soq] (star ;~(less soq escp)))
        (easy ~)
      ==
    ==
  ::
  ++  chrd                                              ::  character data
    %+  cook  |=(a/tape ^-(mars ;/(a)))
    (plus ;~(less doq ;~(pose (just `@`10) escp)))
  ::
  ++  comt                                              ::  comments 
    =-  (ifix [(jest '<!--') (jest '-->')] (star -))
    ;~  pose
      ;~(less hep prn)
      whit
      ;~(less (jest '-->') hep)
    ==
  :: 
  ++  escp  ;~(pose ;~(less gal gar pam prn) enty)
  ++  enty                                              ::  entity
    %+  ifix  pam^sem
    ;~  pose
      =+  def=^+(ent (my [%gt '>'] [%lt '<'] [%amp '&'] [%quot '"'] ~))
      %+  sear  ~(get by (~(uni by def) ent))
      (cook crip ;~(plug alf (stun 1^31 aln)))
      %+  cook  |=(a/@c ?:((gth a 0x10.ffff) '�' (tuft a)))
      =<  ;~(pfix hax ;~(pose - +))
      :-  (bass 10 (stun 1^8 dit))
      (bass 16 ;~(pfix (mask "xX") (stun 1^8 hit)))
    ==
  ::
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
  =>  |%  ++  grub  (unit *) 
          ++  fist  $+(json grub)
      --
  |%
  ++  ar                                                ::  array as list
    |*  wit/fist
    |=  jon/json
    ?.  ?=({$a *} jon)  ~
    %-  zl
    |-  
    ?~  p.jon  ~
    [i=(wit i.p.jon) t=$(p.jon t.p.jon)]
  ::
  ++  at                                                ::  array as tuple
    |*  wil/(pole fist)
    |=  jon/json
    ?.  ?=({$a *} jon)  ~
    =+  raw=((at-raw wil) p.jon)
    ?.((za raw) ~ (some (zp raw)))
  ::
  ++  at-raw                                            ::  array as tuple
    |*  wil/(pole fist)
    |=  jol/(list json)
    ?~  wil  ~
    :-  ?~(jol ~ (-.wil i.jol))
    ((at-raw +.wil) ?~(jol ~ t.jol))
  ::
  ++  bo                                                ::  boolean
    |=(jon/json ?.(?=({$b *} jon) ~ [~ u=p.jon]))
  ::
  ++  bu                                                ::  boolean not
    |=(jon/json ?.(?=({$b *} jon) ~ [~ u=!p.jon]))
  ::
  ++  ci                                                ::  maybe transform
    |*  {poq/$+(* *) wit/fist}
    |=  jon/json
    (biff (wit jon) poq)
  ::
  ++  cu                                                ::  transform
    |*  {poq/$+(* *) wit/fist}
    |=  jon/json
    (bind (wit jon) poq)
  ::
  ++  da                                                ::  UTC date
    |=  jon/json
    ?.  ?=({$s *} jon)  ~
    (bind (stud p.jon) |=(a/date (year a)))
  ::
  ++  di                                                ::  millisecond date
    %+  cu
      |=  a/@u  ^-  @da
      (add ~1970.1.1 (div (mul ~s1 a) 1.000))
    ni
  ::
  ++  mu                                                ::  true unit
    |*  wit/fist
    |=  jon/json
    ?~(jon (some ~) (bind (wit jon) some))
  ::
  ++  ne                                                ::  number as real
    |=  jon/json
    ^-  (unit @rd)
    !!
  ::
  ++  ni                                                ::  number as integer
    |=  jon/json 
    ?.  ?=({$n *} jon)  ~
    (rush p.jon dem)
  ::
  ++  no                                                ::  number as cord
    |=  jon/json
    ?.  ?=({$n *} jon)  ~
    (some p.jon)
  ::
  ++  of                                                ::  object as frond
    |*  wer/(pole {cord fist})
    |=  jon/json
    ?.  ?=({$o {@ *} $~ $~} jon)  ~
    |-
    ?~  wer  ~
    ?:  =(-.-.wer p.n.p.jon)  
      ((pe -.-.wer +.-.wer) q.n.p.jon)
    ((of +.wer) jon)
  ::
  ++  ot                                                ::  object as tuple
    |*  wer/(pole {cord fist})
    |=  jon/json
    ?.  ?=({$o *} jon)  ~
    =+  raw=((ot-raw wer) p.jon)
    ?.((za raw) ~ (some (zp raw)))
  ::
  ++  ot-raw                                            ::  object as tuple
    |*  wer/(pole {cord fist})
    |=  jom/(map @t json)
    ?~  wer  ~
    =+  ten=(~(get by jom) -.-.wer)
    [?~(ten ~ (+.-.wer u.ten)) ((ot-raw +.wer) jom)]
  ::
  ++  om                                                ::  object as map
    |*  wit/fist
    |=  jon/json
    ?.  ?=({$o *} jon)  ~
    (zm (~(run by p.jon) wit))
  ::
  ++  op                                                ::  parse keys of map
    |*  {fel/rule wit/fist}
    %+  cu  my
    %-  ci  :_  (om wit)
    |=  a/(map cord _(need *wit))
    ^-  (unit (list _[(wonk *fel) (need *wit)]))
    =-  (zl (turn (~(tap by a)) -))
    |*  {a/cord b/*}
    =+  nit=(rush a fel) 
    ?~  nit  ~
    (some [u.nit b])
  ::
  ++  pe                                                ::  prefix
    |*  {pre/* wit/fist}
    (cu |*(a/* [pre a]) wit)
  ::
  ++  sa                                                ::  string as tape
    |=  jon/json
    ?.(?=({$s *} jon) ~ (some (trip p.jon)))
  ::
  ++  so                                                ::  string as cord
    |=  jon/json
    ?.(?=({$s *} jon) ~ (some p.jon))
  ::
  ++  su                                                ::  parse string
    |*  sab/rule
    |=  jon/json
    ?.  ?=({$s *} jon)  ~
    (rush p.jon sab)
  ::
  ++  ul  |=(jon/json ?~(jon (some ~) ~))               ::  null
  ++  za                                                ::  full unit pole
    |*  pod/(pole (unit))
    ?~  pod  &
    ?~  -.pod  |
    (za +.pod)
  ::
  ++  zl                                                ::  collapse unit list
    |*  lut/(list (unit))
    ?.  |-  ^-  ?
        ?~(lut & ?~(i.lut | $(lut t.lut)))
      ~
    %-  some
    |-
    ?~  lut  ~
    [i=u:+.i.lut t=$(lut t.lut)]
  ::
  ++  zp                                                ::  unit tuple
    |*  but/(pole (unit))
    ?~  but  !!
    ?~  +.but  
      u:->.but
    [u:->.but (zp +.but)]
  ::
  ++  zm                                                ::  collapse unit map
    |*  lum/(map term (unit))
    ?:  (~(rep by lum) |=({{@ a/(unit)} b/_|} |(b ?=($~ a))))
      ~
    (some (~(run by lum) need))
  --
::
++  joba                                                ::  object from k-v pair
  |=  {p/@t q/json}
  ^-  json
  [%o [[p q] ~ ~]]
::
++  jobe                                                ::  object from k-v list
  |=  a/(list {p/@t q/json})
  ^-  json
  [%o (~(gas by *(map @t json)) a)]
::
++  jape                                                ::  string from tape
  |=  a/tape
  ^-  json
  [%s (crip a)]
::
++  jone                                                ::  number from unsigned
  |=  a/@u
  ^-  json
  :-  %n
  ?:  =(0 a)  '0'
  (crip (flop |-(^-(tape ?:(=(0 a) ~ [(add '0' (mod a 10)) $(a (div a 10))])))))
::
++  jode                                                ::  ms timestamp
  |=  a/time 
  =-  (jone (div (mul - 1.000) ~s1))
  (add (div ~s1 2.000) (sub a ~1970.1.1))
::
++  jesc
  =+  utf=|=(a/@ ['\\' 'u' ((x-co 4):co a)]) 
  |=  a/@  ^-  tape
  ?+  a  ?:((gth a 0x1f) [a ~] (utf a))
    $10  "\\n"
    $34  "\\\""
    $92  "\\\\"
  ==
::
++  scanf                                              ::  formatted scan
  |*  {tape (pole _;/(*{$^(rule tape)}))}
  =>  .(+< [a b]=+<)
  (scan a (parsf b))
++  parsf                                              ::  make parser from:
  |^  |*  a/(pole _;/(*{$^(rule tape)}))            ::  ;"chars{rule}chars"
      %-  cook  :_  (bill (norm a))
      |*  (list)
      ?~  +<  ~
      ?~  t  i
      [i $(+< t)]
  ::
  ::  .=  (norm [;"{n}, {n}"]:n=dim:ag)  ~[[& dim] [| ", "] [& dim]]:ag
  ++  norm                                             
    |*  (pole _;/(*{$^(rule tape)}))
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
    |*  (list (each rule tape))
    ?~  +<  (easy ~)
    ?:  ?=($| -.i)  ;~(pfix (jest (crip p.i)) $(+< t))
    %+  cook  |*({* *} [i t]=+<)
    ;~(plug p.i $(+< t))
  --
::
++  taco                                                ::  atom to octstream
  |=  tam/@  ^-  octs
  [(met 3 tam) tam]
::
++  tact                                                ::  tape to octstream
  |=  tep/tape  ^-  octs
  (taco (rap 3 tep))
::
++  tell                                                ::  wall to octstream
  |=  wol/wall  ^-  octs
  =+  buf=(rap 3 (turn wol |=(a/tape (crip (weld a `tape`[`@`10 ~])))))
  [(met 3 buf) buf]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bF, filesystem interface     ::
::
++  feel                                                ::  simple file write
  |=  {pax/path val/cage}
  ^-  miso
  =+  dir=((hard arch) .^(%cy pax))
  ?~  fil.dir  [%ins val]
  [%mut val]
::
++  file                                                ::  simple file load
  |=  pax/path
  ^-  (unit)
  =+  dir=((hard arch) .^(%cy pax))
  ?~(fil.dir ~ [~ .^(%cx pax)])
::
++  foal                                                ::  high-level write
  |=  {pax/path val/cage}
  ^-  toro
  ?>  ?=({* * * *} pax)
  [i.t.pax [%& [[[t.t.t.pax (feel pax val)] ~]]]]
::
++  fray                                                ::  high-level delete
  |=  pax/path
  ^-  toro
  ?>  ?=({* * * *} pax)
  [i.t.pax [%& [[[t.t.t.pax [%del ~]] ~]]]]
::
++  furl                                                ::  unify changes
  |=  {one/toro two/toro}
  ^-  toro
  ~|  %furl
  ?>  ?&  =(p.one p.two)                                ::  same path
          &(?=($& -.q.one) ?=($& -.q.two))              ::  both deltas
      ==
  [p.one [%& (weld p.q.one p.q.two)]]
::
++  tame                                                ::  parse kite path
  |=  hap/path
  ^-  (unit kite)
  ?.  ?=({@ @ @ @ *} hap)  ~
  =+  :*  hyr=(slay i.hap)
          fal=(slay i.t.hap)
          dyc=(slay i.t.t.hap)
          ved=(slay i.t.t.t.hap)
          ::  ved=(slay i.t.hap)
          ::  fal=(slay i.t.t.hap)
          ::  dyc=(slay i.t.t.t.hap)
          tyl=t.t.t.t.hap
      ==
  ?.  ?=({$~ $$ $tas @} hyr)  ~
  ?.  ?=({$~ $$ $p @} fal)  ~
  ?.  ?=({$~ $$ $tas @} dyc)  ~
  ?.  ?=({$~ $$ case} ved)  ~
  =+  his=`@p`q.p.u.fal
  =+  [dis=(end 3 1 q.p.u.hyr) rem=(rsh 3 1 q.p.u.hyr)]
  ?.  ?&(?=($c dis) ?=(?($v $w $x $y $z) rem))  ~
  [~ rem p.u.ved q.p.u.fal q.p.u.dyc tyl]
::
++  tome                                                ::  parse path to beam
  |=  pax/path
  ^-  (unit beam)
  ?.  ?=({* * * *} pax)  ~
  %+  biff  (slaw %p i.pax)
  |=  who/ship
  %+  biff  (slaw %tas i.t.pax)
  |=  dex/desk
  %+  biff  (slay i.t.t.pax)
  |=  cis/coin
  ?.  ?=({$$ case} cis)  ~
  `(unit beam)`[~ [who dex `case`p.cis] (flop t.t.t.pax)]
::
++  tope                                                ::  beam to path
  |=  bem/beam
  ^-  path
  [(scot %p p.bem) q.bem (scot r.bem) (flop s.bem)]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bG, URL handling             ::
::
++  deft                                                ::  parse url extension
  |=  rax/(list @t)
  |-  ^-  pork
  ?~  rax
    [~ ~]
  ?^  t.rax
    [p.pok [ire q.pok]]:[pok=$(rax t.rax) ire=i.rax]
  =+  ^-  raf/(like term)
      =>  |=(a/@ ((sand %tas) (crip (flop (trip a)))))
      (;~(sfix (sear . sym) dot) [1^1 (flop (trip i.rax))])
  ?~  q.raf
    [~ [i.rax ~]]
  =+  `{ext/term {@ @} fyl/tape}`u.q.raf
  :-  `ext
  ?:(=(~ fyl) ~ [(crip (flop fyl)) ~])
::
++  fuel                                                ::  parse fcgi
  |=  {bem/beam but/path}
  ^-  epic
  ?>  ?=({$web @ *} but)
  =+  dyb=(slay i.t.but)
  ?>  ?&  ?=({$~ $many *} dyb)
          ?=({* * *} p.u.dyb)
          ?=({$$ $ta *} i.p.u.dyb)
          ?=({$blob *} i.t.p.u.dyb)
      ==
  =+  ced=((hard cred) p.i.t.p.u.dyb)
  ::  =+  nep=q.p.i.p.u.dyb
  =+  ^=  gut  ^-  (list @t)
      %+  turn  t.t.p.u.dyb
      |=  a/coin  ^-  @t
      ?>  ?=({$$ $t @} a)
      ?>(((sane %t) q.p.a) q.p.a)
  =+  ^=  quy
      |-  ^-  (list {p/@t q/@t})
      ?~  gut  ~
      ?>  ?=(^ t.gut)
      [[i.gut i.t.gut] $(gut t.t.gut)]
  :*  (~(gas by *(map cord cord)) quy)
      ced
      bem
      t.t.but
  ==
::
++  sifo                                                ::  64-bit encode
  |=  tig/@
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
  (weld (flop (slag poc sif)) (reap poc '='))
::
++  ofis  ::  XX broken
  =-  |=(a/cord (rash a fel))
  =<  fel=(cook |+(a/@ `@t`(swap 3 a)) (bass 64 .))
  =-  (cook welp ;~(plug (plus siw) (stun 0^2 (cold %0 tis))))
  ^=  siw
  ;~  pose
     (cook |=(a/@ (sub a 'A')) (shim 'A' 'Z'))
     (cook |=(a/@ (sub a 'G')) (shim 'a' 'z'))
     (cook |=(a/@ (add a 4)) (shim '0' '9'))
     (cold 62 (just '+'))
     (cold 63 (just '/'))
   ==
::
++  dray                                                ::  load tuple into path
  =-  |*  {a/{@tas (pole @tas)} b/*}  ^-  (paf a)
      =>  .(b `(tup -.a +.a)`b)
      ?~  +.a  [(scot -.a b) ~]
      [(scot -.a -.b) `(paf +.a)`(..$ +.a +.b)]
  :-  paf=|*(a/(pole) ?~(a $~ {(odo:raid -.a(. %ta)) (..$ +.a)}))
  tup=|*({a/@tas b/(pole @tas)} =+(c=(odo:raid a) ?~(b c {c (..$ -.b +.b)})))
::
::  .=  [p=%ack q=~sarnel r=&]
::  (raid /ack+~sarnel+.y p=%tas q=%p r=%f ~)
++  raid                                                ::  demand path odors
  =-  |*  {a/path b/{@tas (pole @tas)}}
      ?~  +.b  `(odo -.b)`(slav -.b -.a)
      [`(odo -.b)`(slav -.b -.a) (..$ +.a +.b)]
  ^=  odo
  |*  a/@tas
  |=  b/*
  =<  a(. (. b))                  ::  preserve face
  ?+  a   @
    $c  @c  $da  @da  $dr  @dr  $f   @f   $if  @if  $is  @is  $p   @p
    $u  @u  $uc  @uc  $ub  @ub  $ui  @ui  $ux  @ux  $uv  @uv  $uw  @uw
    $s  @s  $t   @t   $ta  @ta  $tas  @tas
  ==
::
++  read                                                ::  parse odored path
  =<  |*({a/path b/{@tas (pole @tas)}} ((+> b) a))
  |*  b/{@tas (pole @tas)}
  |=  a/path
  ?~  a  ~
  =+  hed=(slaw -.b i.a)
  ?~  +.b
    ^-  (unit (odo:raid -.b))
    ?^(+.a ~ hed)
  ^-  (unit {(odo:raid -.b) _(need *(..^$ +.b))})
  (both hed ((..^$ +.b) +.a))
::
++  urle                                                ::  URL encode
  |=  tep/tape
  ^-  tape
  %-  zing
  %+  turn  tep
  |=  tap/char
  =+  xen=|=(tig/@ ?:((gte tig 10) (add tig 55) (add tig '0')))
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
  |=  tep/tape
  ^-  (unit tape)
  ?~  tep  [~ ~]
  ?:  =('%' i.tep)
    ?.  ?=({@ @ *} t.tep)  ~
    =+  nag=(mix i.t.tep (lsh 3 1 i.t.t.tep))
    =+  val=(rush nag hex:ag)
    ?~  val  ~
    =+  nex=$(tep t.t.t.tep)
    ?~(nex ~ [~ [`@`u.val u.nex]])
  =+  nex=$(tep t.tep)
  ?~(nex ~ [~ i.tep u.nex])
::
++  earf                                                ::  purf to tape
  |=  purf
  (weld (earn p) ?~(q "" `tape`['#' (trip u.q)]))
::
++  earl                                                ::  localize purl
  |=  {who/@p pul/purl}
  ^-  purl
  pul(q.q [(rsh 3 1 (scot %p who)) q.q.pul])
::
++  earn                                                ::  purl to tape
  =<  |=(pul/purl `tape`(apex %& pul))
  |%
  ++  apex
    |=  qur/quri
    ?-  -.qur
      $&  (weld (head p.p.qur) `tape`['/' $(qur [%| +.p.qur])])
      $|  (weld (body p.qur) (tail q.qur))
    ==
  ::
  ++  body
    |=  pok/pork  ^-  tape
    ?~  q.pok  ~
    |-
    =+  seg=(trip i.q.pok)
    ?~  t.q.pok
      ?~(p.pok seg (welp seg '.' (trip u.p.pok)))
    (welp seg '/' $(q.pok t.q.pok))
  ::
  ++  head
    |=  har/hart
    ^-  tape
    ;:  weld
      ?:(&(p.har !?=(hoke r.har)) "https://" "http://")
    ::
      ?-  -.r.har
        $|  (trip (rsh 3 1 (scot %if p.r.har)))
        $&  =+  rit=(flop p.r.har)
            |-  ^-  tape
            ?~(rit ~ (weld (trip i.rit) ?~(t.rit "" `tape`['.' $(rit t.rit)])))
      ==
    ::
      ?~(q.har ~ `tape`[':' (trip (rsh 3 2 (scot %ui u.q.har)))])
    ==
  ::
  ++  tail
    |=  kay/quay
    ^-  tape
    ?:  =(~ kay)  ~
    :-  '?'
    |-  ^-  tape
    ?~  kay  ~
    ;:  welp
      (urle (trip p.i.kay))
      ?~(q.i.kay ~ ['=' (urle (trip q.i.kay))])
      ?~(t.kay ~ `tape`['&' $(kay t.kay)])
    ==
  --
::
++  urlp                                                ::  url+header parser
  |%
  ++  apat                                              ::  2396 abs_path
    %+  cook  deft
    (ifix [fas ;~(pose fas (easy ~))] (more fas smeg))
  ++  aurf                                              ::  2396 with fragment
    %+  cook  |+(a/purf a)
    ;~(plug auri (punt ;~(pfix hax (cook crip (star pque)))))
  ++  auri                                              ::  2396 URL
    %+  cook
      |=  a/purl
      ?.(?=(hoke r.p.a) a a(p.p &))
    ;~  plug
      ;~  plug
        %+  sear
          |=  a/@t
          ^-  (unit ?)
          ?+(a ~ $http [~ %|], $https [~ %&])
        ;~(sfix scem ;~(plug col fas fas))
        thor
      ==
      ;~(plug ;~(pose apat (easy *pork)) yque)
    ==
  ++  cock                                              ::  cookie
    (most ;~(plug sem ace) ;~(plug toke ;~(pfix tis tosk)))
  ++  dlab                                              ::  2396 domainlabel
    %+  sear
      |=  a/@ta
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
  ++  pquo  ;~(pose pure pesc pold fas wut)             ::  normal query char
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
  ++  thor                                              ::  2396 host+port
    %+  cook  |*({* *} [+<+ +<-])
    ;~  plug
      thos
      ;~((bend) (easy ~) ;~(pfix col dim:ag))
    ==
  ++  thos                                              ::  2396 host, no local
    ;~  plug
      ;~  pose
        %+  stag  %&
        %+  sear                                        ::  LL parser weak here
          |=  a/(list @t)
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
        |=(a/tape [[%$ (crip a)] ~])
      (star pque)
    ==
  ++  zest                                              ::  2616 request-uri
    ;~  pose
      (stag %& (cook |=(a/purl a) auri))
      (stag %| ;~(plug apat yque))
    ==
  --
++  epur                                                ::  url+header parser
  =>(urlp |=(a/cord `(unit purl)`(rush a auri)))
::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bH, names etc                ::
::
++  clan                                                ::  ship to rank
  |=  who/ship  ^-  rank
  =+  wid=(met 3 who)
  ?:  (lte wid 1)   %czar
  ?:  =(2 wid)      %king
  ?:  (lte wid 4)   %duke
  ?:  (lte wid 8)   %earl
  ?>  (lte wid 16)  %pawn
::
++  glam                                                ::  carrier names
  |=  zar/@pD  ^-  tape
  (weld "galaxy " (scow %p zar))
::
++  gnom                                                ::  ship display name
  |=  {{our/@p now/@da} him/@p}  ^-  @t
  =+  yow=(scot %p him)
  =+  pax=[(scot %p our) %name (scot %da now) yow ~]
  =+  woy=((hard @t) .^(%a pax))
  ?:  =(%$ woy)  yow
  (rap 3 yow ' ' woy ~)
::
++  gnow
  |=  {who/@p gos/gcos}  ^-  @t
  ?-    -.gos
      $czar                 (rap 3 '|' (rap 3 (glam who)) '|' ~)
      $king                 (rap 3 '_' p.gos '_' ~)
      $earl                 (rap 3 ':' p.gos ':' ~)
      $pawn                 ?~(p.gos %$ (rap 3 '.' u.p.gos '.' ~))
      $duke
    ?:  ?=($anon -.p.gos)  %$
    %+  rap  3
    ^-  (list @)
    ?-    -.p.gos
        $punk  ~['"' q.p.gos '"']
        ?($lord $lady)
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
++  pale                                                ::  filter peers
  |=  {hid/bowl fun/$+(sink ?)}
  (skim (~(tap by sup.hid)) fun)
::
++  prix                                                ::  filter gate
  |=  pax/path  |=  sink  ^-  ?
  ?~  pax  &  ?~  r.+<  | 
  &(=(i.pax i.r.+<) $(pax t.pax, r.+< t.r.+<))
::
++  prey  |=({pax/path hid/bowl} (pale hid (prix pax))) ::  prefix 
++  hunt                                                ::  first of unit dates
  |=  {one/(unit @da) two/(unit @da)}
  ^-  (unit @da)
  ?~  one  two
  ?~  two  one
  ?:((lth u.one u.two) one two)
::
:: ++  mojo                                                ::  compiling load
::   |=  [pax=path src=*]
::   ^-  (each twig (list tank))
::   ?.  ?=(@ src)
::     [%| ~[[leaf+"musk: malformed: {<pax>}"]]]
::   =+  ^=  mud
::       %-  mule  |.
::       ((full vest) [1 1] (trip src))
::   ?:  ?=(| -.mud)  mud
::   ?~  q.p.mud
::     :~  %|
::         leaf+"musk: syntax error: {<pax>}"
::         leaf+"musk: line {<p.p.p.mud>}, column {<q.p.p.mud>}"
::     ==
::   [%& p.u.q.p.mud]
:: ::
++  mole                                                ::  new to old sky
  |=  ska/$+(* (unit (unit)))
  |=  a/*
  ^-  (unit)
  =+  b=(ska a)
  ?~  b  ~
  ?~  u.b  ~
  [~ u.u.b]
:: ::
:: ++  much                                                ::  constructing load
::   |=  [pax=path src=*]
::   ^-  gank
::    =+  moj=(mojo pax src)
::   ?:  ?=(| -.moj)  moj
::   (mule |.((slap !>(+>.$) `twig`p.moj)))
:: ::
:: ++  musk                                                ::  compiling apply
::   |=  [pax=path src=* sam=vase]
::   ^-  gank
::   =+  mud=(much pax src)
::   ?:  ?=(| -.mud)  mud
::   (mule |.((slam p.mud sam)))
:: ::
++  pack                                                ::  light path encoding
  |=  {a/term b/path}  ^-  knot
  %+  rap  3  :-  (wack a)
  (turn b |=(c/knot (cat 3 '_' (wack c))))
::
++  puck                                                ::  light path decoding
  =+  fel=(most cab (sear wick urt:ab))
  |=(a/knot `(unit {p/term q/path})`(rush a fel))
::
++  saxo                                                ::  autocanon
  |=  who/ship
  ^-  (list ship)
  ?:  (lth who 256)  [who ~]
  [who $(who (sein who))]
::
++  sein                                                ::  autoboss
  |=  who/ship  ^-  ship
  =+  mir=(clan who)
  ?-  mir
    $czar  who
    $king  (end 3 1 who)
    $duke  (end 4 1 who)
    $earl  (end 5 1 who)
    $pawn  `@p`0
  ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bI, Arvo structures          ::
::
++  acru                                                ::  asym cryptosuite
          $_  ^?  |%                                    ::  opaque object
          ++  as  ^?                                    ::  asym ops
            |%  ++  seal  |+({a/pass b/@ c/@} *@)       ::  encrypt to a
                ++  sign  |+({a/@ b/@} *@)              ::  certify as us
                ++  sure  |+({a/@ b/@} *(unit @))       ::  authenticate from us
                ++  tear  |+  {a/pass b/@}              ::  accept from a 
                          *(unit {p/@ q/@})             ::
            --                                          ::
          ++  de  |+({a/@ b/@} *(unit @))               ::  symmetric de, soft
          ++  dy  |+({a/@ b/@} *@)                      ::  symmetric de, hard
          ++  en  |+({a/@ b/@} *@)                      ::  symmetric en
          ++  ex  ^?                                    ::  export
            |%  ++  fig  *@uvH                          ::  fingerprint
                ++  pac  *@uvG                          ::  default passcode
                ++  pub  *pass                          ::  public key
                ++  sec  *ring                          ::  private key
            --                                          ::
          ++  nu  ^?                                    ::  reconstructors
             |%  ++  pit  |+({a/@ b/@} ^?(..nu))        ::  from [width seed]
                 ++  nol  |+(a/@ ^?(..nu))              ::  from naked ring
                 ++  com  |+(a/@ ^?(..nu))              ::  from naked pass
             --                                         ::
          --                                            ::
++  aeon  @ud                                          ::
++  agon  (map {p/ship q/desk} {p/@ud q/@ud r/waks})    ::  mergepts
++  ankh                                                ::  fs node (new)
          $:  fil/(unit {p/lobe q/cage})                ::  file
              dir/(map @ta ankh)                        ::  folders
          ==                                            ::
++  apex  {p/@uvI q/(map @ta @uvI) r/(map @ta $~)}      ::  node report (old)
++  ares  (unit {p/term q/(list tank)})                 ::  possible error
++  ball  @uw                                           ::  statement payload
++  bait  {p/skin q/@ud r/dove}                         ::  fmt nrecvd spec
++  bath                                                ::  convo per client
          $:  sop/shed                                  ::  not stalled
              raz/(map path race)                       ::  statements inbound
              ryl/(map path rill)                       ::  statements outbound
          ==                                            ::
++  beam  {{p/ship q/desk r/case} s/path}               ::  global name
++  beak  {p/ship q/desk r/case}                        ::  garnish with beak
++  bird                                                ::  packet in travel
          $:  gom/soap                                  ::  message identity
              mup/@ud                                   ::  pktno in msg
              nux/@ud                                   ::  xmission count
              lys/@da                                   ::  last sent
              pac/rock                                  ::  packet data
          ==                                            ::
++  bitt  (map bone (pair ship path))                   ::  incoming subs
++  blob                                                ::  fs blob
          $%  {$delta p/lobe q/{p/mark q/lobe} r/page}  ::  delta on q
              {$direct p/lobe q/page}                   ::  immediate
          ==                                            ::
++  boat  (map (pair bone wire) (trel bean ship path))  ::  outgoing subs
++  boon                                                ::  fort output
          $%  {$beer p/ship q/@uvG}                     ::  gained ownership
              {$cake p/sock q/soap r/coop s/duct}       ::  e2e message result
              {$coke p/sock q/soap r/cape s/duct}       ::  message result
              {$mead p/lane q/rock}                     ::  accept packet
              {$milk p/sock q/soap r/*}                 ::  accept message
              {$mulk p/sock q/soap r/*}                 ::  e2e pass message
              {$ouzo p/lane q/rock}                     ::  transmit packet
              {$wine p/sock q/tape}                     ::  notify user
          ==                                            ::
++  bowl                                                ::  standard app state
        $:  $:  our/ship                                ::  host
                src/ship                                ::  guest
                dap/term                                ::  agent
            ==                                          ::  
            $:  wex/boat                                ::  outgoing subs
                sup/bitt                                ::  incoming subs
            ==                                          ::
            $:  ost/bone                                ::  opaque cause
                act/@ud                                 ::  change number
                eny/@uvI                                ::  entropy
                now/@da                                 ::  current time
                byk/beak                                ::  load source
        ==  ==                                          ::
++  bray  {p/life q/(unit life) r/ship s/@da}           ::  our parent us now
++  brow  {p/@da q/@tas}                                ::  browser version
++  buck  {p/mace q/will}                               ::  all security data
++  cake  {p/sock q/skin r/@}                           ::  top level packet
++  cape                                                ::  end-to-end result
          $?  $good                                     ::  delivered
              $dead                                     ::  rejected
          ==                                            ::
++  care  ?($u $v $w $x $y $z)                          ::  clay submode
++  case                                                ::  ship desk case spur
          $%  {$da p/@da}                               ::  date
              {$tas p/@tas}                             ::  label
              {$ud p/@ud}                               ::  number
          ==                                            ::
++  chum  @uvI                                          ::  hashed passcode
++  clot                                                ::  symmetric record
          $:  yed/(unit {p/hand q/code})                ::  outbound
              heg/(map hand code)                       ::  proposed
              qim/(map hand code)                       ::  inbound
          ==                                            ::
++  coal  *                                             ::  untyped vase
++  code  @uvI                                          ::  symmetric key
++  cone                                                ::  reconfiguration
          $%  {$& p/twig}                               ::  transform
              {$| p/(list @tas)}                        ::  alter
          ==                                            ::
++  chum  @uvI                                          ::  hashed passcode
++  claw                                                ::  startup chain
          $:  joy/(unit coal)                           ::  local context
              ran/(unit coal)                           ::  arguments
              pux/(unit path)                           ::  execution path
              jiv/(unit coal)                           ::  app configuration
              kyq/(unit coal)                           ::  app customization
              gam/(unit coal)                           ::  app image
          ==                                            ::
++  clip  (each @if @is)                                ::  client IP
++  coal  *                                             ::  untyped vase
++  code  @uvI                                          ::  symmetric key
++  cone                                                ::  reconfiguration
          $%  {$& p/twig}                                ::  transform
              {$| p/(list @tas)}                        ::  alter
          ==                                            ::
++  coop  (unit ares)                                   ::  e2e ack
++  corn                                                ::  flow by server
          $:  hen/duct                                  ::  admin channel
              nys/(map flap bait)                       ::  packets incoming
              olz/(map flap cape)                       ::  packets completed
              wab/(map ship bath)                       ::  relationship
          ==                                            ::
++  cred                                                ::  credential
          $:  hut/hart                                  ::  client host
              aut/(jug @tas @t)                         ::  client identities
              orx/oryx                                  ::  CSRF secret
              acl/(unit @t)                             ::  accept-language
              cip/(each @if @is)                        ::  client IP
              cum/(map @tas *)                          ::  custom dirt
          ==                                            ::
++  deed  {p/@ q/step r/?}                              ::  sig stage fake?
++  dome                                                ::  project state
          $:  ank/ankh                                  ::  state
              let/@ud                                   ::  top id
              hit/(map @ud tako)                        ::  changes by id
              lab/(map @tas @ud)                        ::  labels
          ==                                            ::
++  dore                                                ::  foreign contact
          $:  wod/road                                  ::  connection to
              wyl/will                                  ::  inferred mirror
              caq/clot                                  ::  symmetric key state
          ==                                            ::
++  dove  {p/@ud q/(map @ud @)}                         ::  count hash 13-blocks
++  epic                                                ::  FCGI parameters
          $:  qix/(map @t @t)                           ::  query
              ced/cred                                  ::  client credentials
              bem/beam                                  ::  original path
              but/path                                  ::  ending
          ==                                            ::
++  flap  @uvH                                          ::  network packet id
++  flow                                                ::  packet connection
          $:  rtt/@dr                                   ::  decaying avg rtt
              wid/@ud                                   ::  logical wdow msgs
          ==                                            ::
++  fort                                                ::  formal state
          $:  $0                                        ::  version
              gad/duct                                  ::  client interface
              hop/@da                                   ::  network boot date
              ton/town                                  ::  security
              zac/(map ship corn)                       ::  flows by server
          ==                                            ::
++  gank  (each vase (list tank))                       ::  abstract result
++  gilt  {@tas *}                                      ::  presumed gift
++  gens  {p/lang q/gcos}                               ::  general identity
++  germ                                                ::  merge style
          $?  $init                                     ::  new desk
              $this                                     ::  ours with parents
              $that                                     ::  hers with parents
              $fine                                     ::  fast forward
              $meet                                     ::  orthogonal files
              $mate                                     ::  orthogonal changes
              $meld                                     ::  force merge
          ==                                            ::
++  gcos                                                ::  id description
          $%  {$czar $~}                                ::  8-bit ship
              {$duke p/what}                            ::  32-bit ship
              {$earl p/@t}                              ::  64-bit ship
              {$king p/@t}                              ::  16-bit ship
              {$pawn p/(unit @t)}                       ::  128-bit ship
          ==                                            ::
++  govt  path                                          ::  country+postcode
++  hand  @uvH                                          ::  hash of code
++  hart  {p/? q/(unit @ud) r/host}                     ::  http sec+port+host
++  hate  {p/purl q/@p r/moth}                          ::  semi-cooked request
++  heir  {p/@ud q/mess r/(unit love)}                  ::  status+headers+data
++  hiss  {p/purl q/moth}                               ::  outbound request
++  hist  {p/@ud q/(list @t)}                           ::  depth texts
++  hole  @t                                            ::  session identity
++  hort  {p/(unit @ud) q/host}                         ::  http port+host
++  host  (each (list @t) @if)                          ::  http host
++  hoke  %+  each   {$localhost $~}                    ::  local host
          ?($.0.0.0.0 $.127.0.0.1)                      ::
++  httq                                                ::  raw http request
          $:  p/meth                                    ::  method
              q/@t                                      ::  unparsed url
              r/(list {p/@t q/@t})                      ::  headers
              s/(unit octs)                             ::  body
          ==                                            ::
++  httr  {p/@ud q/mess r/(unit octs)}                  ::  raw http response
++  httx                                                ::  encapsulated http
          $:  p/?                                       ::  https?
              q/clip                                    ::  source IP
              r/httq                                    ::
          ==                                            ::
++  kite  {p/care q/case r/ship s/desk t/spur}          ::  parsed global name
++  json                                                ::  normal json value
          $@  $~                                        ::  null
          $%  {$a p/(list json)}                        ::  array
              {$b p/?}                                  ::  boolean
              {$o p/(map @t json)}                      ::  object
              {$n p/@ta}                                ::  number
              {$s p/@t}                                 ::  string
          ==                                            ::
++  lamb                                                ::  short path
          $%  {$& p/@tas}                               ::  auto
              {$| p/twig}                               ::  manual
          ==                                            ::
++  lane                                                ::  packet route
          $%  {$if p/@da q/@ud r/@if}                   ::  IP4/public UDP/addr
              {$is p/@ud q/(unit lane) r/@is}           ::  IPv6 w+alternates
              {$ix p/@da q/@ud r/@if}                   ::  IPv4 provisional
          ==                                            ::
++  lang  @ta                                           ::  IETF lang as code
++  lark  {p/(unit @tas) q/lawn}                        ::  parsed command
++  lass  ?($0 $1 $2)                                   ::  power increment
++  lath  $%                                            ::  pipeline stage
              {$0 p/lass q/lamb r/(list cone) s/twig}   ::  command
              {$1 p/twig}                               ::  generator
              {$2 p/twig}                               ::  filter
          ==                                            ::
++  lawn  (list lath)                                   ::
++  lice  {p/ship q/buck}                               ::  full license
++  life  @ud                                           ::  regime number
++  lint  (list rock)                                   ::  fragment array
++  lobe  @uvI                                          ::  blob ref
++  love  $%                                            ::  http response
              {$ham p/manx}                             ::  html node
              {$mid p/mite q/octs}                      ::  mime-typed data
              {$raw p/httr}                             ::  raw http response
              {$wan p/wain}                             ::  text lines
              {$zap p/@ud q/(list tank)}                ::  status+error
          ==                                            ::
++  maki  {p/@ta q/@ta r/@ta s/path}                    ::
++  mace  (list {p/life q/ring})                        ::  private secrets
++  math  (map @t (list @t))                            ::  semiparsed headers
++  meal                                                ::  payload
          $%  {$back p/cape q/flap r/@dr}               ::  acknowledgment
              {$buck p/coop q/flap r/@dr}               ::  e2e ack
              {$bond p/life q/path r/@ud s/*}           ::  message
              {$bund p/life q/path r/@ud s/*}           ::  e2e message
              {$carp p/@ q/@ud r/@ud s/flap t/@}        ::  skin+inx+cnt+hash
              {$fore p/ship q/(unit lane) r/@}          ::  forwarded packet
          ==                                            ::
++  mess  (list {p/@t q/@t})                            ::  raw http headers
++  meta                                                ::  path metadata
          $%  {$& q/@uvI}                               ::  hash
              {$| q/(list @ta)}                         ::  dir
          ==                                            ::
++  meth                                                ::  http methods
          $?  $conn                                     ::  CONNECT
              $delt                                     ::  DELETE
              $get                                      ::  GET
              $head                                     ::  HEAD
              $opts                                     ::  OPTIONS
              $post                                     ::  POST
              $put                                      ::  PUT
              $trac                                     ::  TRACE
          ==                                            ::
++  mime  {p/mite q/octs}                               ::  mimetyped data
++  mite  (list @ta)                                    ::  mime type
++  miso                                                ::  ankh delta
          $%  {$del $~}                                 ::  delete
              {$ins p/cage}                             ::  insert
              {$dif p/cage}                             ::  mutate from diff
              {$mut p/cage}                             ::  mutate from raw
          ==                                            ::
++  misu                                                ::  computed delta
          $%  {$del $~}                                 ::  delete
              {$ins p/cage}                             ::  insert
              {$dif p/lobe q/cage}                      ::  mutate from diff
          ==                                            ::
++  mizu  {p/@u q/(map @ud tako) r/rang}                ::  new state
++  moar  {p/@ud q/@ud}                                 ::  normal change range
++  moat  {p/case q/case r/path}                        ::  change range
++  mood  {p/care q/case r/path}                        ::  request in desk
++  moth  {p/meth q/math r/(unit octs)}                 ::  http operation
++  name  {p/@t q/(unit @t) r/(unit @t) s/@t}           ::  first mid+nick last
++  newt  ?($boot $kick $mess $slay $wake)              ::  lifecycle events
++  nori                                                ::  repository action
          $%  {$& p/soba}                               ::  delta
              {$| p/@tas}                               ::  label
          ==                                            ::
++  nuri                                                ::  repository action
          $%  {$& p/suba}                               ::  delta
              {$| p/@tas}                               ::  label
          ==                                            ::
++  octs  {p/@ud q/@t}                                  ::  octet-stream
++  oryx  @t                                            ::  CSRF secret
++  page  (cask)                                        ::  untyped cage
++  pail  ?($none $warm $cold)                          ::  connection status
++  plan  (trel view (pair @da (unit @dr)) path)        ::  subscription
++  plea  {p/@ud q/{p/? q/@t}}                          ::  live prompt
++  plop  blob                                          ::  unvalidated blob
++  pork  {p/(unit @ta) q/(list @t)}                    ::  fully parsed url
++  pred  {p/@ta q/@tas r/@ta $~}                       ::  proto-path
++  prod  {p/prom q/tape r/tape}                        ::  prompt
++  prom  ?($text $pass $none)                          ::  format type
++  purf  (pair purl (unit @t))                         ::  url with fragment
++  purl  {p/hart q/pork r/quay}                        ::  parsed url
++  putt                                                ::  outgoing message
          $:  ski/snow                                  ::  sequence acked+sent
              wyv/(list rock)                           ::  packet list XX gear
          ==                                            ::
++  pyre                                                ::  cascade stash
          $:  p/(map {p/path q/path r/coal} coal)       ::  by path
              q/(map {p/path q/@uvI r/coal} coal)       ::  by source hash
              r/(map {p/* q/coal} coal)                 ::  by (soft) twig
          ==                                            ::
++  quay  (list {p/@t q/@t})                            ::  parsed url query
++  quri                                                ::  request-uri
          $%  {$& p/purl}                               ::  absolute
              {$| p/pork q/quay}                        ::  relative
          ==                                            ::
++  race                                                ::  inbound stream
          $:  did/@ud                                   ::  filled sequence
              dod/?                                     ::  not processing
              bum/(map @ud ares)                        ::  nacks
              mis/(map @ud {p/cape q/lane r/flap s/(unit)}) ::  misordered
          ==                                            ::
++  rank  ?($czar $king $duke $earl $pawn)              ::  ship width class
++  rang  $:  hut/(map tako yaki)                       ::
              lat/(map lobe blob)                       ::
          ==                                            ::
++  rand                                                ::  unvalidated rant
          $:  p/{p/care q/case r/@tas}                  ::  clade release book
              q/path                                    ::  spur
              r/page                                    ::  data
          ==                                            ::
++  rant                                                ::  namespace binding
          $:  p/{p/care q/case r/@tas}                  ::  clade release book
              q/path                                    ::  spur
              r/cage                                    ::  data
          ==                                            ::
++  rave                                                ::  general request
          $%  {$sing p/mood}                            ::  single request
              {$next p/mood}                            ::  next version
              {$many p/? q/moat}                        ::  change range
          ==                                            ::
++  rill                                                ::  outbound stream
          $:  sed/@ud                                   ::  sent
              san/(map @ud duct)                        ::  outstanding
          ==                                            ::
++  riot  (unit rant)                                   ::  response+complete
++  road                                                ::  secured oneway route
          $:  exp/@da                                   ::  expiration date
              lun/(unit lane)                           ::  route to friend
              lew/will                                  ::  will of friend
          ==                                            ::
++  rock  @uvO                                          ::  packet
++  rout  {p/(list host) q/path r/oryx s/path}          ::  http route (new)
++  rump  {p/care q/case r/@tas s/path}                 ::  relative path
++  saba  {p/ship q/@tas r/moar s/dome}                 ::  patch+merge
++  sack  {p/ship q/ship}                               ::  incoming [our his}
++  sufi                                                ::  domestic host
          $:  hoy/(list ship)                           ::  hierarchy
              val/wund                                  ::  private keys
              law/will                                  ::  server will
              seh/(map hand {p/ship q/@da})             ::  key cache
              hoc/(map ship dore)                       ::  neighborhood
          ==                                            ::
++  salt  @uv                                           ::  entropy
++  seal                                                ::  auth conversation
          $:  whu/(unit ship)                           ::  client identity
              pul/purl                                  ::  destination url
              wit/?                                     ::  wait for partner
              foy/(unit {p/ship q/hole})                ::  partner to notify
              pus/(unit @ta)                            ::  password
          ==                                            ::
++  sect  ?($black $blue $red $orange $white)           ::  banner
++  shed                                                ::  packet flow
          $:  $:  rtt/@dr                               ::  smoothed rtt
                  rto/@dr                               ::  retransmit timeout
                  rtn/(unit @da)                        ::  next timeout
                  rue/(unit @da)                        ::  last heard from
              ==                                        ::
              $:  nus/@ud                               ::  number sent
                  nif/@ud                               ::  number live
                  nep/@ud                               ::  next expected
                  caw/@ud                               ::  logical window
                  cag/@ud                               ::  congest thresh
              ==                                        ::
              $:  diq/(map flap @ud)                    ::  packets sent
                  pyz/(map soup @ud)                    ::  message+unacked
                  puq/(qeu {p/@ud q/soul})              ::  packet queue
              ==                                        ::
          ==                                            ::
++  skit  {p/(unit @ta) q/(list @ta) r/(list @ta)}      ::  tracking path
++  skin  ?($none $open $fast $full)                    ::  encoding stem
++  snow  {p/@ud q/@ud r/(set @ud)}                     ::  window exceptions
++  soap  {p/{p/life q/life} q/path r/@ud}              ::  statement id
++  soup  {p/path q/@ud}                                ::  new statement id
++  soul                                                ::  packet in travel
          $:  gom/soup                                  ::  message identity
              nux/@ud                                   ::  xmission count
              liv/?                                     ::  deemed live
              lys/@da                                   ::  last sent
              pac/rock                                  ::  packet data
          ==                                            ::
++  soba  (list {p/path q/miso})                        ::  delta
++  sock  {p/ship q/ship}                               ::  outgoing [from to]
++  spur  path                                          ::  ship desk case spur
++  step  {p/bray q/gens r/pass}                        ::  identity stage
++  suba  (list {p/path q/misu})                        ::  delta
++  tako  @                                             ::  yaki ref
++  tick  @ud                                           ::  process id
++  toro  {p/@ta q/nori}                                ::  general change
++  town                                                ::  all security state
          $:  lit/@ud                                   ::  imperial modulus
              any/@                                     ::  entropy
              urb/(map ship sufi)                       ::  all keys and routes
              fak/?                                     ::
          ==                                            ::
++  tube  {p/@ta q/@ta r/@ta s/path}                    ::  canonical path
++  tutu  *                                             ::  presumed type
++  yaki  {p/(list tako) q/(map path lobe) r/tako t/@da} ::  commit
++  view  ?($u $v $w $x $y $z)                          ::  view mode
++  waks  (map path woof)                               ::  list file states
++  what                                                ::  logical identity
          $%  {$anon $~}                                ::  anonymous
              {$lady p/whom}                            ::  female person ()
              {$lord p/whom}                            ::  male person []
              {$punk p/sect q/@t}                       ::  opaque handle ""
          ==                                            ::
++  whom  {p/@ud q/govt r/sect s/name}                  ::  year+govt+id
++  woof  $@  $know                                     ::  udon transform
              {$chan (list {$@(@ud {p/@ud q/@ud})})}    ::
++  wund  (list {p/life q/ring r/acru})                 ::  mace in action
++  will  (list deed)                                   ::  certificate
++  zuse  %310                                          ::  hoon+zuse kelvin
::          ::
::::        ::::  this will become `%york`, vane structures.
  ::          ::
++  gift-ames                                           ::  out result <-$
          $%  {$hear p/lane q/@}                        ::  receive packet
              {$init p/@p}                              ::  report install
              {$mass p/mass}                            ::  memory usage
              {$send p/lane q/@}                        ::  transmit packet
              {$waft p/sock q/*}                        ::  response message
              {$wart p/sock q/@tas r/path s/*}          ::  network request
              {$went p/ship q/cape}                     ::  reaction message
              {$woot p/ship q/coop}                     ::  e2e reaction message
          ==                                            ::
++  kiss-ames                                           ::  in request ->$
          $%  {$barn $~}                                ::  new unix process
              {$crud p/@tas q/(list tank)}              ::  error with trace
              {$cash p/@p q/buck}                       ::  civil license
              {$hear p/lane q/@}                        ::  receive packet
              {$hole p/lane q/@}                        ::  packet failed
              {$junk p/@}                               ::  entropy
              {$kick p/@da}                             ::  wake up
              {$make p/(unit @t) q/@ud r/@ s/?}         ::  wild license
              {$sith p/@p q/@uw r/?}                    ::  imperial generator
              {$wake $~}                                ::  timer activate
              {$want p/sock q/path r/*}                 ::  send message
              {$wegh $~}                                ::  report memory
              {$west p/sack q/path r/*}                 ::  network request
              {$wont p/sock q/path r/*}                 ::  e2e send message
          ==                                            ::
::
::::    %behn
  ::
++  gift-behn                                           ::  out result <-$
          $%  {$mass p/mass}                            ::  memory usage
              {$wake $~}                                ::  wakey-wakey
          ==                                            ::
++  kiss-behn                                           ::  in request ->$
          $%  {$rest p/@da}                             ::  cancel alarm
              {$wait p/@da}                             ::  set alarm
              {$wake $~}                                ::  timer activate
              {$wegh $~}                                ::  report memory
          ==                                            ::
::
::::    %clay
  ::
++  khan                                                ::
          $:  fil/(unit (unit cage))                    ::  XX see khan-to-soba
              dir/(unit (map @ta (unit khan)))          ::
          ==                                            ::
++  mode  (list {path (unit mime)})                     ::
++  riff  {p/desk q/(unit rave)}                        ::  request+desist
::::                                                    ::
++  gift-clay                                           ::  out result <-$
          $%  {$ergo p/@tas q/mode}                     ::  version update
              {$hill p/(list @tas)}                     ::  mount points
              {$mack p/(unit tang)}                     ::  ack
              {$mass p/mass}                            ::  memory usage
              {$mere p/(each (set path) (pair term tang))}  ::  merge result
              {$note p/@tD q/tank}                      ::  debug message
              {$ogre p/@tas}                            ::  delete mount point
              {$writ p/riot}                            ::  response
          ==                                            ::
++  kiss-clay                                           ::  in request ->$
          $%  {$boat $~}                                ::  pier rebooted
              {$drop p/@p q/@tas}                       ::  cancel pending merge
              {$info p/@p q/@tas r/nori}                ::  internal edit
              {$init p/@p}                              ::  report install
              {$into p/@tas q/? r/mode}                 ::  external edit
              {$merg p/@p q/@tas r/@p s/@tas t/case u/germ}  ::  merge desks
              {$mont p/@tas q/@p r/@tas s/path}         ::  mount to unix
              {$ogre p/$@(@tas beam)}                   ::  delete mount point
              {$warp p/sock q/riff}                     ::  file request
              {$wegh $~}                                ::  report memory
              {$west p/sack q/path r/*}                 ::  network request
          ==                                            ::
::
::::
  ::
++  blew  {p/@ud q/@ud}                                 ::  columns rows
++  belt                                                ::  old belt
  $%  {$aro p/?($d $l $r $u)}                           ::  arrow key
      {$bac $~}                                         ::  true backspace
      {$ctl p/@c}                                       ::  control-key
      {$del $~}                                         ::  true delete
      {$met p/@c}                                       ::  meta-key
      {$ret $~}                                         ::  return
      {$txt p/(list @c)}                                ::  utf32 text
  ==                                                    ::
++  blit                                                ::  old blit
  $%  {$bel $~}                                         ::  make a noise
      {$clr $~}                                         ::  clear the screen
      {$hop p/@ud}                                      ::  set cursor position
      {$lin p/(list @c)}                                ::  set current line
      {$mor $~}                                         ::  newline
      {$sag p/path q/*}                                 ::  save to jamfile
      {$sav p/path q/@}                                 ::  save to file
      {$url p/@t}                                       ::  activate url
  ==                                                    ::
++  dill-belt                                           ::  new belt
  $%  {$aro p/?($d $l $r $u)}                           ::  arrow key
      {$bac $~}                                         ::  true backspace
      {$cru p/@tas q/(list tank)}                       ::  echo error
      {$ctl p/@}                                        ::  control-key
      {$del $~}                                         ::  true delete
      {$hey $~}                                         ::  refresh
      {$met p/@}                                        ::  meta-key
      {$ret $~}                                         ::  return
      {$rez p/@ud q/@ud}                                ::  resize, cols, rows
      {$txt p/(list @c)}                                ::  utf32 text
      {$yow p/gill}                                     ::  connect to app
  ==                                                    ::
++  dill-blit                                           ::  new blit
  $%  {$bel $~}                                         ::  make a noise
      {$clr $~}                                         ::  clear the screen
      {$hop p/@ud}                                      ::  set cursor position
      {$mor p/(list dill-blit)}                         ::  multiple blits
      {$pro p/(list @c)}                                ::  show as cursor+line
      {$qit $~}                                         ::  close console
      {$out p/(list @c)}                                ::  send output line
      {$sag p/path q/*}                                 ::  save to jamfile
      {$sav p/path q/@}                                 ::  save to file
      {$url p/@t}                                       ::  activate url
  ==                                                    ::
++  flog                                                ::  sent to %dill
  $%  {$crud p/@tas q/(list tank)}                      ::
      {$heft $~}                                        ::
      {$text p/tape}                                    ::
      {$veer p/@ta q/path r/@t}                         ::  install vane
      {$vega p/path}                                    ::  reboot by path
      {$verb $~}                                        ::  verbose mode
  ==                                                    ::
++  gill  (pair ship term)                              ::  general contact
::::
++  gift-dill                                           ::  out result <-$
  $%  {$bbye $~}                                        ::  reset prompt
      {$blit p/(list blit)}                             ::  terminal output
      {$burl p/@t}                                      ::  activate url
      {$init p/@p}                                      ::  set owner
      {$logo $~}                                        ::  logout
      {$mass p/mass}                                    ::  memory usage
      {$veer p/@ta q/path r/@t}                         ::  install vane
      {$vega p/path}                                    ::  reboot by path
      {$verb $~}                                        ::  verbose mode
  ==                                                    ::
++  kiss-dill                                           ::  in request ->$
  $%  {$belt p/belt}                                    ::  terminal input
      {$blew p/blew}                                    ::  terminal config
      {$boot p/*}                                       ::  weird %dill boot
      {$crud p/@tas q/(list tank)}                      ::  error with trace
      {$flog p/flog}                                    ::  wrapped error
      {$flow p/@tas q/(list gill)}                      ::  terminal config
      {$hail $~}                                        ::  terminal refresh
      {$heft $~}                                        ::  memory report
      {$hook $~}                                        ::  this term hung up
      {$harm $~}                                        ::  all terms hung up
      {$init p/ship}                                    ::  after gall ready
      {$tick p/@p q/@p}                                 ::  initial ticket
      {$noop $~}                                        ::  no operation
      {$talk p/tank}                                    ::
      {$text p/tape}                                    ::
      {$veer p/@ta q/path r/@t}                         ::  install vane
      {$vega p/path}                                    ::  reboot by path
      {$verb $~}                                        ::  verbose mode
  ==                                                    ::
::
::::    %eyre
  ::
++  gram                                                ::  inter-ship message
  =+  fuz={path *}                                      ::  ames format
  $?  {{$lon $~} p/hole}                                ::  login request
      {{$aut $~} p/hole}                                ::  login reply
      {{$hat $~} p/hole q/hart}                         ::  login redirect
      {{$get $~} p/@uvH q/{? clip httq}}                ::  remote request
      {{$got $~} p/@uvH q/httr}                         ::  remote response
      {{$gib $~} p/@uvH}                                 ::  remote cancel
  ==                                                    ::
::::                                                    ::
++  kiss-eyre                                           ::  in request ->$
          $%  {$born $~}                                ::  new unix process
              {$crud p/@tas q/(list tank)}              ::  XX rethink
              {$hiss p/mark q/cage}                     ::  outbound user req
              {$init p/@p}                              ::  report install
              {$them p/(unit hiss)}                     ::  outbound request
              {$they p/@ud q/httr}                      ::  inbound response
              {$this p/? q/clip r/httq}                 ::  inbound request
              {$thud $~}                                ::  inbound cancel
              {$wegh $~}                                ::  report memory
              {$west p/sack q/fuz:gram}                 ::  network request
          ==                                            ::
++  gift-eyre                                           ::  out result <-$
          $%  {$mass p/mass}                            ::  memory usage
              {$mack p/(unit tang)}                     ::  message ack
              {$sigh p/cage}                            ::  marked http response
              {$thou p/httr}                            ::  raw http response
              {$thus p/@ud q/(unit hiss)}               ::  http request+cancel
              {$veer p/@ta q/path r/@t}                 ::  drop-through
              {$vega p/path}                            ::  drop-through
          ==                                            ::
::
::::    %ford
  ::
++  hood                                                ::  assembly plan
  $:  zus/@ud                                           ::  zuse kelvin
      sur/(list hoof)                                   ::  structures
      lib/(list hoof)                                   ::  libraries
      fan/(list horn)                                   ::  resources
      src/(list hoop)                                   ::  program
  ==                                                    ::
++  hoof  (pair term (unit (pair case ship)))           ::  resource reference
++  hoop                                                ::  source in hood
  $%  {$& p/twig}                                       ::  direct twig
      {$| p/beam}                                       ::  resource location   
  ==                                                    ::
++  horn                                                ::  resource tree
  $%  {$ape p/twig}                                     ::  /~  twig by hand
      {$arg p/twig}                                     ::  /$  argument
      {$day p/horn}                                     ::  /|  list by @dr
      {$dub p/term q/horn}                              ::  /+  apply face
      {$fan p/(list horn)}                              ::  /.  list
      {$for p/path q/horn}                              ::  /,  descend
      {$hel p/@ud q/horn}                               ::  /%  propagate heel
      {$hub p/horn}                                     ::  /@  list by @ud
      {$man p/(map knot horn)}                          ::  /*  hetero map
      {$nap p/horn}                                     ::  /_  homo map
      {$now p/horn}                                     ::  /&  list by @da
      {$saw p/twig q/horn}                              ::  /;  operate on
      {$see p/beam q/horn}                              ::  /:  relative to
      {$sic p/twig q/horn}                              ::  /^  cast
      {$toy p/mark}                                     ::  /mark+  static
  ==                                                    ::
++  milk  (trel ship desk silk)                         ::  sourced silk
++  silk                                                ::  construction layer
  $^  {p/silk q/silk}                                   ::  cons
  $%  {$$ p/cage}                                       ::  literal
      {$bake p/mark q/beam r/path}                      ::  local synthesis
      {$boil p/mark q/beam r/path}                      ::  general synthesis
      {$bunt p/mark}                                    ::  example of mark
      {$call p/silk q/silk}                             ::  slam
      {$cast p/mark q/silk}                             ::  translate
      {$core p/beam}                                    ::  build program
      {$diff p/silk q/silk}                             ::  diff
      {$dude p/(trap tank) q/silk}                      ::  error wrap
      {$file p/beam}                                    ::  from clay
      {$flag p/(set $@(@uvH beam)) q/silk}              ::  add dependencies
      {$join p/mark q/silk r/silk}                      ::  merge
      {$mash p/mark q/milk r/milk}                      ::  annotate
      {$mute p/silk q/(list (pair wing silk))}          ::  mutant
      {$pact p/silk q/silk}                             ::  patch
      {$plan p/beam q/spur r/hood}                      ::  structured assembly
      {$reef $~}                                        ::  kernel reef
      {$ride p/twig q/silk}                             ::  silk thru twig
      {$tabl p/(list (pair silk silk))}                 ::  list
      {$vale p/mark q/*}                                ::  validate
      {$volt p/(cask *)}                                ::  unsafe add type
  ==                                                    ::
::::
++  bilk  (pair beak silk)                              ::  sourced request
++  gage                                                ::  recursive cage+tang
  $%  {$& p/cage}                                       ::  success
      {$| p/tang}                                       ::  error
      {$tabl p/(list (pair gage gage))}                 ::  table of results
  ==
++  gift-ford                                           ::  out result <-$
          $%  {$made p/@uvH q/gage}                     ::  computed result
              {$mass p/mass}                            ::  memory usage
              {$news p/@uvH}                            ::  fresh depends
          ==                                            ::
++  kiss-ford                                           ::  in request ->$
          $%  {$exec p/@p q/(unit bilk)}                ::  make / kill
              {$wasp p/@p q/{@uvH ?}}                   ::  depends ask / kill
              {$wegh $~}                                ::  report memory
          ==                                            ::
::
::::    %gall
  ::
++  club                                                ::  agent action
  $%  {$peel p/mark q/path}                             ::  translated peer
      {$peer p/path}                                    ::  subscribe
      {$poke p/cage}                                    ::  apply
      {$puff p/mark q/noun}                             ::  unchecked poke
      {$pull $~}                                        ::  unsubscribe
      {$punk p/mark q/cage}                             ::  translated poke
      {$pump $~}                                        ::  pump yes+no
  ==                                                    ::
++  cuft                                                ::  internal gift
  $%  {$coup p/(unit tang)}                             ::  poke result
      {$diff p/cage}                                    ::  subscription output
      {$doff p/mark q/noun}                             ::  untyped diff
      {$quit $~}                                        ::  close subscription
      {$reap p/(unit tang)}                             ::  peer result
  ==                                                    ::
++  culm                                                ::  config action
  $%  {$load p/scup}                                    ::  load+reload
  ::  {$kick $~}                                        ::  restart everything
  ::  {$stop $~}                                        ::  toggle suspend
  ::  {$wipe $~}                                        ::  destroy all state
  ==                                                    ::
++  cush  (pair term club)                              ::  internal kiss
++  dude  term                                          ::  server identity
++  scup  (pair ship desk)                              ::  autoupdate
++  well  (pair desk term)                              ::
++  suss  (trel dude @tas @da)                          ::  config report
::::                                                    ::
++  kiss-gall                                           ::  incoming request
  $%  {$conf p/dock q/culm}                             ::  configure app
      {$init p/ship}                                    ::  set owner
      {$deal p/sock q/cush}                             ::  full transmission
      {$west p/sack q/path r/*}                         ::  network request
      {$wegh $~}                                        ::  report memory
  ==                                                    ::
++  gift-gall                                           ::  outgoing result
  $%  {$mass p/mass}                                    ::  memory usage
      {$onto p/(each suss tang)}                        ::  about agent
      {$unto p/cuft}                                    ::  within agent
      {$mack p/(unit tang)}                             ::  message ack
  ==                                                    ::
::
::::    %arvo
  ::
++  gift-arvo                                           ::  out result <-$
  $?  gift-ames
      gift-clay
      gift-dill
      gift-eyre
      gift-ford
      gift-gall
      gift-behn
  ==
++  kiss-arvo                                           ::  in request ->$
  $?  kiss-ames
      kiss-clay
      kiss-dill
      kiss-eyre
      kiss-ford
      kiss-gall
      kiss-behn
  ==
++  note-arvo                                           ::  out request $->
  $?  {@tas $meta vase}
  $%  {$a kiss-ames}
      {$b kiss-behn}
      {$c kiss-clay}
      {$d kiss-dill}
      {$e kiss-eyre}
      {$f kiss-ford}
      {$g kiss-gall}
  ==  ==
++  sign-arvo                                           ::  in result $<-
  $%  {$a gift-ames}
      {$b gift-behn}
      {$c gift-clay}
      {$d gift-dill}
      {$e gift-eyre}
      {$f gift-ford}
      {$g gift-gall}
  ==
--
