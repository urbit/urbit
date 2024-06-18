/+  dprint
/+  easy-print=language-server-easy-print
::  $neo: New Shrub
::
::    Urbit is a namespace, from a path -> data
::    /~hastuc-dibtux/chats/unit-731 :: this a chat 
::    /~hastuc-dibtux/chats/unit-731/msg/~2024.1.27..10.30 :: this is a
::    message inside the chat
::
::    neo is a recipe for defining the kinds of data that live at these
::    paths. For instance, you would maybe like to define a chat
::    datatype, that could be bound into your namespace, so that your
::    friends could send you memes.
::
::
::
::
::
=>  
|%
::  
::  $care: Perspective on a path
::    
+$  care
  $~  %x
  $?  %x  :: single node
      %y  :: single node and immediate children
      %z  :: single node and all descendants
      :: without pails or vials
      %a  :: single node (%x)
      %b  :: node and immediate children (%y)
      %c  :: node and all descendants
  ==
+$  hunt   [=care =pith] ::
::  $over: range of time for case bookkeeping
::
::
+$  over
  $:  why=(pair @ud @ud)
      why-mut=(pair @ud @ud)
      zed=(pair @ud @ud)
      zed-mut=(pair @ud @ud)
      =rift
  ==
::
::  $ever: Total shrub version
::
::    .exe is incremented only when the shrub itself changes i.e. it
::    versions the %x care
::    .why is incremented when the shrub or any of its children changes
::    i.e. it versions the %y care
::    .zed is incremened when the shrub of any of its descendants change
::
::  $lock: Data and shape numbers
::
::     .p is the data version
::     .q is the shape version
+$  lock  (pair @ud @ud)
::
+$  ever  
  $:  exe=lock
      why=lock
      zed=lock
    ::
      shrub-life=@ud :: 3
      ship-life=@ud :: 6
      ship-rift=@ud :: 9
    ::
      =time
  ==
::  $once: Partial version
::
::    Identify shrub by either %x, %y or %z, as per $ever
::
+$  once
  [?(%x %y %z) p=case]

+$  pulp  ?(%noun %json)
::  $hash: Hash
+$  hash   @uvH
::  $seal: Signature
::
+$  seal   @uvH
::  $myth: Binding, possibly tombstoned
::
+$  myth
  $:  pail=(unit pail)
      =aeon
  ==
::
+$  vial  (pair stud *)
+$  book  (pair tale pail)
+$  poem  (pair tale (unit pail))
::
+$  aeon  (pair ever oath)
+$  tale  (pair case oath)
+$  oath  (pair hash seal)
+$  yuga  (axal aeon)
:: 
+$  saga  (pair aeon pail)
+$  pail  (pair stud vase)
::
+$  epic  $+(epic (axal saga))
+$  feat  (pair aeon vial)
+$  gest  $+(gest (axal feat))
++  road   (pole iota)
::  $stud: name for build system outputs
::  
::    Build system outputs cannot live at arbitrary points in the
::    namespace, to allow for the build system to bootstrap itself.
::    
+$  stud
  $@  @tas                                 ::  auth=urbit
  $:  mark=@tas                            :: 
      [=ship =desk]
  ==
++  stud-to-pith
  |=  s=stud
  ^-  pith
  ?@  s  ~[s]
  #/[p/ship.s]/[desk.s]/[mark.s]
::
++  pith-to-stud
  |=  p=road
  ^-  stud
  ?:  ?=([s=@ ~] p)
    s.p
  ?>  ?=([[%p ship=@] desk=@ mark=@ ~] p)
  [mark ship desk]:p
++  axal
  |$  [item]  
  [fil=(unit item) kid=(map iota $)]
++  axil
  |$  [item]
  [fil=(unit item) kid=(map pith item)]
++  of
  =|  fat=(axal)
  |@ 
  ++  view
    =|  res=(map pith _?>(?=(^ fil.fat) u.fil.fat))
    |=  [=care pax=pith]
    =.  fat  (dip pax)
    =?  res  ?=(^ fil.fat)
     (~(put by res) ~ u.fil.fat)
    ?+  care  !!
      %x  res
      %y  =.(fat snip (~(uni by res) tar))
      %z  (~(uni by res) tar)
    ==
  ::
  ++  anc-jab
    |*  [pax=pith fun=$-(* *)]
    ^+  fat
    ?~  pax
      fat
    =?  fil.fat  ?=(^ fil.fat)
      `(fun u.fil.fat)
    fat(kid (~(put by kid.fat) i.pax $(fat (~(got by kid.fat) i.pax), pax t.pax)))
    
  ::
  ++  anc
    =|  res=(list pith)
    =|  cur=pith
    |=  pax=pith
    ^-  (set pith)
    ?~  pax
      (~(gas in *(set pith)) res)
    =?  res  ?=(^ fil.fat)
      [cur res]
    $(fat (~(got by kid.fat) i.pax), pax t.pax, cur (snoc cur i.pax))
  ++  parent
    =|  res=(unit pith)
    =|  cur=pith
    |=  pax=pith
    |-  ^+  res
    ?~  pax
      res
    =?  res  ?=(^ fil.fat)
      `cur
    =/  nex  (~(get by kid.fat) i.pax)
    ?~  nex
      res
    $(fat u.nex, pax t.pax, cur (snoc cur i.pax))
  ++  snip
    |-  ^+  fat
    =*  loop  $
    %_    fat
        kid
      %-  ~(run by kid.fat)
      |=  f=_fat
      ?^  fil.f
        [`u.fil.f ~]
      loop(fat f)
    ==
  ::
  ++  kid
    |=  pax=pith
    ^-  (map pith _?>(?=(^ fil.fat) u.fil.fat))
    =.  fat  (dip pax)
    =.  fat  snip
    =.  fil.fat  ~
    tar
  ::
  ++  kids
    |=  pax=pith
    ^-  (axil _?>(?=(^ fil.fat) u.fil.fat))
    :-  (get pax)
    (kid pax)
  ::
  ++  del
    |=  pax=pith
    ^+  fat
    ?~  pax  [~ kid.fat]
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  fat
    fat(kid (~(put by kid.fat) i.pax $(fat u.kid, pax t.pax)))
  ::
  ::  Descend to the axal at this path
  ::
  ++  dip
    |=  pax=pith
    ^+  fat
    ?~  pax  fat
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  [~ ~]
    $(fat u.kid, pax t.pax)
  ::
  ++  gas
    |*  lit=(list (pair pith _?>(?=(^ fil.fat) u.fil.fat)))
    ^+  fat
    ?~  lit  fat
    $(fat (put p.i.lit q.i.lit), lit t.lit)
  ++  got
    |=  pax=pith
    ~|  missing-room/pax
    (need (get pax))
  ++  gut
    |*  [pax=pith dat=*]
    =>  .(dat `_?>(?=(^ fil.fat) u.fil.fat)`dat, pax `pith`pax)
    ^+  dat
    (fall (get pax) dat)
  ::
  ++  get
    |=  pax=pith
    fil:(dip pax)
  ::  Fetch file at longest existing prefix of the path
  ::
  ++  fit
    |=  pax=pith
    ^+  [pax fil.fat]
    ?~  pax  [~ fil.fat]
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  [pax fil.fat]
    =/  low  $(fat u.kid, pax t.pax)
    ?~  +.low
      [pax fil.fat]
    low
  ::
  ++  has
    |=  pax=pith
    !=(~ (get pax))
  ::  Delete subtree
  ::
  ++  lop
    |=  pax=pith
    ^+  fat
    ?~  pax  fat
    |-
    ?~  t.pax  fat(kid (~(del by kid.fat) i.pax))
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  fat
    fat(kid (~(put by kid.fat) i.pax $(fat u.kid, pax t.pax)))
  ::
  ++  put
    |*  [pax=pith dat=*]
    =>  .(dat `_?>(?=(^ fil.fat) u.fil.fat)`dat, pax `pith`pax)
    |-  ^+  fat
    ?~  pax  fat(fil `dat)
    =/  kid  (~(gut by kid.fat) i.pax ^+(fat [~ ~]))
    fat(kid (~(put by kid.fat) i.pax $(fat kid, pax t.pax)))
  ::
  ++  tap
    =|  pax=pith
    =|  out=(list (pair pith _?>(?=(^ fil.fat) u.fil.fat)))
    |-  ^+   out
    =?  out  ?=(^ fil.fat)  :_(out [pax u.fil.fat])
    =/  kid  ~(tap by kid.fat)
    |-  ^+   out
    ?~  kid  out
    %=  $
      kid  t.kid
      out  ^$(pax (weld pax /[p.i.kid]), fat q.i.kid)
    ==
  ::  Serialize to map
  ::
  ++  tar
    (~(gas by *(map pith _?>(?=(^ fil.fat) u.fil.fat))) tap)
  --
::  $span: 
+$  span  (pair @ud seal)
::  $case: Canonical (%x) version
+$  case  @ud
::  $soil: aeons, indexed by x
++  soil
  =<  a
  ^~
  |%
  ++  on    ((^on case poem) lte)
  +$  a  ((mop case poem) lte)
  --
+$  mode  ?(%add %dif %del)
+$  loot  [case=@ud =mode]
+$  dust  [=pith loot]
+$  grit  (list dust)
::  $dirt: Layer 1 of the namespace
++  dirt
  |%
  +$  card  (pair pith note)
  +$  note
    $%  [%grow =pail case=(unit case) =oath]
        [%cull ~]
    ==
  +$  loam  (axal soil)
  +$  gift   dust
  --
--
|%
++  land
  =<  land
  |%
  +$  land  ((mop case over) lte)
  ++  on    ((^on case over) lte)
  --
+$  turf  [=land =plot]
++  plan
  =<  plan
  |%
  +$  plan  ((mop @ud ,(set pith)) lte)
  ++  on    ((^on @ud ,(set pith)) lte)
  --
+$  plot
  $:  by-kids-mut=plan
      by-desc-mut=plan
      by-kids=tend
      by-desc=tend
  ==
++  tend
  =<  tend
  |%
  +$  tend  ((mop @ud ,(map pith =case)) lte)
  ++  on    ((^on @ud ,(map pith =case)) lte)
  --
++  farm  
  =<  farm
  |%
  +$  farm  (axal turf)
  +$  card  (pair pith note)
  +$  note
    $%  [%grow p=saga]
    ==
  --
--
|%
+$  sync  (trel pith hunt ?(%start %stop))
::  $curb: Constraint
+$  curb
  $~  [%pro %$]
  $%  [%or p=(list curb)] :: product type
      [%only p=stud] :: exclusivel
      [%rol p=stud q=curb]
      [%not p=curb q=curb] :: q not p
      [%pro p=stud] :: base case
      [%any ~]
  ==

++  compile-curb
  =|  fac=_|  :: did we just apply a face
  |=  [cur=curb get=$-(stud type)]
  =*  loop  $
  ^-  type
  ?+    -.cur  !!
      %or   
    :-  %fork
    %-  ~(gas in *(set type))
    %+  turn  p.cur
    |=  c=curb
    ^-  type
    !!
    :: :+  %cell
::    [%atom %tas `tag]
::  loop(cur c, fac |)
  ::
      %rol
    [%face (get-stud-name p.cur) $(cur q.cur, fac &)]
  ::
      %pro
    =-  (get p.cur)
    ?:  fac  -
    [%face (get-stud-name p.cur)]
  ==
::  +sell: pretty-print a vase to a tank using +deal.
::
++  a-sell
  ~/  %sell
  |=  vax=vase
  ^-  tank
  ~|  %sell
  (~(deal us p.vax) q.vax)
::

++  us                                                  ::  prettyprinter
  =>  |%
      +$  cape  [p=(map @ud wine) q=wine]               ::
      +$  wine                                          ::
                $@  $?  %noun                           ::
                        %path                           ::
                        %type                           ::
                        %void                           ::
                        %wall                           ::
                        %wool                           ::
                        %yarn                           ::
                    ==                                  ::
                $%  [%mato p=term]                      ::
                    [%core p=(list @ta) q=wine]         ::
                    [%face p=term q=wine]               ::
                    [%list p=term q=wine]               ::
                    [%pear p=term q=@]                  ::
                    [%bcwt p=(list wine)]               ::
                    [%plot p=(list wine)]               ::
                    [%stop p=@ud]                       ::
                    [%tree p=term q=wine]               ::
                    [%unit p=term q=wine]               ::
                    [%name p=^stud q=wine]               ::
                ==                                      ::
      --
  |_  sut=type
  ++  dash
    |=  [mil=tape lim=char lam=tape]
    ^-  tape
    =/  esc  (~(gas in *(set @tD)) lam)
    :-  lim
    |-  ^-  tape
    ?~  mil  [lim ~]
    ?:  ?|  =(lim i.mil)
            =('\\' i.mil)
            (~(has in esc) i.mil)
        ==
      ['\\' i.mil $(mil t.mil)]
    ?:  (lte ' ' i.mil)
      [i.mil $(mil t.mil)]
    ['\\' ~(x ne (rsh 2 i.mil)) ~(x ne (end 2 i.mil)) $(mil t.mil)]
  ::
  ++  deal  |=(lum=* (dish dole lum))
  ++  dial
    |=  ham=cape
    =+  gid=*(set @ud)
    =<  `tank`-:$
    |%
    ++  many
      |=  haz=(list wine)
      ^-  [(list tank) (set @ud)]
      ?~  haz  [~ gid]
      =^  mor  gid  $(haz t.haz)
      =^  dis  gid  ^$(q.ham i.haz)
      [[dis mor] gid]
    ::
    ++  $
      ^-  [tank (set @ud)]
      ?-    q.ham
          %noun      :_(gid [%leaf '*' ~])
          %path      :_(gid [%leaf '/' ~])
          %type      :_(gid [%leaf '#' 't' ~])
          %void      :_(gid [%leaf '#' '!' ~])
          %wool      :_(gid [%leaf '*' '"' '"' ~])
          %wall      :_(gid [%leaf '*' '\'' '\'' ~])
          %yarn      :_(gid [%leaf '"' '"' ~])
          [%mato *]  :_(gid [%leaf '@' (trip p.q.ham)])
          [%core *]
        =^  cox  gid  $(q.ham q.q.ham)
        :_  gid
        :+  %rose
          [[' ' ~] ['<' ~] ['>' ~]]
        |-  ^-  (list tank)
        ?~  p.q.ham  [cox ~]
        [[%leaf (rip 3 i.p.q.ham)] $(p.q.ham t.p.q.ham)]
      ::
          [%face *]
        =^  cox  gid  $(q.ham q.q.ham)
        :_(gid [%palm [['=' ~] ~ ~ ~] [%leaf (trip p.q.ham)] cox ~])
      ::
          [%list *]
        =^  cox  gid  $(q.ham q.q.ham)
        :_(gid [%rose [" " (weld (trip p.q.ham) "(") ")"] cox ~])
      ::
          [%bcwt *]
        =^  coz  gid  (many p.q.ham)
        :_(gid [%rose [[' ' ~] ['?' '(' ~] [')' ~]] coz])
      ::
          [%plot *]
        =^  coz  gid  (many p.q.ham)
        :_(gid [%rose [[' ' ~] ['[' ~] [']' ~]] coz])
      ::
          [%pear *]
        :_(gid [%leaf '%' ~(rend co [%$ p.q.ham q.q.ham])])
      ::
          [%stop *]
        =+  num=~(rend co [%$ %ud p.q.ham])
        ?:  (~(has in gid) p.q.ham)
          :_(gid [%leaf '#' num])
        =^  cox  gid
            %=  $
              gid    (~(put in gid) p.q.ham)
              q.ham  (~(got by p.ham) p.q.ham)
            ==
        :_(gid [%palm [['.' ~] ~ ~ ~] [%leaf ['^' '#' num]] cox ~])
      ::
          [%tree *]
        =^  cox  gid  $(q.ham q.q.ham)
        :_(gid [%rose [" " (weld (trip p.q.ham) "(") ")"] cox ~])
      ::
          [%unit *]
        =^  cox  gid  $(q.ham q.q.ham)
        :_(gid [%rose [" " (weld (trip p.q.ham) "(") ")"] cox ~])
      ::
          [%name *]
        :_  gid
        ?@  p.q.ham  (cat 3 '#' mark.p.q.ham)
        (rap 3 '#' auth.p.q.ham '+' (spat type.p.q.ham) ~)
      ==
    --
  ::
  ++  dish  !:
    |=  [ham=cape lum=*]  ^-  tank
    ~|  [%dish-h ?@(q.ham q.ham -.q.ham)]
    ~|  [%lump lum]
    ~|  [%ham ham]
    %-  need
    =|  gil=(set [@ud *])
    |-  ^-  (unit tank)
    ?-    q.ham
        %noun
      %=    $
          q.ham
        ?:  ?=(@ lum)
          [%mato %$]
        :-  %plot
        |-  ^-  (list wine)
        [%noun ?:(?=(@ +.lum) [[%mato %$] ~] $(lum +.lum))]
      ==
    ::
        %path
      :-  ~
      :+  %rose
        [['/' ~] ['/' ~] ~]
      |-  ^-  (list tank)
      ?~  lum  ~
      ?@  lum  !!
      ?>  ?=(@ -.lum)
      [[%leaf (rip 3 -.lum)] $(lum +.lum)]
    ::
        %type
      =+  tyr=|.((dial dole))
      =+  vol=tyr(sut lum)
      =+  cis=;;(tank .*(vol [%9 2 %0 1]))
      :^  ~   %palm
        [~ ~ ~ ~]
      [[%leaf '#' 't' '/' ~] cis ~]
    ::
        %wall
      :-  ~
      :+  %rose
        [[' ' ~] ['<' '|' ~] ['|' '>' ~]]
      |-  ^-  (list tank)
      ?~  lum  ~
      ?@  lum  !!
      [[%leaf (trip ;;(@ -.lum))] $(lum +.lum)]
    ::
        %wool
      :-  ~
      :+  %rose
        [[' ' ~] ['<' '<' ~] ['>' '>' ~]]
      |-  ^-  (list tank)
      ?~  lum  ~
      ?@  lum  !!
      [(need ^$(q.ham %yarn, lum -.lum)) $(lum +.lum)]
    ::
        %yarn
      [~ %leaf (dash (tape lum) '"' "\{")]
    ::
        %void
      ~
    ::
        [%mato *]
      ?.  ?=(@ lum)
        ~
      :+  ~
        %leaf
      ?+    (rash p.q.ham ;~(sfix (cook crip (star low)) (star hig)))
          ~(rend co [%$ p.q.ham lum])
        %$    ~(rend co [%$ %ud lum])
        %t    (dash (rip 3 lum) '\'' ~)
        %tas  ['%' ?.(=(0 lum) (rip 3 lum) ['$' ~])]
      ==
    ::
        [%core *]
      ::  XX  needs rethinking for core metal
      ::  ?.  ?=(^ lum)  ~
      ::  =>  .(lum `*`lum)
      ::  =-  ?~(tok ~ [~ %rose [[' ' ~] ['<' ~] ['>' ~]] u.tok])
      ::  ^=  tok
      ::  |-  ^-  (unit (list tank))
      ::  ?~  p.q.ham
      ::    =+  den=^$(q.ham q.q.ham)
      ::    ?~(den ~ [~ u.den ~])
      ::  =+  mur=$(p.q.ham t.p.q.ham, lum +.lum)
      ::  ?~(mur ~ [~ [[%leaf (rip 3 i.p.q.ham)] u.mur]])
      [~ (dial ham)]
    ::
        [%face *]
      =+  wal=$(q.ham q.q.ham)
      ?~  wal
        ~
      [~ %palm [['=' ~] ~ ~ ~] [%leaf (trip p.q.ham)] u.wal ~]
    ::
        [%list *]
      ?:  =(~ lum)
        [~ %leaf '~' ~]
      =-  ?~  tok
            ~
          [~ %rose [[' ' ~] ['~' '[' ~] [']' ~]] u.tok]
      ^=  tok
      |-  ^-  (unit (list tank))
      ?:  ?=(@ lum)
        ?.(=(~ lum) ~ [~ ~])
      =+  [for=^$(q.ham q.q.ham, lum -.lum) aft=$(lum +.lum)]
      ?.  &(?=(^ for) ?=(^ aft))
        ~
      [~ u.for u.aft]
    ::
        [%bcwt *]
      |-  ^-  (unit tank)
      ?~  p.q.ham
        ~
      =+  wal=^$(q.ham i.p.q.ham)
      ?~  wal
        $(p.q.ham t.p.q.ham)
      wal
    ::
        [%plot *]
      =-  ?~  tok
            ~
          [~ %rose [[' ' ~] ['[' ~] [']' ~]] u.tok]
      ^=  tok
      |-  ^-  (unit (list tank))
      ?~  p.q.ham
        ~
      ?:  ?=([* ~] p.q.ham)
        =+  wal=^$(q.ham i.p.q.ham)
        ?~(wal ~ [~ [u.wal ~]])
      ?@  lum
        ~
      =+  gim=^$(q.ham i.p.q.ham, lum -.lum)
      ?~  gim
        ~
      =+  myd=$(p.q.ham t.p.q.ham, lum +.lum)
      ?~  myd
        ~
      [~ u.gim u.myd]
    ::
        [%pear *]
      ?.  =(lum q.q.ham)
        ~
      =.  p.q.ham
        (rash p.q.ham ;~(sfix (cook crip (star low)) (star hig)))
      =+  fox=$(q.ham [%mato p.q.ham])
      ?>  ?=([~ %leaf ^] fox)
      ?:  ?=(?(%n %tas) p.q.ham)
        fox
      [~ %leaf '%' p.u.fox]
    ::
        [%stop *]
      ?:  (~(has in gil) [p.q.ham lum])  ~
      =+  kep=(~(get by p.ham) p.q.ham)
      ?~  kep
        ~|([%stop-loss p.q.ham] !!)
      $(gil (~(put in gil) [p.q.ham lum]), q.ham u.kep)
    ::
        [%tree *]
      =-  ?~  tok
            ~
          [~ %rose [[' ' ~] ['{' ~] ['}' ~]] u.tok]
      ^=  tok
      =+  tuk=*(list tank)
      |-  ^-  (unit (list tank))
      ?:  =(~ lum)
        [~ tuk]
      ?.  ?=([n=* l=* r=*] lum)
        ~
      =+  rol=$(lum r.lum)
      ?~  rol
        ~
      =+  tim=^$(q.ham q.q.ham, lum n.lum)
      ?~  tim
        ~
      $(lum l.lum, tuk [u.tim u.rol])
    ::
        [%unit *]
      ?@  lum
        ?.(=(~ lum) ~ [~ %leaf '~' ~])
      ?.  =(~ -.lum)
        ~
      =+  wal=$(q.ham q.q.ham, lum +.lum)
      ?~  wal
        ~
      [~ %rose [[' ' ~] ['[' ~] [']' ~]] [%leaf '~' ~] u.wal ~]
    ::
        [%name *]
      $(q.ham q.q.ham)
    ==
  ::
  ++  doge
    |=  ham=cape
    =-  ?+  woz  woz
          [%list * [%mato %'ta']]  %path
          [%list * [%mato %'t']]   %wall
          [%list * [%mato %'tD']]  %yarn
          [%list * %yarn]          %wool
        ==
    ^=  woz
    ^-  wine
    ?.  ?=([%stop *] q.ham)
      ?:  ?&  ?=  [%bcwt [%pear %n %0] [%plot [%pear %n %0] [%face *] ~] ~]
                q.ham
              =(1 (met 3 p.i.t.p.i.t.p.q.ham))
          ==
        [%unit =<([p q] i.t.p.i.t.p.q.ham)]
      q.ham
    =+  may=(~(get by p.ham) p.q.ham)
    ?~  may
      q.ham
    =+  nul=[%pear %n 0]
    ?.  ?&  ?=([%bcwt *] u.may)
            ?=([* * ~] p.u.may)
            |(=(nul i.p.u.may) =(nul i.t.p.u.may))
        ==
      q.ham
    =+  din=?:(=(nul i.p.u.may) i.t.p.u.may i.p.u.may)
    ?:  ?&  ?=([%plot [%face *] [%face * %stop *] ~] din)
            =(p.q.ham p.q.i.t.p.din)
            =(1 (met 3 p.i.p.din))
            =(1 (met 3 p.i.t.p.din))
        ==
      :+  %list
        (cat 3 p.i.p.din p.i.t.p.din)
      q.i.p.din
    ?:  ?&  ?=  $:  %plot
                    [%face *]
                    [%face * %stop *]
                    [[%face * %stop *] ~]
                ==
                din
            =(p.q.ham p.q.i.t.p.din)
            =(p.q.ham p.q.i.t.t.p.din)
            =(1 (met 3 p.i.p.din))
            =(1 (met 3 p.i.t.p.din))
            =(1 (met 3 p.i.t.t.p.din))
        ==
      :+  %tree
        %^    cat
            3
          p.i.p.din
        (cat 3 p.i.t.p.din p.i.t.t.p.din)
      q.i.p.din
    q.ham
  ::
  ++  dole
    ^-  cape
    =+  gil=*(set type)
    =+  dex=[p=*(map type @) q=*(map @ wine)]
    =<  [q.p q]
    |-  ^-  [p=[p=(map type @) q=(map @ wine)] q=wine]
    =-  [p.tez (doge q.p.tez q.tez)]
    ^=  tez
    ^-  [p=[p=(map type @) q=(map @ wine)] q=wine]
    ?-    sut
        %noun      [dex sut]
        %void      [dex sut]
        [%atom *]  [dex ?~(q.sut [%mato p.sut] [%pear p.sut u.q.sut])]
        [%cell *]
      =+  hin=$(sut p.sut)
      =+  yon=$(dex p.hin, sut q.sut)
      :-  p.yon
      :-  %plot
      ?:(?=([%plot *] q.yon) [q.hin p.q.yon] [q.hin q.yon ~])
    ::
        [%core *]
      =+  yad=$(sut p.sut)
      :-  p.yad
      =+  ^=  doy  ^-  [p=(list @ta) q=wine]
          ?:  ?=([%core *] q.yad)
            [p.q.yad q.q.yad]
          [~ q.yad]
      :-  %core
      :_  q.doy
      :_  p.doy
      %^  cat  3
        %~  rent  co
        :+  %$  %ud
        %-  ~(rep by (~(run by q.r.q.sut) |=(tome ~(wyt by q.+<))))
        |=([[@ a=@u] b=@u] (add a b))
      %^  cat  3
        ?-(r.p.q.sut %gold '.', %iron '|', %lead '?', %zinc '&')
      =+  gum=(mug q.r.q.sut)
      %+  can  3
      :~  [1 (add 'a' (mod gum 26))]
          [1 (add 'a' (mod (div gum 26) 26))]
          [1 (add 'a' (mod (div gum 676) 26))]
      ==
    ::
        [%hint *]
      =+  yad=$(sut q.sut)
      ?.  ?=(%know -.q.p.sut)  yad
      [p.yad [%name p.q.p.sut q.yad]]
    ::
        [%face *]
      =+  yad=$(sut q.sut)
      ?^(p.sut yad [p.yad [%face p.sut q.yad]])
    ::
        [%fork *]
      =+  yed=(sort ~(tap in p.sut) aor)
      =-  [p [%bcwt q]]
      |-  ^-  [p=[p=(map type @) q=(map @ wine)] q=(list wine)]
      ?~  yed
        [dex ~]
      =+  mor=$(yed t.yed)
      =+  dis=^$(dex p.mor, sut i.yed)
      [p.dis q.dis q.mor]
    ::
        [%hold *]
      =+  hey=(~(get by p.dex) sut)
      ?^  hey
        [dex [%stop u.hey]]
      ?:  (~(has in gil) sut)
        =+  dyr=+(~(wyt by p.dex))
        [[(~(put by p.dex) sut dyr) q.dex] [%stop dyr]]
      =+  rom=$(gil (~(put in gil) sut), sut ~(repo ut sut))
      =+  rey=(~(get by p.p.rom) sut)
      ?~  rey
        rom
      [[p.p.rom (~(put by q.p.rom) u.rey q.rom)] [%stop u.rey]]
    ==
  ::
  ++  duck  (dial dole)
  --
::
+$  deed
  $@  ?
  $:  time=(unit @dr)
      req=?
  ==
::
::  $gift: notification that a children changed
::
+$  gift  (axal loot)
::
::  $hunt: perspective and shrub
::    
+$  tour   hunt
::  $block: call-stack state for blocking semantics
::
::    .get is the list of blocking requests
::    .err is the possible error state
::    If .err is not ~, then .get is functionally ~
::    
+$  block  [get=(set hunt) err=(unit quit)]
::
::  $halt: Currently blocked flows and indices
::
+$  halt
  $:  by-hunt=(map hunt flow) :: flow blocked by hunt
      by-flow=(jug flow hunt) :: hunts blocked by flow
      clog=(map flow (qeu move)) :: enqueued moves to send on unblock
  ==
+$  quit
  $%  [%gone =term] :: injected a bad dependency
      [%goof =tang] :: crash
  ==
++  print-quit
  |=  q=quit
  ^-  tang
  ?-  -.q
    %goof  tang.q
    %gone  ~[leaf/"No dependency at {<term>}"]
  ==
+$  ack  (pair flow (unit quit))
::
::  $flow: Call direction
::  
::    .p is the source
::    .q is the destination
::
+$  flow  (pair pith pith)
::
::  $disk: Reference to a suite of code
::
::    If sig case, then refers to the %std disk
+$  disk
  $@(~ [=ship =term])
::  $tack: Type of code being distributed
::
::    %con: CONverter of protocols
::    %imp: IMPlemenation of shrub
::    %pro: PROtocol (type)
::
+$  tack
  ?(%con %imp %pro %lib %rol %fil)
::  $post: Name of code being distributed
::
+$  post  (pair tack stud)
::  $prop: Code unit inside desk
+$  prop  (pair tack mark)
::
::  +get-stud-name: Get name for $stud
::
++  get-stud-name
  |=  =stud
  ?@  stud  stud
  mark.stud
::  +drive: Path multiplexer core
::
++  drive
  |%
  ::  +en:drive: Multiplex several paths into one
  ::    
  ::    See also: (+de:drive)
  ++  en
    =|  res=pith
    |=  ps=(list pith)
    ?~  ps
      res
    $(res (welp res [%ud (lent i.ps)] i.ps), ps t.ps)
  ::  +de:drive: Demultiplex one path into several
  ::    
  ::    See also: (+en:drive)

  ++  de
    =|  res=(list pith)
    |=  pax=(pole iota)
    ^+  res
    ?:  =(~ pax)
      (flop res)
    =^  nex=pith  pax
      ?>  ?=([[%ud len=@] rest=*] pax)
      `[pith (pole iota)]`[(scag [len rest]:pax) (slag [len rest]:pax)]
    $(res [nex res])
  --
++  curt
  =|  res=(set post)
  |=  =curb
  ^+  res
  =*  loop-curb  $
  ?-    -.curb
      ?(%pro %only)
    (~(put in res) pro/p.curb)
  ::
      %or
    =/  curs  p.curb
    |-
    ?~  curs
      res
    =/  new  loop-curb(curb i.curs)
    =.  res  (~(uni in res) new)
    $(curs t.curs)
  ::
      %rol
    =.  res  (~(put in res) rol/p.curb)
    $(curb q.curb)
  ::
      %not
    $(curb q.curb)
  ::
      %any  res

  ==

::  +ford: Container for build system bootstrapping
++  ford
  |%
  +$  kind  ?(%pre %src %out)
  ++  desk
    |%
    ++  kids  *^kids
    --
  ++  dock
    ^-  ^dock
    [pro/%vase ~ ~]
  ++  is-stud
    |=  s=stud
    ?^  s  |
    =(%ford (end [3 4] s))
  ++  out
    ^-  curb
    [%or pro/%vase pro/%tang ~]
  ::  +riff:ford: Constant build system node
  ::  
  ::    Required for bootstrapping. This is used to put the reef and
  ::    other ford combinators into the build system to bootstrap
  ::    everything else. To update a riff, simply %make over the top
  ::
  ++  riff
    ^-  kook
    |%
    ++  state  out:ford
    ++  poke   *(set stud)
    ++  kids  ~
    ++  deps  ~
    ++  form
      ^-  ^form
      |_  [=bowl =aeon =stud state-vase=vase]
      +*  sta  !<([cache=(unit vase) ~] state-vase)
      ++  poke
        |=  pal=pail
        ^-  (quip card pail)
        !!
      ::
      ++  init
        |=  pal=(unit pail)
        ^-  (quip card pail)
        =/  old  (need pal)
        ?>  ?=(%vase p.old)
        `old
      --
    --
  ::  +dep:ford: $fief for a ford dependency
  ::  
  ::    Handy shortcut to specifiy a dependency in the build system
  ++  dep  `fief`[& [pro/%vase (sy %ford-in ~)] ~]
  ::  +get-output: pull build resuit of dependency
  ::
  ++  get-output
    |=  [=bowl =term]
    ^-  (unit vase)
    =/  [=pith =lore]  (~(got by deps.bowl) term)
    ?~  fil.lore
      ~
    `q.pail.u.fil.lore
  ::
  ++  run
    |=  txt=@t
    (scan (trip txt) (rein *name))
  +$  loc
    [=disk =pith]
  ::  $lib:ford: Specification of library import
  ::
  +$  lib
    [face=term =stud]
  ::  $pro:ford: Specification of protocol import
  ::
  +$  pro
    [face=term =stud]
  +$  fil  
    [face=term =stud]
  +$  far  
    [face=term =stud]
  +$  fal
    [face=term =stud]
  +$  vale
    [face=term =stud]
  ::  $file:ford: Code with imports
  ::
  ::    XX: rename to pile
  ::
  +$  file
    $:  pro=(list pro)
        :: grab=(list 
        lib=(list lib)
        fil=(list fil)
        far=(list far)
        fal=(list fal)
        =hoon
    ==
  ::  +rein:ford: Parse code with imports
  ++  rein
    |=  =name
    =<  apex
    |%
    ++  dis  
      ;~  pose
        (cold ~ cab)
        ;~((glue bar) ;~(pfix sig fed:ag) sym)
      ==
    ::  +lib-loc: Parse library location
    ::
    ++  lib-loc
      ;~(plug dis stip)
    ::  +old-nam: Parse file path (deprecated: XX revisit)
    ::
    ::     Examples:
    ::     Absolute ~hastuc-dibtux/src/foo/bar/test
    ::     Relative %^/bar
    ++  old-nam
      :: ^-  $-(nail (like name))
      ;~  pose
        %+  sear 
          |=  [kets=(list) pit=pith]
          ^-  (unit ^name)
          %-  mole
          |.  ^-  ^name
          =.  pit  (scag (sub (lent pit) (lent kets)) pit)
          =-  ~&(parsed-name/- -)
          name(pith (welp pith.name pit))
        ;~(pfix cen ;~(plug (star ket) stip))                     :: relative
        ;~(plug ;~(pfix fas sig fed:ag) stip) :: absolute
      ==
    ::  +std:rein:ford: Parse import directive
    ::
    ::    Either  name:~ship/desk
    ::    or      name (from %std disk)
    ::
    ++  std
      ;~  pose
        ;~(plug sym ;~(pfix col sig fed:ag) ;~(pfix fas sym))
        sym
      ==
    ::  +pro:rein:ford: Parse protocol import directive
    ::
    ::    /@  foo=bar  :: imports %bar protocol from %std disk with name foo
    ::    /@  bar      :: imports %bar protocol from %std disk with name bar
    ::

    ++  pro
      :: ^-  $-(nail (like ^pro))
      %+  rune  pat
      ;~  pose
        ;~(plug sym ;~(pfix tis std))
        %+  cook
          |=  =stud
          ?@  stud  [stud stud]
          [mark.stud stud]
        std
      ==
    ++  lib
      :: ^-  $-(nail (like ^pro))
      %+  rune  hep
      ;~  pose
        ;~(plug sym ;~(pfix tis std))
        %+  cook
          |=  =stud
          ?@  stud  [stud stud]
          [mark.stud stud]
        std
      ==
    ::
    ++  fil
      :: ^-  $-(nail (like ^pro))
      %+  rune  tar
      ;~  pose
        ;~(plug sym ;~(pfix tis std))
        %+  cook
          |=  =stud
          ?@  stud  [stud stud]
          [mark.stud stud]
        std
      ==
    ::
    ++  far
      :: ^-  $-(nail (like ^pro))
      %+  rune  gar
      ;~  pose
        ;~(plug sym ;~(pfix tis std))
        %+  cook
          |=  =stud
          ?@  stud  [stud stud]
          [mark.stud stud]
        std
      ==
    ++  fal
      :: ^-  $-(nail (like ^pro))
      %+  rune  gal
      ;~  pose
        ;~(plug sym ;~(pfix tis std))
        %+  cook
          |=  =stud
          ?@  stud  [stud stud]
          [mark.stud stud]
        std
      ==


    ::  +old-lib: Parse arbitrary library import directive
    ::
    ::    Unused, todo revive with more recursive build system
    ::
    ::    /-  face=~hastuc-dibtux/foo/bar <- imports ~hastuc-dibtux
    ::    /-  %^/bar <- imports bar/hoon up one level with face bar
    ::
    ++  old-lib
      :: ^-  $-(nail (like ^lib))
      %+  rune  hep
      ;~  pose
        ;~(plug sym ;~(pfix tis old-nam))
        %+  cook
          |=  n=^name
          =/  last  (rear pith.n)
          :_  n
          ?@  last  last
          (scot last)
        old-nam
      ==
    ++  rune
      |*  [car=rule rul=rule]
      (ifix [;~(plug fas car gap) gay] rul)

    ++  libs
      :: ^-  $-(nail (like (list ^lib)))
      (star lib)
    ++  pros
      :: ^-  $-(nail (like (list ^pro)))
      (star pro)
    ++  fils  (star fil)
    ++  fals  (star fal)
    ++  fars  (star far)
    ++  hone
      :: ^-  $-(nail (like hoon))
      =+  vaz=(vang & (en-path:^name name))
      (ifix [gay gay] tall:vaz)
    ++  apex
      :: ^-  rule
      ;~  plug 
        pros
        libs
        fils
        fars
        fals
        hone
      ==
    --
  ::  +with-face:ford: Decorate vase with face
  ::
  ++  with-face
    |=  [fac=@tas =vase]
    vase(p [%face fac p.vase])
  ::  +with-faces:ford: Decorate vases with faces, slopped onto reef
  ::
  ++  with-faces
    |=  [reef=vase faces=(list (pair term vase))]
    ?~  faces
      reef
    $(reef (slop (with-face i.faces) reef), faces t.faces)
  --
++  gall
  |%
  +$  req  
    $%  [%peek p=@dr]
        [%keep ~]
        [%reap ~]
        [%peer ~]
    ==
  +$  peek  [src=(set pith) refresh=@dr]
  +$  sign  sign:agent:^gall
  +$  res   (pair pith sign)
  --
::  +behn: Timer vane
++  behn
  |%
  ::  $req: Timer request
  ::
  ::    %wait: Set timer
  ::    %rest: Cancel timer
  ::
  +$  req  $>(?(%rest %wait) task:^behn)
  ::  $behnres: Timer response
  ::
  ::    %wake: Timer went off
  ::
  +$  res  $>(%wake gift:^behn)
  --
++  eyre
  |%
  +$  req  [%connect =binding:^eyre =pith]
  +$  res  [%bound ~]
  +$  sign  (pair @ta gift)
  +$  gift
    $%  [%head =response-header:http]
        [%data dat=(unit octs)]
        [%done ~]
    ==
  +$  task
    (pair @ta inbound-request:^eyre)
  --
::  +clay: Filesystem overlay
::
++  clay
  |%
  ::  $peer:clay: Request for file(s) subscription
  ::
  +$  peer  [=desk =path as=(unit mark)]
  ::
  ::  $req:clay: Filesystem request
  ::   
  ::    %peer: Setup file subscription at .pith
  ::    %pull: Cancel file subscripiton at .pith
  ::
  +$  req
    $%  [%peer =pith =peer]
        [%pull =pith]
    ==
  ::  $res:clay: Filesystem response
  +$  res  [=pith case=@ud files=(axol cage)]
  --
::
++  iris
  |%
  +$  req  [hand=pith dat=request:http]
  +$  res  [hand=pith dat=client-response:^iris]
  --
:: * A `$bolt` is a `[=stud =once]`
::
::  $peer: Subscription

::  $tone: parent change tracking
::
+$  yell  (pair care howl)
+$  tone
  $%  [%sell ~]
      [%rely =term =pith]
      [%halt ~]
  ==
+$  dead  term
+$  howl  tone
::  $wail: change result
+$  wail  (trel hunt howl mode)
  
:: $song
::  $sound: internal change tracking listeners
::
+$  roar  (pair pith tone)
  
+$  meow
  (jug pith (pair care tone))
::
::  $noise: external change tracking listeners
+$  noise
  (jug hunt rely)
+$  riot  (axal rave)
+$  rave
  $:  exe=(set howl)
      why=(set howl)
      zed=(set howl)
  ==

::
::  $ring: node change tracking
::
+$  ring
  $%  [%dep p=term =care q=name]
      [%sync ~]
  ==
::
+$  out
  $%  [%sync p=hunt]
      [%stop p=hunt]
  ==
::  $town: foreign
+$  town  (axal mall)
+$  deli
  $:  last=ever
      desc=_|
      =yuga :: if nonempty, paths still loading
      =epic
  ==
+$  mall
  $:  mart=(set hunt)  :: subscribers
      del=(unit deli) :: if responsible, subscription info
      shop=(unit aeon) :: if behind, latest case
      ~
  ==
::  $city: synchronsation, indexed by publisher
+$  city  (axal ward)
+$  ward
  $:  exe=(set path)
      why=(set path)
      zed=(set path)
  ==
++  pave
  |=  p=path
  ^-  pith
  %+  turn  p
  |=  i=@ta
  ?^  res=(rush i spot:stip)
    u.res
  ?:  =('.y' i)
    f/&
  ?:  =('.n' i)
    f/|
  [%ta i]
::
++  stip                                                ::  typed path parser 
  =<  swot
  |%
  ++  swot  |=(n=nail `(like pith)`(;~(pfix fas (more fas spot)) n))
  ::
  ++  spot
    %+  sear
      |=  a=*
      ^-  (unit iota)
      ?+  a  ~
        @      ?:(((sane %tas) a) [~ `@tas`a] ~)
        [@ @]  ((soft iota) a)
      ==
    %-  stew
    ^.  stet  ^.  limo
    :~  :-  'a'^'z'  sym
        :-  '$'      (cold [%tas %$] buc)
        :-  '0'^'9'  bisk:so
        :-  '-'      tash:so
        :-  '.'      zust:so
        :-  '~'      ;~(pfix sig ;~(pose (stag %da (cook year when:so)) crub:so (easy [%n ~])))
        :-  '\''     (stag %t qut)
    ==
  --
::
::
::
++  pith
  |^  $+(pith ^pith)
  ++  en-tape
    |=  pit=$
    (spud (pout pit))
  ++  dif
    |=  [a=$ b=$]
    |-  ^+  a
    ?~  a  b
    ?~  b  a
    ?.  =(i.a i.b)
      a
    $(a t.a, b t.b)
  ++  sub
    |=  [from=$ del=$]
    ~|  pith-sub/[from del]
    !.
    |-  ^+  from
    ?~  del  from
    ?>  ?=(^ from)
    ?>  =(i.del i.from)
    $(del t.del, from t.from)
  ::
  ++  en-cord
    |=  pit=$
    (spat (pout pit))
  ::
  ++  prefix
    =|  res=$
    |=  [long=$ curt=$]
    ^-  (unit _res)
    ?~  curt  `(flop res)
    ?~  long  ~
    ?.  =(i.long i.curt)
      ~
    $(long t.long, curt t.curt, res [i.long res])
  ::
  ++  suffix
    |=  [long=$ curt=$]
    ^-  _curt
    ?~  curt
      long
    ?~  long
      ~
    $(curt t.curt, long t.long)
  ++  sort
    |=  [a=$ b=$]
    (lte (lent a) (lent b))
  --
++  name
  =<  name
  |%
  +$  name  [=ship =pith]  
  ++  rule
    :: ^-  _|~(nail *(like name))
    ;~(plug ;~(pfix fas sig fed:ag) stip)
  ++  en-spar
    |=  nam=name
    ^-  spar:ames
    [ship.nam (pout pith.nam)]
  ::
  ++  en-pith
    |=  nam=name
    ^-  pith
    [p/ship.nam pith.nam]
  ++  en-tape
    |=  nam=name
    (spud (pout (en-pith nam)))
  ++  en-path
    |=  nam=name
    (pout (en-pith nam)) 
  ++  de-pith  |=(pith ~|(de-pith/+< (need (de-pith-soft +<))))
  ++  de-pith-soft
    |=  =pith
    ^-  (unit name)
    ?.  ?=([[%p @] *] pith)
      ~
    `[+.i.pith t.pith]
  --
++  axol  axal:..zuse
++  axol-of  of:..zuse
++  axal
  |$  [item]  
  [fil=(unit item) kid=(map iota $)]
++  axil
  |$  [item]
  [fil=(unit item) kid=(map pith item)]
++  of
  =|  fat=(axal)
  |@ 
  ++  rot
    (need fil.fat)

  ++  view
    =|  res=(map pith _?>(?=(^ fil.fat) u.fil.fat))
    |=  [=care pax=pith]
    =.  fat  (dip pax)
    =?  res  ?=(^ fil.fat)
     (~(put by res) ~ u.fil.fat)
    ?-  care
      %x  res
      %y  =.(fat snip (~(uni by res) tar))
      %z  (~(uni by res) tar)
    ==
  ::
  ++  anc-jab
    |*  [pax=pith fun=$-(* *)]
    ^+  fat
    ?~  pax
      fat
    =?  fil.fat  ?=(^ fil.fat)
      `(fun u.fil.fat)
    fat(kid (~(put by kid.fat) i.pax $(fat (~(got by kid.fat) i.pax), pax t.pax)))
    
  ::
  ++  anc
    =|  res=(list pith)
    =|  cur=pith
    |=  pax=pith
    ^-  (set pith)
    ?~  pax
      (~(gas in *(set pith)) res)
    =?  res  ?=(^ fil.fat)
      [cur res]
    $(fat (~(got by kid.fat) i.pax), pax t.pax, cur (snoc cur i.pax))
  ++  parent
    =|  res=(unit pith)
    =|  cur=pith
    |=  pax=pith
    |-  ^+  res
    ?~  pax
      res
    =?  res  ?=(^ fil.fat)
      `cur
    =/  nex  (~(get by kid.fat) i.pax)
    ?~  nex
      res
    $(fat u.nex, pax t.pax, cur (snoc cur i.pax))
  ++  snip
    |-  ^+  fat
    =*  loop  $
    %_    fat
        kid
      %-  ~(run by kid.fat)
      |=  f=_fat
      ?^  fil.f
        [`u.fil.f ~]
      loop(fat f)
    ==
  ::
  ++  kid
    |=  pax=pith
    ^-  (map pith _?>(?=(^ fil.fat) u.fil.fat))
    =.  fat  (dip pax)
    =.  fat  snip
    =.  fil.fat  ~
    tar
  ::
  ++  kids
    |=  pax=pith
    ^-  (axil _?>(?=(^ fil.fat) u.fil.fat))
    :-  (get pax)
    (kid pax)
  ::
  ++  del
    |=  pax=pith
    ^+  fat
    ?~  pax  [~ kid.fat]
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  fat
    fat(kid (~(put by kid.fat) i.pax $(fat u.kid, pax t.pax)))
  ::
  ::  Descend to the axal at this path
  ::
  ++  dip
    |=  pax=pith
    ^+  fat
    ?~  pax  fat
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  [~ ~]
    $(fat u.kid, pax t.pax)
  ::
  ++  gas
    |*  lit=(list (pair pith _?>(?=(^ fil.fat) u.fil.fat)))
    ^+  fat
    ?~  lit  fat
    $(fat (put p.i.lit q.i.lit), lit t.lit)
  ++  got
    |=  pax=pith
    ~|  missing-room/pax
    (need (get pax))
  ++  gut
    |*  [pax=pith dat=*]
    =>  .(dat `_?>(?=(^ fil.fat) u.fil.fat)`dat, pax `pith`pax)
    ^+  dat
    (fall (get pax) dat)
  ::
  ++  get
    |=  pax=pith
    fil:(dip pax)
  ::  Fetch file at longest existing prefix of the path
  ::
  ++  fit
    |=  pax=pith
    ^+  [pax fil.fat]
    ?~  pax  [~ fil.fat]
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  [pax fil.fat]
    =/  low  $(fat u.kid, pax t.pax)
    ?~  +.low
      [pax fil.fat]
    low
  ::
  ++  has
    |=  pax=pith
    !=(~ (get pax))
  ::  Delete subtree
  ::
  ++  lop
    |=  pax=pith
    ^+  fat
    ?~  pax  fat
    |-
    ?~  t.pax  fat(kid (~(del by kid.fat) i.pax))
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  fat
    fat(kid (~(put by kid.fat) i.pax $(fat u.kid, pax t.pax)))
  ++  rep
    |*  [pax=pith fit=_fat]
    |-  ^+  fat
    ?~  pax  fit
    =/  kid  (~(gut by kid.fat) i.pax ^+(fat [~ ~]))
    fat(kid (~(put by kid.fat) i.pax $(fat kid, pax t.pax)))
  ::
  ++  jab
    |*  [pax=pith fun=$-(_?>(?=(^ fil.fat) u.fil.fat) _?>(?=(^ fil.fat) u.fil.fat))]
    ^+  fat
    =/  kid  (got pax)
    (put pax (fun kid))
  ::
  ++  put
    |*  [pax=pith dat=*]
    ^+  fat
    =>  .(dat `_?>(?=(^ fil.fat) u.fil.fat)`dat, pax `pith`pax)
    |-  ^+  fat
    ?~  pax  fat(fil `dat)
    =/  kid  (~(gut by kid.fat) i.pax ^+(fat [~ ~]))
    fat(kid (~(put by kid.fat) i.pax $(fat kid, pax t.pax)))
  ::
  ++  rut
    =|  here=pith
    |*  fun=$-([pith _?>(?=(^ fil.fat) u.fil.fat)] _?>(?=(^ fil.fat) u.fil.fat))
    ^+  fat
    %=  fat
      fil  ?~(fil.fat ~ `(fun here u.fil.fat))
      kid  (~(rut by kid.fat) |=([iot=iota f=_fat] ^$(here (snoc here iot), fat f)))
    ==
  ::
  ++  tap
    =|  pax=pith
    =|  out=(list (pair pith _?>(?=(^ fil.fat) u.fil.fat)))
    |-  ^+   out
    =?  out  ?=(^ fil.fat)  :_(out [pax u.fil.fat])
    =/  kid  ~(tap by kid.fat)
    |-  ^+   out
    ?~  kid  out
    %=  $
      kid  t.kid
      out  ^$(pax (weld pax /[p.i.kid]), fat q.i.kid)
    ==
  ::  Serialize to map
  ::
  ++  tar
    (~(gas by *(map pith _?>(?=(^ fil.fat) u.fil.fat))) tap)
  --
+$  pate  [[%p p=ship] q=pith]
  
+$  dita  (each iota aura)
+$  zeta  (each iota aura)
::  $pish: Pattern match over a path
::  
::    Ending with & indicates that the path match continues with
::    Ending with | indicates that the path match stops
::
+$  pish
  $+  pish
  $@(? [i=dita t=pish])
+$  rail
  $+  rail
  $@(? [i=zeta t=rail])
+$  conf  (map term pith)
+$  crew  (map term pith)
::  $deck: ???
::
::  apse, cove, dais, 
+$  card  (pair pith note)
+$  request
  [src=pith dest=pith val=*]
+$  nack
  $%  [%get p=(set pith)]
      [%sec p=(set pith)]
      [%err p=tang]
  ==
+$  update  diff
+$  watch  (list update)
::
+$  err
  $%  [%here =pith]
      [%goof err=*]
      [%fail err=*]
  ==

+$  response-status
  $%  [%done ~]
      err
  ==
+$  page  (pair stud *)
:: +$  cage  (pair stud vase)
::
+$  made
  [=stud init=(unit pail) =conf]
+$  note
  $%  [%make made] :: todo: configuration values, init cannot be ^ if installing over
      [%poke =pail]
      [%tomb cas=(unit case)]    :: tom
      [%cull ~]
      [%link from=pith src=stud] :: XX deprecate
  ==
+$  raw-poke
  (pair flow vial)
+$  yard
  $+  yard
  $~  ~
  (map iota hall)
+$  rely
  [=term =leaf]
::
+$  peer
  $:  run-nonce=@uvJ
      ~
  ==
+$  mate  (map ship peer)
+$  meet  (pair ship peer)
::
+$  dish  (pair pith mode)
+$  yarn  (pair aeon mode)
::  $leaf: diff over namespace
+$  leaf
  (axal mode)
::
+$  lore  (axal idea)
+$  idea
  $:  =saga :: original binding
      thru=(unit stud) :: any role
      =pail :: transformed binding
  ==
+$  dive
  $:  con=(map [grab=stud role=stud grow=stud] stud)
      by-grab=(jug stud [role=stud stud]) :: indexed by from type
      by-grow=(jug stud [role=stud stud]) :: indexed by to type
  ==
::
++  unix
  =<  unix
  |%
  +$  unix
    $:  =clay
        =iris
        =behn
        =gall
        =eyre
    ==
  +$  behn  (jug @da pith)
  +$  clay
    (map [src=pith hand=pith] [case=@ud =desk =path as=(unit mark)])
  +$  iris
    (set [src=pith hand=pith])
  +$  eyre
    $:  by-id=(map @ta pith)
        by-pith=(map pith @ta)
        bind=(map binding:^^eyre pith)
    ==
  ++  gall
    =<  gall
    |%
    +$  gall
      $:  peer=(jug pith pith)
          peek=(map pith peek:^^gall)
      ==
    --
  --
::
+$  stem
  $~  [*ever %x %stud *vase]
  %+  pair  ever
  $%  [%x =pail]
      [%y =pail kids=(map pith [=ever =mode =pail])]
      [%z =pail kids=(map pith [=ever =mode =pail])]
  ==
+$  twig
  $~  [*ever %x %stud ~]
  %+  pair  ever
  $%  [%x =vial]
      [%y =vial kids=(map pith [=ever =mode =vial])]
      [%z =vial kids=(map pith [=ever =mode =vial])]
  ==
++  wand
  |^
  $~  [%x *ever [%$ ~] ~]
  $:  =care
      =ever
      =vial
      kids=(map pith [=ever =vial])
  ==
  ++  make
    |=  [=care at=pith ax=(axal room)]
    ^-  ^$
    =/  rot  (need fil.ax)
    :+  care  (get-ever:room rot)
    :-  (to-vial:room rot)
    %-  ~(gas by *(map pith [ever vial]))
    =-  %+  turn  ~(tap by -)
        |=([k=pith v=[ever vial]] [(welp at k) v])
    ?+    care  !!
        %x  ~
        %y
      =.  ax  ~(snip of ax)
      (~(run by ~(tar of ax)) |=(r=room [(get-ever:room r) (to-vial:room r)]))
    ::
        %z
      (~(run by ~(tar of ax)) |=(r=room [(get-ever:room r) (to-vial:room r)]))
    ==  
  --
::
::  $cane: (deprecated)
+$  cane
  $~  [%x *ever [%$ *vase] ~]
  $:  =care
      =ever
      =pail
      kids=(map pith [=ever =pail])
  ==
::
++  make-cane
  |=  [=care at=pith ax=(axal room)]
  ^-  cane
  =/  rot  (need fil.ax)
  ?>  ?=(%icon -.seat.rot)
  =/  =icon  icon.seat.rot
  :+  care  ever.icon
  :-  [state.rot state.icon]
  %-  ~(gas by *(map pith [ever pail]))
  =-  %+  turn  ~(tap by -)
      |=([k=pith v=[ever pail]] [(welp at k) v])
  ?+    care   !!
      %x  ~
      %y
    =.  ax  ~(snip of ax)
    =.  fil.ax  ~
    (~(run by ~(tar of ax)) |=(r=room [(get-ever:room r) (to-pail:room r)]))
  ::
      %z
    =.  fil.ax  ~
    (~(run by ~(tar of ax)) |=(r=room [(get-ever:room r) (to-pail:room r)]))
  ==
++  dejs
  =,  dejs:format
  |%
  ++  pail
    |=  fun=$-([@ json] vase)
    |=  jon=json
    ^-  ^pail
    ?>  ?=(^ jon)
    ?>  ?=(%o -.jon)
    =/  std  (~(got by p.jon) %stud)
    =/  dat  (~(got by p.jon) %data)
    ?>  ?=(%s -.std)
    [`@tas`p.std (fun [p.std dat])]
  --
::
::  !!stud refers to imp/ different from $vial
+$  diff  [=pith =ever dat=(unit vial)]
:: +$  mace  [=pith =ever =diff]
::
++  sign
  |^
  $%  [%poke status=response-status]
      [%conf conf]
  ==
  +$  conf
    $%  [%val ~]
        [%pith ~ q=pith]
    ==
  --
:: $ewer: deprecated
+$  ewer  (pair stud vase)
+$  move  (pair pith card)
::
+$  icon
  $:  =ever
      state=vase
      history=(list *)
      migration=(list *)
  ==
:: subscription metadata
+$  brig
  $+  brig  (axal cane)
  
+$  fleet
  $+  fleet
  $~  ~
  (map ship brig)
+$  hall  hall:room
+$  seat
  $%  [%icon =icon]
      [%soil =soil]
  ==
::  $room: state of a shrub
::    
::    TODO: refactor for networking?
++  room
  =<  room
  |%
  +$  hall
    $%  [%exit pith]
        [%room room]
    ==
  +$  room
    $+  room
    $:  code=stud
        state=stud
        =conf
        =seat
    ==
  ++  get-ever
    |=  rom=room
    ^-  ever
    ?>  ?=(%icon -.seat.rom)
    ever.icon.seat.rom
  ::
  ++  to-pail
    |=  rom=room
    ^-  pail
    ?>  ?=(%icon -.seat.rom)
    [state.rom state.icon.seat.rom]
  ++  to-vial
    |=  rom=room
    [p q.q]:(to-pail rom)
  ::
  ++  de-hall-soft
    |=  hal=hall
    ^-  (unit room)
    ?.  ?=(%room -.hal)
      ~
    `+.hal
   ++  de-hall
     |=  hal=hall
     (need (de-hall-soft hal))
  --
+$  bowl
  $:  src=name :: [=ship =pith]
      our=@p   :: our
      were=pith :: XX: rename to here
      here=pith :: 
      now=@da
      eny=@uvJ
      deps=(map term (pair pith lore))
      kids=lore
  ==
++  dbug
  |%
  ++  bowl
    |=  =^bowl
    ^-  tank
    =/  bol  bowl(kids ~, deps ~)
    >bol<
  ++  epic
    |=  =^epic
    ^-  tank
    >~(key by ~(tar of epic))<
  ++  deps
    |=  deps=(map term (pair pith ^epic))
    =-  (sell !>(-))
    (turn ~(tap by deps) |=([=term =pith e=^epic] [term pith]))
  --
++  quay
  =<  quay
  |%
  +$  quay   (pair lash (unit port))
  ++  get-care
    |=  q=quay
    ^-  care
    ?~  q.q
      %x
    p.u.q.q
  --
+$  fief  [=deed =quay]
::
::  $port: Children API
::
::
+$  lash
  [state=curb poke=(set stud)]
+$  dock
  [state=curb poke=(set stud) =kids]
:: +$  slip
::  [state=stud diffs=(set stud) =kids] ::  
+$  deps  band
::  $band: Dependencies
::
+$  band  $+(band (map term fief))
+$  lads  $+(lads (map pish lash))
+$  dare  ?(%y %z)
+$  port  (pair dare lads)
+$  kids  (unit port)
+$  tear
  $%  [%only wan=curb hav=stud]
      [%not not=curb hav=stud]
  ==

::  $dude: virtual namespace binding
::
+$  dude
  $_  ^&
  |%
  ::  $state: the state of this value in the urbit namespace
  ::
  ::    For instance, a message would be
  ::      [author=ship time-sent=time message=txt]
  ::
  ++  state  *stud
  ++  work
    |~  =bowl
    *vase
  ::
  ::  +kids: Some nodes in the namespace define what children are
  ::  allowed to be under them. For instance, it should not  be allowed
  ::  to create /~hastuc-dibtux/chats/unit-731/blog-post-1. This is
  ::  nonsensical because blog posts don't go in chats.
  ++  kids   *^kids
  ::
  ::  +deps: Some nodes in the namespace might like to hear about other
  ::  things that happen in the namespace. For instance, a substack-type
  ::  software would like to know where the wallet software is located
  ::  in the name
  ++  deps   *(map term fief)
  --
++  o-till  !!
::|=  plot=*
::^-  firm
::|%
::++  state  state:plot
::++  poke   *(set stud)
::++  kids   kids:plot
::++  deps   deps:plot
::++  form   
::  ^-  ^form
::  ~&  %accessing-bad-form
::  *^form
::--
+$  o-soil
  $:  init=(pair ever (map name ever))
      dirt=(unit o-dirt)
  ==
+$  o-dirt
  $:  =ever
      =vase
  ==
::  $farm: produce reactive value
+$  o-farm
  $_  ^|
  |=  =bowl
  *vase
::  $kook: type of the value in the urbit namespace
::
+$  kook
  $_  ^&
  |%
  ::  $state: the state of this value in the urbit namespace
  ::
  ::    For instance, a message would be
  ::    ```hoon
  ::    [author=ship time-sent=time message=txt]
  ::    ```
  ::
  ::    ```
  ++  state  *curb
  ::  $poke: a poke is a request to change a value in teh urbit
  ::  namespace.
  ::
  ::    For instance a blocked list that is a set of users would be
  ::      [%add who=user]
  ::      [%del who=user]
  ::
  ::
  ++  poke   *(set stud)
  ++  form   *^form
  ::
  ::  +kids: Some nodes in the namespace define what children are
  ::  allowed to be under them. For instance, it should not  be allowed
  ::  to create /~hastuc-dibtux/chats/unit-731/blog-post-1. This is
  ::  nonsensical because blog posts don't go in chats.
  ++  kids   *(unit port)
  ::
  ::  +deps: Some nodes in the namespace might like to hear about other
  ::  things that happen in the namespace. For instance, a substack-type
  ::  software would like to know where the wallet software is located
  ::  in the name
  ++  deps   *(map term fief)
  --
+$  wave
  $:  code=stud
      =dock
      =crew
      live=?
  ==
+$  tide  (axal wave)
::
+$  form
  $_  ^|
  |_  [=bowl =saga]
  ::  +reduce: apply %poke, producing state and IO
  ::
  ::    ('liam'' ~) [%add who='ruby'] -> ('liam' 'ruby')
  ::    ('liam' 'ruby' ~) [%del who='ruby'] -> ('liam')
  ++  poke
    |~  [=stud val=vase]
    *(quip card pail)
  ++  init
    |~  old=(unit pail)
    *(quip card pail)
  --
+$  gang  (axal cove)
+$  cove
  $:  =crew
      code=stud
      rime=case
  ==
::
++  peon
  |%
  ++  match
    |=  [nedl=pish hstk=pith]
    ^-  ?
    ?@  nedl
      |(nedl =(~ hstk))
    ?~  hstk
      |
    ?:  ?=(%& -.i.nedl)
      &(=(p.i.nedl i.hstk) $(nedl t.nedl, hstk t.hstk))
    ?@  i.hstk
      =(p.i.nedl %tas)
    &(=(-.i.hstk p.i.nedl) $(nedl t.nedl, hstk t.hstk))
      
  ++  find
    |=  [pax=pith pis=(set pish)]
    ^-  (unit pish)
    ?~  lis=(skim ~(tap in pis) |=(pish (match +< pax)))
      ~
    `i.lis
  --
++  press
  |_  =post
  ++  disk  ^-  ^disk  ?@(q.post ~ +.q.post)
  ++  stud   q.post
  ++  eject
    |=  =^pith
    ^-  [kind:ford _post _pith]
    ~|  ejecting/pith
    =^  dis=^disk  pith  
      ?>  ?=([%cod *] pith)
      (eject:floppy t.pith)
    ?>  ?=([kind:ford tack @ *] pith)
    =/  =kind:ford  i.pith
    =/  =tack  i.t.pith
    :+  kind  [tack ?@(dis i.t.t.pith [i.t.t.pith ship.dis term.dis])]
    t.t.t.pith
    
  ++  slip
    |=  [=kind:ford pax=^pith]
    =/  [@ p=^post =^pith]
      (eject pax)
    (~(pith press p) kind)
  ++  path
    |=  =kind:ford
    (pout (pith kind))
  ::
  ++  pith
    |=  =kind:ford
    :-  %cod
    %+  welp  ~(pith floppy disk)
    :-  kind
    :-  p.post
    =-  ~[-]
    ?@  q.post  q.post
    mark.q.post
  --
++  floppy
  |_  =disk
  ++  eject
    |=  =^pith
    ^+  [disk pith]
    ?:  ?=([%std *] pith)
      [~ t.pith]
    ?>  ?=([[%p @] @ *] pith)
    [[+.i.pith i.t.pith] t.t.pith]
  ++  pith
    ^-  ^pith
    ?@  disk
      #/std
    [p/ship.disk term.disk ~]
  --
++  lte-pith
  |=  [a=pith b=pith]
  ^-  ?
  ?~  a  &
  ?~  b  |
  ?.  =(i.a i.b)
    %+  lte-dime
      ?^(i.a i.a [%tas i.a])
    ?^(i.b i.b [%tas i.b])
  $(a t.a, b t.b)
::
++  lte-dime
  |=  [a=dime b=dime]
  ^-  ?
  ?.  =(p.a p.b)  
    (aor -.a -.b)
  ?+  p.a  (lte q.a q.b)
    %rd             (lte:rd q.a q.b)
    %rh             (lte:rh q.a q.b)
    %rq             (lte:rq q.a q.b)
    %rs             (lte:rs q.a q.b)
    %s              !=(--1 (cmp:si q.a q.b))
    ?(%t %ta %tas)  (aor q.a q.b)
  ==
::
++  dprint  ^dprint
++  easy-print  ^easy-print
++  slow                                                ::  get arms in core
  |=  typ=type
  ^-  (list (list term))
  ?+    typ  ~
      [%hold *]  $(typ ~(repo ut typ))
      [%hint *]  $(typ ~(repo ut typ))
      [%core *]
    %-  zing
    %+  turn  ~(tap by q.r.q.typ)
      |=  [a=term b=tome]
    %+  turn  ~(tap by q.b)
      |=  [c=term *]
    ~[a c]
  ==
--
