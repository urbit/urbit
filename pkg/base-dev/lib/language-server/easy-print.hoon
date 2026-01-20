::  Fast type printing that's easy on the eyes or your money back
::
=>  |%
    +$  cape  [p=(map @ud wine) q=wine]
    +$  wine
      $@  $?  %noun
              %path
              %type
              %void
              %wall
              %wool
              %yarn
          ==
      $%  [%mato p=term]
          [%gate p=hoon q=type r=wine]
          [%core p=(list @ta) q=wine]
          [%face p=term q=wine]
          [%list p=term q=wine]
          [%pear p=term q=@]
          [%bcwt p=(list wine)]
          [%plot p=(list wine)]
          [%stop p=@ud]
          [%tree p=term q=wine]
          [%unit p=term q=wine]
      ==
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
  =|  top-level=?  ::  don't need circumfix punctuation
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
        [%gate *]
      =^  sam  gid
        ?.  ?=([%plot * * *] r.q.ham)
          ?:  ?=(%plot -.r.q.ham)
            %-  (slog -:$(q.ham r.q.ham) ~)
            `gid
          `gid
        [`u=- +]:$(q.ham i.p.r.q.ham, top-level |)
      :_  gid
      :+  %rose
        :-  ?>  ?=(%core -.q.q.ham)
            ?:  ?=(%dry q.p.q.q.q.ham)
              " -> "
            " ~> "
        ?:  top-level
          ["" ""]
        ["(" ")"]
      :+  ?~(sam leaf+"_" u.sam)
        =/  res  (mule |.((~(play ut q.q.ham) p.q.ham)))
        ?-  -.res
          %&  duck(sut p.res)
          %|  leaf+"###"
        ==
      ~
    ::
        [%core *]
      =^  sam  gid
        ?.  ?=([%plot * * ~] q.q.ham)
          `gid
        [`u=- +]:$(q.ham i.p.q.q.ham)
      :_  gid
      ?~  sam
        :+  %rose
          [[' ' ~] ['<' ~] ['>' ~]]
        |-  ^-  (list tank)
        ?~  p.q.ham  ~
        [[%leaf (rip 3 i.p.q.ham)] $(p.q.ham t.p.q.ham)]
      :+  %rose
        [" -> " "" ""]
      :+  u.sam
        :+  %rose
          [[' ' ~] ['<' ~] ['>' ~]]
        |-  ^-  (list tank)
        ?~  p.q.ham  ~
        [[%leaf (rip 3 i.p.q.ham)] $(p.q.ham t.p.q.ham)]
      ~
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
      :_(gid [%leaf '$' ~(rend co [%$ p.q.ham q.q.ham])])
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
      [%gate *]
    !!
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
  ?:  (~(meet ut sut) -:!>(*type))
    [dex %type]
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
    ?:  ?=([[%$ * [[%$ @ *] ~ ~]] ~ ~] q.r.q.sut)
      =/  dad  $(sut p.sut)
      :-  p.dad
      ~!  q.r.q.sut
      [%gate q.n.q.q.n.q.r.q.sut sut(r.p.q %gold) q.dad]
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
    $(sut q.sut)
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
