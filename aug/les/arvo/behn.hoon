!:
::  behn (4b), shell
::
|=  pit=vase
^-  vane
=>  =~
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::              section 4bA, shell models               ::
::
|%
++  bard                                                ::  new session
  |=  who=seat  ^-  brad
  %*  .  *brad
    hox    (scot %p who)
    cwd    %try
    fog    [0 ~]
    p.hit  1
    p.sur  1
    p.god  1
  ==
++  brad                                                ::  session/dynamic
  $:  fog=(list ,@ud)                                   ::  virtual consoles
      hox=@ta                                           ::  identity text
      cws=path                                          ::  working spur
      cwd=@tas                                          ::  working desk
      way=(map ,@tas vase)                              ::  variables
      ser=(map ,@tas (set ,[p=@ud q=@ud r=wire]))       ::  message servers
      hit=[p=@ud q=(list ,@t)]                          ::  command history
      sur=[p=@ud q=(qeu vase)]                          ::  result history
      god=[p=@ud q=(map ,@ud gyre)]                     ::  process state
  ==                                                    ::
::                                                      ::
++  bran                                                ::  static "state"
  $:  nub=vase                                          ::  
      ^=  vax                                           ::  chestnut vases
    $:  sot=vase                                        ::  'slot'
    ==                                                  ::
      ^=  gen                                           ::
    $:  yom=gene                                        ::  '*(set ,@tas)'
        zim=gene                                        ::  '*(map ,@tas ,*)'
    ==                                                  ::
      ^=  typ                                           ::  chestnut types
    $:  cof=type                                        ::  '*conf'
        gee=type                                        ::  '*gene'
        liz=type                                        ::  '*(list ,@t)'
        pah=type                                        ::  '*path'
        noq=type                                        ::  '*note'
        tak=type                                        ::  '*task'
        vas=type                                        ::  '*vase'
    ==                                                  ::
  ==                                                    ::
::
++  bred                                                ::  make defaults
  =+  nib=pit
  =+  pal=~(play ut p.nib)
  ~+
  %*  .  *bran
    nub      nib
    sot.vax  (slap nib (vice 'slot'))
    yom.gen  (vice '*(set ,@tas)')
    zim.gen  (vice '*(map ,@tas ,*)')
    cof.typ  (pal (vice '*conf'))
    gee.typ  (pal (vice '*gene'))
    liz.typ  (pal (vice '*(list ,@t)'))
    pah.typ  (pal (vice '*path'))
    noq.typ  (pal (vice '*note'))
    tak.typ  (pal (vice '*task'))
    vas.typ  (pal (vice '*vase'))
  ==
++  brat  ,[[who=seat bran] brad]                       ::  don't ask why
++  brim  (list ,[p=seat q=brad])                       ::  session
--                                                      ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::              section 4bB, session engine             ::
::
|%
++  be                                                  ::  repl/shell
  |=  brat                                              ::  core state
  |=  [now=@da eny=@ sky=_|+(* *(unit))] 
  =+  wen=(scot %da now)
  =+  wer=`path`[hox cwd wen cws]
  =+  rew=(flop wer)
  =+  vez=(vang | wer)
  |%
  ++  fest                                              ::    fest:be
    |=  [gyp=@ud hen=duct]                              ::  find a task
    (fi gyp hen (need (~(get by q.god) gyp)))
  ::
  ++  fist                                              ::    fist:be
    |=  hen=duct                                        ::  new task
    =+  [gyp=p.god gyr=*gyre]
    =:  p.god  +(p.god)
        q.god  (~(put by q.god) p.god *gyre)
      ==
    (fi gyp hen gyr)
  :: 
  ++  heat                                              ::    heat:be
    |=  [hen=duct het=hate]                             ::  dispatch http req
    ^-  [p=(list move) q=brat]
    =+  sud=(lout p.het)
    ?~  sud
      :_  +<.^^$
      :~  :+  [~ %iron who]
            hen
          :-  %that
          :-  %raw
          [404 ~ [~ (tact "http error 404")]]
      ==
    =<  abet
    =<  lash
    =<  abet
    %+  glib:(past:(fest p.u.sud hen) q.u.sud)
      r.u.sud 
    [%ht s.u.sud q.het r.het]
  ::
  ++  lead                                              ::    lead:be
    |=  [tea=wire hen=duct]                             ::  route note
    ^-  [p=wire q=_ra:*fi]
    ?>  ?=([@ @ *] tea)
    =+  [ped=(slay i.tea) wad=(slay i.t.tea)]
    ?>  &(?=([~ %% %ud @] ped) ?=([~ %% %ud @] wad))
    [t.t.tea (past:(fest q.p.u.ped hen) q.p.u.wad)]
  ::
  ++  lean                                              ::    lean:be
    |=  [tea=wire hen=duct fav=card]                    ::  deliver card
    ^+  *fi
    =+  lay=(lead tea hen)
    ?>  ?=([%ma *] p.lay)
    abet:(glob:q.lay t.p.lay fav)
  ::
  ++  leap                                              ::    leap:be
    |=  [tea=wire hen=duct fav=card]                    ::  handle event
    ^-  [p=(list move) q=brat]
    ?:  ?=([%crud *] fav)
      [[[[~ %iron who] [/d hen] [%flog fav]] ~] +<.^^$]
    ?+  -.fav  
             [[[[~ %iron who] hen fav] ~] +<.^^$]
      %hail  [[[[~ %iron who] hen [%helo prot]] ~] +<.^^$]
      %line  =+  gyp=?>(?=(^ fog) i.fog)
             =<  abet
             ?:  =(0 gyp)
               lash:(gill:(fist hen) p.fav)
             lash:(como:(fest gyp hen) p.fav)
      %ling  ?>  ?=(^ fog)
             =>  .(fog (weld t.fog `(list ,@ud)`[i.fog ~]))
             [[[[~ %iron who] hen [%helo prot]] ~] +<.^^$]
      %noop  [~ +<.^^$]
      %thee  (heat hen p.fav)
      %went  abet:lash:(lean tea hen fav)
      %writ  abet:lash:(loam tea hen +.fav)
      %wart  (lion hen +.fav)
    ==
  ::
  ++  loam                                              ::    loam:be
    |=  [tea=wire hen=duct rot=riot]                    ::  handle response
    ^+  *fi
    =+(a=(lead tea hen) abet:(gall:q.a p.a rot))
  ::
  ++  lion                                              ::    lion:be
    |=  [hen=duct him=@p cha=@ta num=@ud val=(unit ,*)] ::  handle message
    ^-  [(list move) brat]
    =+  yes=(~(get by ser) cha)
    ?~  yes  [~ +<.^^$]
    =+  sey=(~(tap by u.yes) *(list ,[p=@ud q=@ud r=wire]))
    |-  ^-  [(list move) brat]
    ?~  sey  [~ +<.^^^$]
    =^  von  +<.^^^$
      =<  abet
      =<  lash
      =<  abet
      =<  abet
      %-  pong:(ox:(past:(fest p.i.sey hen) q.i.sey) r.i.sey)
      [%wart him cha num val]
    =^  vun  +<.^^^$  $(sey t.sey)
    [(weld von vun) +<.^^^$]
  ::
  ++  loot                                              ::    loot:be
    |=  [uri=purl rut=rout]                             ::  match route
    ^-  (unit scud)
    ?.  |-  ^-  ?
        ?~  p.rut  |
        =(i.p.rut `host`r.p.uri)
      ~
    =+  tac=*path
    |-  ^-  (unit scud)
    ?~  q.rut
      :-  ~
      :-  :(weld (flop q.q.uri) tac s.rut)
      `scar`[p.uri (flop tac) p.q.uri s.rut]
    ?:  |(?=(~ q.q.uri) !=(i.q.rut i.q.q.uri))
      ~
    $(q.rut t.q.rut, q.q.uri t.q.q.uri, tac [i.q.rut tac])
  ::
  ++  lout                                              ::    lout:be
    |=  uri=purl                                        ::  match http req
    ^-  (unit ,[p=@ud q=@ud r=path s=scab])
    =+  dog=`(list ,[p=@ud q=gyre])`(~(tap by q.god) ~)
    |-  ^-  (unit ,[p=@ud q=@ud r=path s=scab])
    ?~  dog  ~
    =+  pig=`(list ,[p=@ud q=beak])`(~(tap by q.wip.q.i.dog) ~)
    |-  ^-  (unit ,[p=@ud q=@ud r=path s=scab])
    ?~  pig  ^$(dog t.dog)
    =+  ask=`(list slip)`(~(tap by q.q.i.pig) ~)
    |-  ^-  (unit ,[p=@ud q=@ud r=path s=scab])
    ?~  ask  ^$(pig t.pig)
    ?.  ?=([%ht *] q.i.ask)
      $(ask t.ask)
    |-  ^-  (unit ,[p=@ud q=@ud r=path s=scab])
    ?~  p.q.i.ask  ^$(ask t.ask)
    =+  sem=(loot uri i.p.q.i.ask)
    ?~  sem  
      $(p.q.i.ask t.p.q.i.ask)
    [~ p.i.dog p.i.pig p.i.ask `scab`[`oryx`r.i.p.q.i.ask r.uri u.sem]]
  ::
  ++  lube                                              ::    lube:be
    ^-  vase                                            ::  build subject
    ;:  slop
      ;:  slop
        %+  slop
          [[%atom %da] now]
        [[%atom %ta] ~(rent co [~ %da now])]
      ::
        %+  slop
          [[%atom %p] who]
        [[%atom %ta] ~(rent co [~ %p who])]
      ::
        [liz.typ q.hit]
      ==
    ::
      =+  voy=(~(tap to q.sur) ~)
      |-  ^-  vase
      ?~(voy [[%atom %n] ~] (slop i.voy $(voy t.voy)))
    ::
      ?~  way
        nub
      %-  slop
      :_  nub
      |-  ^-  vase
      ?+  way  !!     ::  XX some inference weirdness here?
        [* ~ ~]  [[%face p.n.way p.q.n.way] q.q.n.way]
        [* ~ ^]  (slop $(r.way ~) $(way r.way))
        [* ^ ~]  (slop $(l.way ~) $(way l.way))
        [* ^ ^]  :(slop $(r.way ~, l.way ~) $(way l.way) $(way r.way))
      ==
    ==
  ::
  ++  prot                                              ::    prot:be
    ^-  prod                                            ::  get prompt
    ?>  ?=(^ fog)
    ?.  =(0 i.fog)
      perd:(fest i.fog ~)
    text/:(weld (trip (rap 3 [hox '/' cwd ~])) "=" ?~(cws "" (spud cws)) "> ")
  ::
  ++  fi                                                ::    fi:be
    |=  [gyp=@ud hen=duct gyr=gyre]                     ::  process task
    =|  duv=(list ,[p=duct q=card])
    |%
    ++  abet                                            ::    abet:fi:be
      ^-  [(list move) brat]                            ::  resolve
      =+  ^=  fod  ^+  [p=fog q=q.god]
          ?~  q.wip.gyr
            :-  (skip fog |=(a=@ud =(a gyp))) 
            (~(del by q.god) gyp)
          :-  ?:((lien fog |=(a=@ud =(a gyp))) fog [gyp fog])
          q=(~(put by q.god) gyp gyr)
      =+  sam==(fog p.fod)
      =:  fog    p.fod
          q.god  q.fod
        ==
      :_  +<.^^$
      %+  turn 
        (flop `_duv`?:(sam duv [[~ [%helo prot]] duv]))
      |=([p=duct q=card] [[~ %iron who] (weld p hen) q])
    ::
    ++  bitt  |=(lap=path [(scot %ud gyp) lap])         ::    bitt:fi:be
    ++  como                                            ::    como:fi:be
      |=  lin=@t                                        ::  command
      ^+  +>
      =+  ryg=~(top to paq.gyr)
      ?~  ryg  
        ~&  %como-no
        +>.$
      abet:abet:(pong:(ox:(past p.u.ryg) q.u.ryg) [%line lin])
    ::
    ++  gill                                            ::    gill:fi:be
      |=  lin=@t                                        ::  input line
      ^+  +>
      =+  zif=((full (ifix [gay gay] kral:lo)) [1 1] (trip lin))
      ?~  q.zif 
        =+  duf=[p=~(rend co ~ %ud p.p.zif) q=~(rend co ~ %ud q.p.zif)]
        (warn "<syntax error at [{p.duf} {q.duf}]>")
      %-  lamp
      ^-  (list lath)
      =+  kar=`lark`p.u.q.zif
      ?+  kar  !!
        [%ec *]  [[[hox %main wen %fun %echo ~] [~ ~] [%k p.kar]] ~]
        [%go *]  [+.kar ~]
      ==
    ::                                                  ::    hoop:fi:be
    ++  hoop                                            ::  delete prompt
      |=  [lap=wire pid=@ud]                            ::  XX ugly
      ^+  +>
      %=    +>
          paq.gyr
        %-  ~(gas to *(qeu gyro))  
        %+  skip
          (~(tap to paq.gyr) *(list gyro))
        |=(a=gyro &(=(pid p.a) =(lap q.a)))
      ==
    ::
    ++  hoot                                            ::    hoot:fi:be
      |=  [lap=wire pid=@ud pod=prod]                   ::  install prompt
      ^+  +>
      %_(+> paq.gyr (~(put to paq.gyr) [pid lap pod]))
    ::
    ++  lash                                            ::    lash:fi:be
      ^.  .                                             ::  execute task
      =+  pew=(sort (turn (~(tap by q.wip.gyr) ~) |=([p=@ud *] p)) lth)
      |-  ^+  ..lash
      ?~  pew  ..lash
      $(pew t.pew, ..lash abet:grip:(past i.pew))
    ::
    ++  lamp                                            ::    lamp:fi:be 
      |=  kaw=(list lath)                               ::  start pipeline
      ^+  +>
      ?~  kaw  +>
      $(kaw t.kaw, +>.$ (pant i.kaw ?:(=(~ t.kaw) ~ [~ +(p.wip.gyr)])))
    ::
    ++  pant                                            ::    pant:fi:be 
      |=  [lat=lath nex=(unit ,@ud)]                    ::  start process
      %=  +>
        p.wip.gyr  +(p.wip.gyr)
        q.wip.gyr  (~(put by q.wip.gyr) p.wip.gyr [nex ~ [~ ~ ~ %| lat]])
      ==
    ::
    ++  past                                            ::    past:fi:be
      |=  pid=@ud                                       ::  select process
      ^+  ra
      =+  bek=(need (~(get by q.wip.gyr) pid))
      ~(. ra pid p.bek q.bek r.bek)
    ::
    ++  perd                                            ::    perd:fi:be
      ^-  prod                                          ::  produce prompt
      =+  top=~(top to paq.gyr)
      ?~(top [%none "[waiting...]"] r.u.top)
    ::
    ++  warn                                            ::    warn:fi:be
      |=  txt=tape                                      ::  send warning
      ^+  +>
      +>(duv :_(duv [~ [%warn txt]]))
    ::
    ++  ra                                              ::    ra:fi:be
      |_  $:  pid=@ud                                   ::  process id
              nex=(unit ,@ud)                           ::  next in pipeline
              loz=(map wire goal)                       ::  waiting for
              orb=boor                                  ::  image
          ==
      ::
      ++  abet                                          ::  resolve
        ^+  ..ra
        ?:  &(=(& -.s.orb) |(=(~ p.s.orb) =(~ loz)))
          =.  .  (gird ~)
          =.  .  ?~(nex . (glob:(past u.nex) ~ [%over ~]))
          ..ra(q.wip.gyr (~(del by q.wip.gyr) pid))
        ..ra(q.wip.gyr (~(put by q.wip.gyr) pid nex loz orb))
      ::
      ++  bist  |=(lap=path (bitt (scot %ud pid) lap))  ::  form path
      ++  bust                                          ::  slice coal
        |=  [axe=axis vux=coal]
        ^-  coal
        =<  q
        %+  slam  sot.vax
        (slop [[%atom %%] axe] [vas.typ vux])
      ::
      ++  fret                                          ::  process coal
        |=  poc=coal
        ^-  beef
        :-  ((hard (list gift)) +:(bust 2 poc))
        =+  doy=(bust 3 poc)
        ?~  +.doy  [~ %& ~]
        :-  ((hard (list slip)) +>-.doy)
        [%& ~ (bust 7 doy)]
      ::
      ++  gall                                          ::  deliver result
        |=  [lap=wire rot=riot]
        ^+  +>
        ?.  ?=([%au * *] lap)
          ?>  ?=([%ma *] lap)
          (glob t.lap [%writ rot])
        =+  dup=(slay i.t.lap)
        ?>  ?=([~ %% %ud @] dup)
        =+  kit=(need (~(get by p.orb) q.p.u.dup))
        ?~  rot
          %_(+>.$ ..ra (warn (spud (meat kit))), s.orb [%& ~])
        =+  tyk=`kite`[p.p.u.rot q.p.u.rot r.kit r.p.u.rot q.u.rot]
        ?>  =(kit tyk)
        +>.$(p.orb (~(del by p.orb) q.p.u.dup))
      ::
      ++  gird                                          ::  change slips
        |=  ask=(list slip)
        ^+  +>
        =+  zal=(~(tap by loz) ~)
        =+  zim=(~(gas by *(map path goal)) ask)        ::  XX clumsy
        =.  +>.$
          |-  ^+  +>.^$
          ?~  zal  +>.^$
          %=  $
            zal    t.zal
            +>.^$  ?:((~(has by zim) p.i.zal) +>.^$ abet:pang:(ox p.i.zal))
          ==
        |-  ^+  +>.^$
        ?~  ask  +>.^$
        $(ask t.ask, +>.^$ abet:(pane:(ox p.i.ask) q.i.ask))
      ::
      ++  glib                                          ::  pending note
        |=  [lap=wire nob=note]
        ^+  +>
        %_(+> r.orb (~(put to r.orb) [lap nob]))
      ::
      ++  glob                                          ::  extern
        |=  [lap=wire fav=card]
        ^+  +>
        %_(+> q.orb (~(put to q.orb) [lap fav]))
      ::
      ++  glum                                          ::  blocked process
        |=  [gez=(list path) hog=boar]
        =|  [inx=@ud err=(list path) bez=(map ,@ud kite)]
        |-  ^+  +>.^$
        ?~  gez  
          ?:  =(~ err)
            +>.^$(orb [bez ~ ~ hog])
          |-  ^+  +>.^^$
          ?~  err  +>.^^$(orb [~ ~ ~ %& ~])
          $(err t.err, ..ra (warn (spud i.err)))
        =+  myt=(tame i.gez)
        ?~  myt
          $(gez t.gez, err [i.gez err]) 
        %=  $
          gez    t.gez
          inx    +(inx)
          bez    (~(put by bez) inx u.myt)
          +>.^$  (gulp (bist %au (scot %ud inx) ~) u.myt)
        ==
      ::
      ++  gram                                          ::  add action
        |=  [hom=duct fav=card] 
        %_(+> duv [[hom fav] duv])
      ::
      ++  gran
        |=  vid=(list ,[p=duct q=card])                 ::  add actions
        ^+  +>
        ?~(vid +> $(vid t.vid, +> (gram i.vid)))
      ::
      ++  grin                                          ::  process result
        |=  [ton=toon hog=boar]
        ^+  +>
        ?-  -.ton
          %0  (haul (fret p.ton))
          %1  (glum ((list path) p.ton) hog)
          %2  (gram(orb [~ ~ ~ %& ~]) ~ [%crud p.ton])
        ==
      ::
      ++  grip                                          ::  step to completion
        |-  ^+  +
        =+(a=grit ?:(=(+.$ a) +.$ $(+.$ a)))
      ::
      ++  grit                                          ::  work step
        ^+  .
        ?^  p.orb  .
        ?-    -.s.orb
            |  (grin (mong [fapp:zu lube p.s.orb] sky) s.orb)
            &
          ?~  p.s.orb  .
          ?:  =(~ r.orb)
            ?:  =(~ q.orb)  .
            =^  pud  q.orb  ~(get to q.orb)
            abet:(pong:(ox p.p.pud) q.p.pud)
          =^  pud  r.orb  ~(get to r.orb)
          (grin (mong [fane:zu [p.p.pud q.p.pud u.p.s.orb]] sky) s.orb)
        ==
      ::  
      ++  gull                                          ::  request control
        |=  [tea=wire him=seat ryf=riff]
        (gram ~[/c [%b tea]] [%warp him ryf])
      ::
      ++  gulf                                          ::  stop request
        |=  [tea=wire kit=kite]
        (gull tea r.kit [s.kit ~])
      ::
      ++  gulp                                          ::  start request
        |=  [tea=wire kit=kite]
        %^  gull  tea
          r.kit
        ^-  riff
        [s.kit ~ %& p.kit q.kit t.kit]
      ::
      ++  gump                                          ::  message server
        |=  [ton=? cha=@tas gyp=@ud pid=@ud lap=wire]
        ^+  +>
        =+  ^=  yes  ^-  (set ,[p=@ud q=@ud r=wire])
            =+  yes=(~(get by ser) cha)
            ?~(yes ~ u.yes)
        %_    +>.$
            ser
          %+  ~(put by ser)  cha
          ?:  ton
            (~(put in yes) gyp pid lap)
          (~(del in yes) gyp pid lap)
        ==
      ::
      ++  haft                                          ::  process gift
        |=  gud=gift
        ^+  +>
        ?-  -.gud
          %%   ?~  nex  
                 %+  gram  ~
                 ?:  =([~ %wall] +<.gud)
                   [%tell ((hard (list ,@t)) +>.gud)]
                 [%talk (dish:ut +.gud)]
               +>(..ra abet:(glob:(past u.nex) ~ [%pipe ~ p.gud q.gud ~]))
          %cs  +>(cws p.gud)
          %cd  +>(cwd p.gud)
          %de  (gram ~ %note '#' q.gud)
          %ha  (gram ~ %crud [p.gud ~])
          %ho  (gram ~ %crud p.gud)
          %la  (gram ~ %talk p.gud)
          %lo  (gran (turn p.gud |=(a=tank [~ %talk a])))
          %mu  ?~  nex  
                 ?:  =([~ %atom %t] p.gud)  $(gud [%% [~ %wall] q.gud])
                 (gran (turn q.gud |=(a=* [~ %talk (dish:ut [p.gud a])])))
               +>(..ra abet:(glob:(past u.nex) ~ [%pipe ~ +.gud]))
          %mx  |-  ^+  +>.^$
               ?~  p.gud  +>.^$
               $(p.gud t.p.gud, +>.^$ ^$(gud i.p.gud))
          %ok  (gram [/c ~] %into who p.gud q.gud)
          %te  (gram ~ %tell p.gud)
          %th  (gram ~ %that p.gud)
          %xx  (gram ~ p.gud)
          %xy  (gram [p.gud /b ~] q.gud)
        == 
      ::
      ++  hale                                          ::  process gifts
        |=  guz=(list gift)
        ^+  +>
        ?~(guz +> $(guz t.guz, +> (haft i.guz)))
      ::
      ++  haul                                          ::  process success
        |=  bof=beef
        ^+  +>
        =.  s.orb  r.bof
        =.  +>  (hale p.bof)
        (gird q.bof)
      ::
      ++  loss                                          ::  stop goal
        |=  [lap=wire gal=goal]
        ^+  +>
        ?-  -.gal
          %%   +>
          %eg  (gulf (bist %ma lap) p.gal)
          %es  (gull (bist %ma lap) p.gal q.gal ~)
          %ht  (gram [/e [%b (bist [%ma lap])] ~] [%bund who ~])
          %oy  (gump | p.gal gyp pid lap)
          %up  +>(..ra (hoop lap pid))
          %wa  !!
          %yo  +>
        ==
      ::
      ++  moor                                          ::  start goal
        |=  [lap=wire gal=goal]
        ^+  +>
        ?-    -.gal
          %%   +>
          %eg  (gulp (bist %ma lap) p.gal)
          %es  (gull (bist %ma lap) p.gal q.gal [~ r.gal])
          %ht  (gram [/e [%b (bist [%ma lap])] ~] [%bund who p.gal])
          %oy  (gump & p.gal [gyp pid lap])
          %up  +>(..ra (hoot lap pid p.gal))
          %wa  !!
          %yo  (gram [/a [%b (bist [%ma lap])] ~] [%want +.gal])
        ==
      ::
      ++  ox                                            ::  per delivery
        |=  lap=wire                                    ::  per request
        =+  gul=(~(get by loz) lap)
        =+  lug=gul
        |%
        ++  abet                                        ::  resolve
          ^+  +>.$
          ?~  lug
            ?~  gul  +>.$
            (loss(loz (~(del by loz) lap)) lap u.gul)
          ?~  gul
            (moor(loz (~(put by loz) lap u.lug)) lap u.lug)
          ?:  =(u.lug u.gul)  +>.$
          =.  +>.$  (loss(loz (~(del by loz) lap)) lap u.gul)
          (moor(loz (~(put by loz) lap u.lug)) lap u.lug)
        ::
        ++  pane  |=(gal=goal %_(. lug [~ gal]))        ::  set goal
        ++  pang  %_(. lug ~)                           ::  delete goal
        ++  pong                                        ::  accept note
          |=  fav=card
          ^+  +>
          ?>  ?=(^ lug)
          ?-    -.u.lug
              ~
            ?>  ?=(%pipe -.fav)
            +>.$(+>.$ (glib lap [%% ?~(p.fav ~ [~ q.u.p.fav])]))
          ::
              %eg
            ?>  ?=(%writ -.fav)
            +>.$(lug ~, +>.$ (glib lap [%eg +.fav]))
          ::
              %es
            ?>  ?=(%writ -.fav)
            +>.$(+>.$ (glib lap [%eg +.fav]))
          ::
              %ht  !!
              %up
            ?>  ?=(%line -.fav)
            +>.$(+>.$ (glib lap [%up +.fav]))
          ::
              %oy 
            ?>  ?=(%wart -.fav)
            +>.$(+>.$ (glib lap [%oy +.fav]))
          ::
              %wa  !!
              %yo 
            ?>  ?=(%went -.fav)
            +>.$(lug ~, +>.$ (glib lap [%yo +.fav]))
          ==
        --
      --
    --
  ::
  ++  lo                                                ::  command parsers
    |%
    ++  htap 
      ;~  pfix  fas
        %+  cook
          |=  [a=reef b=path]
          =+  pot=(flop (scag q.a rew))
          =+  gaw=(slag q.a rew)
          =+  pre=?:(p.p.a (scag q.p.a (flop gaw)) (flop (slag q.p.a gaw)))
          (weld pre ?~(pot b (weld b pot)))
        ;~  plug
          ;~  plug
            ;~  pose
              (cold [%| 0] lus)
              (cook |=(a=(list) [%| (lent a)]) (plus tis))
              (cook |=(a=(list) [%& (lent a)]) (star tar))
            ==
            (cook |=(a=(list) (lent a)) (star buc))
          ==
          ;~(sfix (more (cook |=(a=coin ~(rent co a)) nuck:so) fas) fas)
        ==
      ==
    ::
    ++  ipto                                            ::  lights
      %+  knee  *(list ,@tas)  |.  ~+
      %+  cook
        |=  a=(list (list ,@tas))  ^-  (list ,@tas)
        ?~(a ~ (weld i.a $(a t.a)))
      %+  most
        ;~(plug com ace)
      ;~  pose
        (cook |=(a=@ [a ~]) sym)
        (cook |=(a=@ (rip 3 a)) ;~(pfix lus sym))
      ==
    ::
    ++  ipty                                            ::  notices
      %+  knee  *(list ,[p=@tas q=crow])  |.  ~+
      (most ace ;~(plug sym ;~(pfix fas worc)))
    ::
    ++  kral                                            ::  parse lark
      ;~  pose
        ;~  pfix  col
          ;~  pose
            ;~  pfix  ;~(plug (jest 'cd') gap)
              (stag %cd sym)
            ==
          ::
            ;~  pfix  ;~(plug (jest 'cc') gap)
              (stag %cc worc)
            ==
          ::
            ;~  pfix  ;~(plug (jest 'to') gap)
              (stag %to (stag %p (most ace worc)))
            ==
          ::
            ;~  pfix  ;~(plug (jest 'do') gap)
              (stag %do (stag %l (most ace worc)))
            ==
          ::
            ;~  pfix  ;~(plug (jest 'kl') gap)
              (stag %kl (cook |=(a=dime ?>(?=(%ud p.a) q.a)) bisk:so))
            ==
          ::
            ;~  pfix  ;~(plug (jest 'so') gap)
              %+  stag  %so
              ;~  plug  
                sym
                ;~(pfix gap (stag %p (most ace worc)))
              ==
            ==
          ::
            (cold [%nk ~] (jest 'nk'))
            (cold [%ps ~] (jest 'ps'))
          ::
            %+  stag
              %go
            ;~  plug
              (thap %fun)
              ;~  pose 
                %+  ifix  [kel ker]
                ;~  pose
                  ;~  plug
                    ipto
                    ;~(pose ;~(pfix ;~(plug sem ace) ipty) (easy ~))
                  ==
                  (stag ~ ipty)
                ==
                (easy [~ ~])
              ==
              (stag %l (star ;~(pfix ace worc)))
            ==
          ==
        ==
      ::
        (stag %ec (stag %p (most ace worc)))
      ==
    ::
    ++  thap
      |=  rol=@ta
      ;~  pose
        htap
        %+  cook                                        ::  XX bletcherous
          |=  a=path  ^-  path
          ?>  ?=([@ @ @ *] a)
          [i.a i.t.t.a i.t.a t.t.t.a]
        ;~  plug
          %+  cook
            |=(a=seat ~(rent co ~ %p a))
          ;~(pose ;~(pfix sig fed:ag) (easy who))
        ::
          %+  cook
            |=  a=[p=@ta q=(unit ,[p=@ta q=(unit ,@ta)])]
            ?~  q.a      [~(rent co ~ %da now) %main rol p.a ~]
            ?~  q.u.q.a  [~(rent co ~ %da now) p.a rol p.u.q.a ~]
                         [u.q.u.q.a p.a rol p.u.q.a ~] 
          ;~  plug
            sym
            ;~  pose
              %+  stag  ~
              ;~  plug
                ;~(pfix fas sym)
                ;~(pose (stag ~ ;~(pfix fas sym)) (easy ~))
              ==
              (easy ~)
            ==
          ==
        ==
      ==
    ::
    ++  worc
      %+  knee  *crow  |.  ~+
      ;~  pose
        ;~  pfix  buc
          %+  cook
            |=  [a=path b=(list crow)]
            `crow`[%c [%f a] b]
          ;~  plug
            (thap %fit)
            (ifix [pel per] (most ace worc))
          ==
        ==
        (stag %g (stag ~ wide:vez))
      ==
    --
  ::
  ++  zu                                                ::  user level
    |%
    ++  dive                                            ::  opts to gene
      |=  cux=cone
      ^-  gene
      :-  :+  %cncl
            [%cnsg [%gas ~] [%cnbc %in] yom.gen]
          [%clsg (turn p.cux |=(a=@ [%dtpt %tas a]))]
      :+  %cncl
        [%cnsg [%gas ~] [%cnbc %by] zim.gen]
      :-  %clsg
      %+  turn  q.cux
      |=([p=@tas q=crow] [[%dtpt %tas p] (doul q)])
    ::
    ++  doul                                            ::  crow to gene
      |=  woc=crow
      ^-  gene
      ?-    -.woc
          %c  [%cnhp $(woc p.woc) $(woc [%p q.woc]) ~]
          %e  [%hxgl $(woc p.woc) ~]
          %f
        %+  scan  (trip ((hard ,@) .^(%cx (weld p.woc /hoon))))
        (full (ifix [gay gay] tall:vez(wer p.woc)))
      ::
          %g
        |-  ^-  gene
        ?~  p.woc
          q.woc
        [%tsgr ^$(woc [%f i.p.woc]) $(p.woc t.p.woc)]
      ::
          %k  [%hxgr $(woc p.woc) ~]
          %l
        |-  ^-  gene
        ?~  p.woc
          [%bcts %null]
        [^$(woc i.p.woc) $(p.woc t.p.woc)]
      ::
          %p
        |-  ^-  gene
        ?~  p.woc  !!
        ?~  t.p.woc
          ^$(woc i.p.woc)
        [^$(woc i.p.woc) $(p.woc t.p.woc)]
      ==
    ::
    ++  echo                                            ::  echo argument
      |=  [yun=vase woc=crow]
      ^-  tank
      =+  vax=(slap yun (doul woc))
      (dish:ut ~(dole ut p.vax) q.vax) 
    ::
    ++  ecto                                            ::  type only
      |=  [yun=vase woc=crow]
      ^-  tank
      (dial:ut ~(dole ut (~(play ut p.yun) (doul woc))))
    ::
    ++  fane                                            ::  deliver note 
      |=  [pux=path nog=note tas=vase]
      ^-  vase
      %+  slam  tas
      ;:  slop
        [[%atom %da] now] 
        [pah.typ pux] 
        [noq.typ nog]
      ==
    ::
    ++  fapp                                            ::  launch app
      |=  [yun=vase pax=path cux=cone arg=crow]
      ^-  vase
      %+  slam
        %+  slam
          %+  slam
            (slap nub (doul [%f pax]))
          ;:  slop 
            [[%atom %p] who] 
            [[%atom %da] now] 
            [[%atom %%] (shax :(mix eny now (shax p.god)))]
            [pah.typ pax]
          ==
        (slap yun (dive cux))
      (slap yun (doul arg))
    --
  --
--
. ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::              section 4bC, shell vane                 ::
::
=|  $:  dez=(map duct brim)                             ::  state by seat
    ==                                                  ::
|=  [now=@da eny=@ sky=||(* (unit))]                    ::  current invocation
^?                                                      ::  opaque core
|%                                                      ::  poke/peek pattern
++  beat                                                ::  process move
  |=  [wru=(unit writ) tea=wire hen=duct fav=curd]
  =>  .(fav ((hard card) fav))
  ^-  [p=(list move) q=vane]
  =+  dus=(~(get by dez) hen)
  ?~  dus
    ?+    -.fav  
      ~&  [%beat-none -.fav tea hen]
      ~|([%beat-none -.fav] !!)
    ::
        %init
      ?~  wru  !! 
      :-  [[wru hen fav] ~]
      ..^$(dez (~(put by dez) hen [[q.u.wru (bard q.u.wru)] ~]))
    ::
        ?(%loin %make %sith)
      [[[wru [/a [%b tea] hen] fav] ~] ..^$]
    ==
  ?>  ?=(^ u.dus)
  ?+    -.fav
      =+  beg=`brat`[[p.i.u.dus bred] q.i.u.dus]
      =+  yub=(leap:((be beg) now eny sky) tea hen fav)
      :-  p.yub
      ..^$(dez (~(put by dez) hen [[p.i.u.dus +.q.yub] t.u.dus]))
  ::
      %init
    ?~  wru  !! 
    $(fav [%hail ~], dez (~(put by dez) hen [[q.u.wru (bard q.u.wru)] u.dus]))
  ::
      %limn
    $(fav [%hail ~], dez (~(put by dez) hen (weld t.u.dus [i.u.dus ~])))
  ==
::
++  come  
  |=  old=vase
  ^-  vane
  ~|(%load-nest-bede !!)
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  flee  stay
++  load
  |=  new=vase
  ^-  vane
  ?.  (~(nest ut -:!>(dez)) & p.new)
    (come new) 
  ..^$(dez ((map duct brim) q.new))
::
++  raze  
  ^-  vane
  ..$(dez ~)
::
++  scry
  |=  [our=seat ren=@tas his=seat syd=disc lot=coin tyl=path]
  ^-  (unit)
  ~
::
++  stay  `vase`!>(dez)
--
