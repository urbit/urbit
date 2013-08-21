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
  |=  who=ship  ^-  brad
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
      loq=(unit case)                                   ::  working version
      pyr=pyre                                          ::  compose cache
      war=(map ,@tas coal)                              ::  variables
      sac=(list skit)                                   ::  library stack
      sev=(map ,@tas (set ,[p=@ud q=@ud r=wire]))       ::  message servers
      tem=(map ,[p=@ud q=@ud r=wire] ,@da)              ::  timeouts 
      hit=[p=@ud q=(list ,@t)]                          ::  command history
      sur=[p=@ud q=(qeu vase)]                          ::  result history
      god=[p=@ud q=(map ,@ud task)]                     ::  process state
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
    $:  gee=type                                        ::  '*gene'
        liz=type                                        ::  '*(list ,@t)'
        pah=type                                        ::  '*path'
        noq=type                                        ::  '*note'
        tak=type                                        ::  '*tart'
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
    gee.typ  (pal (vice '*gene'))
    liz.typ  (pal (vice '*(list ,@t)'))
    pah.typ  (pal (vice '*path'))
    noq.typ  (pal (vice '*note'))
    tak.typ  (pal (vice '*tart'))
    vas.typ  (pal (vice '*vase'))
  ==
++  brat  ,[[who=ship bran] brad]                       ::  don't ask why
++  brim  (list ,[p=ship q=brad])                       ::  session
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
  ++  fear                                              ::    fear:be
    |=  tea=wire                                        ::  validate wire
    ^-  ?                                               ::  XX  hack
    =+  fat=(feat tea)
    =+  dog=(~(get by q.god) p.fat)
    ?~  dog  |
    =+  gib=(~(get by q.wip.u.dog) q.fat)
    ?~  gib  |
    ?.  ?=([%ma *] r.fat)  |
    (~(has by q.u.gib) t.r.fat)
  ::
  ++  feat                                              ::    feat:be
    |=  tea=wire                                        ::  decode wire
    ^-  [p=@ud q=@ud r=wire]
    ?>  ?=([@ @ *] tea)
    =+  [ped=(slay i.tea) wad=(slay i.t.tea)]
    ?>  &(?=([~ %% %ud @] ped) ?=([~ %% %ud @] wad))
    [q.p.u.ped q.p.u.wad t.t.tea]
  ::
  ++  fest                                              ::    fest:be
    |=  [gyp=@ud hen=duct]                              ::  find a task
    (fi gyp hen (need (~(get by q.god) gyp)))
  ::
  ++  fist                                              ::    fist:be
    |=  hen=duct                                        ::  new task
    =+  [gyp=p.god gyr=*task]
    =:  p.god  +(p.god)
        q.god  (~(put by q.god) p.god *task)
      ==
    (fi gyp hen gyr)
  ::
  ++  lake                                              ::    lake:be
    |=  [hen=duct gyp=@ud pid=@ud lap=wire]             ::  deliver wakeup
    ^-  [p=(list move) q=brat]
    abet:lash:abet:(glob:(past:(fest gyp hen) pid) lap [%wake ~])
  ::
  ++  lead                                              ::    lead:be
    |=  [tea=wire hen=duct]                             ::  route note
    ^-  [p=wire q=_ra:*fi]
    =+  fat=(feat tea)
    [r.fat (past:(fest p.fat hen) q.fat)]
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
      %kill  =+  gyp=?>(?=(^ fog) i.fog)
             ?:  =(0 gyp)  [~ +<.^^$]
             abet:kill:(fest gyp hen)
      %ling  ?>  ?=(^ fog)
             =>  .(fog (weld t.fog `(list ,@ud)`[i.fog ~]))
             [[[[~ %iron who] hen [%helo prot]] ~] +<.^^$]
      %noop  [~ +<.^^$]
      %thee  abet:lash:(lean tea hen fav)
      %went  ?.((fear tea) [~ +<.^^$] abet:lash:(lean tea hen fav))
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
    =+  yes=(~(get by sev) cha)
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
  ++  prot                                              ::    prot:be
    ^-  [p=path q=prod]                                 ::  get prompt
    ?>  ?=(^ fog)
    ?.  =(0 i.fog)
      perd:(fest i.fog ~)
    :+  %text
      :(weld (trip (rap 3 [hox '/' cwd ~])) "=" ?~(cws "" (spud cws)) "> ")
    ~
  ::
  ++  fi                                                ::    fi:be
    |=  [gyp=@ud hen=duct gyr=task]                     ::  process task
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
      =+  zif=((full (ifix [gay gay] lark:lo)) [1 1] (trip lin))
      ?~  q.zif 
        =+  duf=[p=~(rend co ~ %ud p.p.zif) q=~(rend co ~ %ud q.p.zif)]
        (warn "<syntax error at [{p.duf} {q.duf}]>")
      (limp p.u.q.zif)
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
    ++  kill
      ^+  .
      =+  pew=(sort (turn (~(tap by q.wip.gyr) ~) |=([p=@ud *] p)) lth)
      |-  ^+  ..kill
      ?~  pew  ..kill
      $(pew t.pew, ..kill abet:goon:(past i.pew))
    ::
    ++  lash                                            ::    lash:fi:be
      ^.  .                                             ::  execute task
      =+  pew=(sort (turn (~(tap by q.wip.gyr) ~) |=([p=@ud *] p)) lth)
      |-  ^+  ..lash
      ?~  pew  ..lash
      $(pew t.pew, ..lash abet:grip:(past i.pew))
    ::
    ++  limp                                            ::    limp:fi:be 
      |=  kaw=(list lath)                               ::  start pipeline
      ^+  +>
      ?~  kaw  +>
      $(kaw t.kaw, +>.$ (pant i.kaw ?:(=(~ t.kaw) ~ [~ +(p.wip.gyr)])))
    ::
    ++  pant                                            ::    pant:fi:be 
      |=  [lat=lath nex=(unit ,@ud)]                    ::  start process
      %=  +>
        p.wip.gyr  +(p.wip.gyr)
        q.wip.gyr  (~(put by q.wip.gyr) p.wip.gyr [nex ~ [~ ~ ~ %n *claw lat]])
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
      ?~(top [%none "[waiting...]" ~] r.u.top)
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
        ?:  &(?=(%r -.s.orb) |(=(~ p.s.orb) =(~ loz)))
          =>  (gird ~)
          =>  guff
          =.  ..ra  ?~(nex ..ra abet:(glob:(past u.nex) ~ [%pipe ~]))
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
        :-  ((hard (list gilt)) +:(bust 2 poc))
        =+  doy=(bust 3 poc)
        ?~  +.doy  [~ %r ~]
        :-  ((hard (list slip)) +>-.doy)
        [%r ~ (bust 7 doy)]
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
          %_(+>.$ ..ra (warn (spud (meat kit))), s.orb [%r ~])
        =+  tyk=`kite`[p.p.u.rot q.p.u.rot r.kit r.p.u.rot q.u.rot]
        ?>  =(kit tyk)
        +>.$(p.orb (~(del by p.orb) q.p.u.dup))
      ::
      ++  gasp                                          ::  logical path
        ^-  path
        [hox cwd ?~(loq wen (scot u.loq)) cws]
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
      ++  glee                                          ::  assemble stack
        =+  [kas=sac boy=nub]
        |-  ^-  [(unit coal) _+>]
        ?~  kas  [[~ boy] +>.$]
        =^  vid  +>.$  $(kas t.kas)
        ?~  vid  [~ +>.$]
        =^  lez  +>.$  (grok | (gnat i.kas) u.vid)
        ?~  lez
          [~ ?.(&(?=(^ duv) =(%crud -.q.i.duv)) +>.$ good:+>.$(sac t.kas))]
        [[~ q.u.lez] +>.$]
      ::
      ++  glib                                          ::  pending note
        |=  [lap=wire nob=nose]
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
          ?~  err  +>.^^$(orb [~ ~ ~ %r ~])
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
      ++  gnat                                          ::  skit to path
        |=  sik=skit
        (weld q.sik `path`[?~(p.sik wen u.p.sik) %lib r.sik])
      ::
      ++  goad                                          ::  print skit
        |=  sik=skit
        ^+  +>
        %+  gram  ~
        :+  %note  '^'
        :-  %leaf
        ;:  weld
          (spud q.sik)
          ?~(p.sik "=" (spud u.p.sik ~)) 
          "/lib" 
          (spud r.sik)
          "/hoon"
        ==
      ::
      ++  good                                          ::  print skits
        =+  kas=sac
        |-  ^+  +>
        ?~(kas +> (goad:$(kas t.kas) i.kas))
      ::
      ++  goon                                          ::  kill
        ^+  .
        .(s.orb [%r ~])
      ::
      ++  grab                                          ::  chase simple path
        |=  lam=lamb  ^-  gene
        ?-  -.lam
          &  =+  tes=(sky [%cx hox %main wen %bin p.lam %hoon ~])
             (grad [hox ?^(tes %main cwd) wen p.lam ~])
          |  p.lam
        ==
      ::
      ++  grad                                          ::  path to gene
        |=  pax=path  ^-  gene
        [%clsg (turn pax |=(a=@ta [%dtpt %ta a]))]
      ::
      ++  gram                                          ::  add action
        |=  [hom=duct fav=card] 
        %_(+> duv [[hom fav] duv])
      ::
      ++  gran                                          ::  add actions
        |=  vid=(list ,[p=duct q=card])
        ^+  +>
        ?~(vid +> $(vid t.vid, +> (gram i.vid)))
      ::
      ++  gray                                          ::  process result
        |=  ton=toon
        ^-  [(unit) _+>]
        ?-  -.ton
          %0  [[~ p.ton] +>]
          %1  [~ (glum ((list path) p.ton) s.orb)]
          %2  [~ (gram(orb [~ ~ ~ %r ~]) ~ [%crud %exit p.ton])]
        ==
      ::
      ++  grid                                          ::  process result
        |=  [ton=toon fun=||(* _+>)]
        ^+  +>
        ?-  -.ton
          %0  (fun p.ton)
          %1  (glum ((list path) p.ton) s.orb)
          %2  (gram(orb [~ ~ ~ %r ~]) ~ [%crud %exit p.ton])
        ==
      ::
      ++  grin                                          ::  process result
        |=  [ton=toon hog=boar]
        ^+  +>
        ?-  -.ton
          %0  (haul (fret p.ton))
          %1  (glum ((list path) p.ton) hog)
          %2  (gram(orb [~ ~ ~ %r ~]) ~ [%crud %exit p.ton])
        ==
      ::
      ++  grip                                          ::  step to completion
        |-  ^+  +
        =+(a=grit ?:(=(+.$ a) +.$ $(+.$ a)))
      ::
      ++  grit                                          ::  work step
        |-  ^+  +
        ?.  =(~ p.orb)  +
        =+  hog=s.orb
        ?-    -.hog
            %n                                          ::  new
          =+  gen=?:(?=(0 -.q.hog) s.q.hog p.q.hog)
          ?~  joy.p.hog
            =^  juy  +.$  (grow ~ gasp)
            ?~  juy  +.$
            $(s.orb hog(joy.p [~ (need (mang [food:zu war u.juy] sky))]))
          ?~  ran.p.hog
            =^  noh  +.$  glee
            ?~  noh  +.$
            =^  nur  +.$  (gray (mong [slap u.noh gen] sky))
            ?~  nur  +.$
            $(s.orb hog(ran.p nur))
          ?:  ?=(1 -.q.hog)
             =.  +.$  (gybe ~ -.u.ran.p.hog +.u.ran.p.hog ~)
             $(s.orb [%r ~])
          ?:  ?=(2 -.q.hog)
            $(s.orb [%t u.ran.p.hog])
          ?~  pux.p.hog 
            =^  wim  +.$  
              (gray (mong [slap u.joy.p.hog (grab q.q.hog)] sky))
            ?~  wim  +.$
            $(s.orb hog(pux.p [~ (jump %bin ((hard path) +.u.wim))]))
          =+  ^=  mop  |-  ^-  (list ,@tas)
                       ?~  r.q.hog  ~
                       =+  mor=$(r.q.hog t.r.q.hog)
                       ?.(?=(| -.i.r.q.hog) mor (weld p.i.r.q.hog mor))
          ?~  jiv.p.hog
            =^  woh  +.$  (grow mop u.pux.p.hog)
            ?~  woh  +.$
            $(s.orb hog(jiv.p woh))
          ?~  kyq.p.hog
            =^  mux  +.$
              (gray (mong [fuel:zu r.q.hog u.jiv.p.hog] sky))
            ?~  mux  +.$
            $(s.orb hog(kyq.p mux))
          ?~  gam.p.hog
            =^  lez  +.$  (grok | u.pux.p.hog u.kyq.p.hog)
            ?~  lez  +.$
            $(s.orb hog(gam.p [~ q.u.lez]))
          %-  grin  :_  hog
          %-  mong  :_  sky
          [fapp:zu u.gam.p.hog u.ran.p.hog]
        ::
            %r                                          ::  running
          ?~  p.hog  +.$
          ?:  =(~ r.orb)
            ?:  =(~ q.orb)  +.$
            =^  pud  q.orb  ~(get to q.orb)
            abet:(pong:(ox p.p.pud) q.p.pud)
          =^  pud  r.orb  ~(get to r.orb)
          (grin (mong [fane:zu [p.p.pud q.p.pud u.p.hog]] sky) hog)
        ::
            %t                                          ::  transform
          ?:  =(~ q.orb)  +.$
          =^  neb  q.orb  ~(get to q.orb)
          =+  pun=q.p.neb
          ?>  ?=(%pipe -.pun)
          ?~  p.pun  
            =.  +.$  (gybe ~)
            $(s.orb [%r ~])
          %+  grid  (mong [slit -.p.hog p.u.p.pun] sky)
          |=  noy=typo
          =|  zil=(list)
          |-  ^+  +.^^$
          ?~  q.u.p.pun 
            (gybe ~ noy (flop zil))
          %+  grid  (mong [slam [p.hog [p.u.p.pun i.q.u.p.pun]]] sky)
          |=  zom=*
          ^$(q.u.p.pun t.q.u.p.pun, zil [+.zom zil])
        ==
      ::
      ++  grok                                          ::  extend config
        |=  [sot=? pax=path boy=coal]
        ^-  [(unit ,[p=? q=coal]) _+>]
        =+  wiz=(~(get by p.pyr) wer pax boy)
        ?^  wiz  [[~ & u.wiz] +>.$] 
        =^  gar  +>.$  (gray (mong [fuss:zu sot pax] sky))
        ?~  gar  [~ +>.$]
        =>  .(gar ((hard (unit ,[p=@uvI q=*])) u.gar))
        ?~  gar  [[~ | boy] +>.$]
        =+  wex=(~(get by q.pyr) wer p.u.gar boy)
        ?^  wex  [[~ & u.wex] +>.$]
        =+  xow=(~(get by r.pyr) q.u.gar boy)
        ?^  xow  [[~ & u.xow] +>.$]
        =^  yeq  +>.$  (gray (mong [slap boy q.u.gar] sky))
        ?~  yeq  [~ +>.$]
        :-  [~ & u.yeq]
        %=  +>.$
          p.pyr  (~(put by p.pyr) [wer pax boy] u.yeq)
          q.pyr  (~(put by q.pyr) [wer p.u.gar boy] u.yeq)
          r.pyr  (~(put by r.pyr) [q.u.gar boy] u.yeq)
        ==
      ::
      ++  grim
        |=  [paw=(list path) boy=coal]
        ^-  [(unit ,[p=(list path) q=coal]) _+>]
        ?~  paw  [[~ ~ boy] +>.$]
        =^  wuh  +>.$  (grok & i.paw boy)
        ?~  wuh  [~ +>.$]
        =^  hyq  +>.$  $(paw t.paw, boy q.u.wuh)
        ?~  hyq  [~ +>.$]
        ?.  p.u.wuh  [hyq +>.$]
        [[~ [i.paw p.u.hyq] q.u.hyq] +>.$]
      ::
      ++  grow
        |=  [mod=(list ,@tas) pax=path]
        ^-  [(unit coal) _+>]
        =+  ^=  paw  ^-  (list path)
            =+  :*  mog=`path`~[hox %main wen] 
                    rim=(scag 2 pax)
                ==
            :*  (weld (scag 3 pax) `path`[%con ~])
                (weld mog `path`[%nat rim])
                (turn mod |=(a=@tas (weld mod `path`[%alt a rim])))
            ==
        =+  sho=(scag 3 pax)
        =+  boy=(grub pax)
        |-  ^-  [(unit coal) _+>.^$]
        =^  hyq  +>.^$  (grim paw boy)
        ?~  hyq  [~ +>.^$]
        ?~  sho  [[~ q.u.hyq] +>.^$]
        %=  $
          sho  t.sho
          boy  q.u.hyq
          paw  (turn paw |=(a=path (weld a [i.sur ~])))
        ==
      ::
      ++  grub                                          ::  initial compose
        |=  pax=path  ^-  coal
        :-  [%cell [%cell [%atom %p] pah.typ] -.nub]
        [[who pax] +.nub]
      ::
      ++  guff                                          ::  kill all depends
        ^+  .
        =+  yop=(~(tap by p.orb) *(list ,[p=@ud q=kite]))
        |-  ^+  ..guff
        ?~  yop  ..guff
        %=  $
          yop     t.yop
          ..guff  (gulf (bist %au (scot %ud p.i.yop) ~) q.i.yop)
        ==
      :: 
      ++  gull                                          ::  request control
        |=  [tea=wire him=ship ryf=riff]
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
            =+  yes=(~(get by sev) cha)
            ?~(yes ~ u.yes)
        %_    +>.$
            sev
          %+  ~(put by sev)  cha
          ?:  ton
            (~(put in yes) gyp pid lap)
          (~(del in yes) gyp pid lap)
        ==
      ::
      ++  gush 
        |=  [wak=@da gyp=@ud pid=@ud lap=wire]
        ^+  +>
        +>.$(tem (~(put by tem) [gyp pid lap] wak))
      ::
      ++  gust
        |=  [gyp=@ud pid=@ud lap=wire]
        +>.$(tem (~(del by tem) [gyp pid lap]))
      ::
      ++  gybe                                          ::  pipe forward
        |=  pun=(unit ,[p=typo q=(list)])
        ^+  +>
        ?~  nex
          ?~  pun  +>
          (gran (turn q.u.pun |=(a=* [~ (gyve p.u.pun a)])))
        +>.$(..ra abet:(glob:(past u.nex) ~ [%pipe pun]))
      ::
      ++  gyve                                          ::  print vase
        |=  [toy=typo val=*]  ^-  card
        =+  caf=((hard calf) (need (mang [felt:zu toy] sky)))
        ?:  =([~ [%atom %t]] caf)
          [%tell ((hard ,@t) val) ~]
        ?:  =([~ %wall] caf)
          [%tell ((hard (list ,@t)) val)]
        [%talk (dish:ut caf val)]
      ::
      ++  haft                                          ::  process gift
        |=  guf=gilt
        ^+  +>
        ?-    -.guf
            %%  (gybe ~ +<.guf +>.guf ~)
            %mu (gybe ~ +<.guf ((hard (list)) +>.guf))
        ::
            %va
          =+  tey=((hard ,[p=@tas q=(unit)]) +.guf)
          %=  +>.$
            war  ?~(q.tey (~(del by war) p.tey) (~(put by war) p.tey u.q.tey))
          ==
        ::
            *
          =+  gud=((hard gift) guf)
          |-  ^+  +>.^$
          ?-  -.gud
            %%   !!
            %cs  +>.^$(cws p.gud)
            %cd  +>.^$(cwd p.gud)
            %de  (gram ~ %note '#' q.gud)
            %ha  (gram ~ %crud %soft [p.gud ~])
            %ho  (gram ~ %crud %soft p.gud)
            %la  (gram ~ %talk p.gud)
            %lo  (gran (turn p.gud |=(a=tank [~ %talk a])))
            %mu  !!
            %mx  |-  ^+  +>.^^$
                 ?~  p.gud  +>.^^$
                 $(p.gud t.p.gud, +>.^^$ ^$(gud i.p.gud))
            %ok  (gram [/c ~] %info who p.gud q.gud)
            %sc  good:+>.^$(sac ?~(p.gud ?~(sac ~ +.sac) [u.p.gud sac]))
            %te  (gram ~ %tell p.gud)
            %th  (gram [/e ~] %that p.gud q.gud)
            %va  !!
            %xx  (gram ~ p.gud)
            %xy  (gram [p.gud /b ~] q.gud)
          == 
        ==
      ::
      ++  hale                                          ::  process gifts
        |=  guz=(list gilt)
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
          %ht  (gram [/e [%b (bist [%ma lap])] ~] [%band who ~])
          %oy  (gump | p.gal gyp pid lap)
          %up  +>(..ra (hoop lap pid))
          %wa  (gust gyp pid lap)
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
          %ht  (gram [/e [%b (bist [%ma lap])] ~] [%band who p.gal])
          %oy  (gump & p.gal [gyp pid lap])
          %up  +>(..ra (hoot lap pid p.gal))
          %wa  (gush p.gal gyp pid lap)
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
            +>.$(+>.$ (glib lap [%% p.fav]))
          ::
              %eg
            ?>  ?=(%writ -.fav)
            +>.$(lug ~, +>.$ (glib lap [%eg +.fav]))
          ::
              %es
            ?>  ?=(%writ -.fav)
            +>.$(+>.$ (glib lap [%eg +.fav]))
          ::
              %ht
            ?>  ?=(%thee -.fav)
            +>.$(+>.$ (glib lap [%ht +.fav]))
          ::
              %up
            ?>  ?=(%line -.fav)
            +>.$(+>.$ (glib lap [%up +.fav]))
          ::
              %oy 
            ?>  ?=(%wart -.fav)
            +>.$(+>.$ (glib lap [%oy +.fav]))
          ::
              %wa  
            ?>  ?=(%wake -.fav)
            +>.$(+>.$ (glib lap [%wa ~]))
          ::
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
    ++  coax                                            ::  parse flags
      |=  coo=tape  ^-  gene
      :+  %cnts  [[~ 1] ~]
      |-  ^-  gent
      ?~  coo  ~
      :_  $(coo t.coo)
      ?:  &((gte i.coo 'a') (lte i.coo 'z'))
        [[%cnbc i.coo] [%dtpt %f &]]
      ?>  &((gte i.coo 'A') (lte i.coo 'Z'))
      [[%cnbc (sub i.coo 32)] [%dtpt %f |]]
    ::
    ++  cone                                            ::  parse conf
      %+  cook  
        |=  a=(list (list ^cone))
        ?~  a  ~
        ?~(i.a $(a t.a) [i.i.a $(i.a t.i.a)])
      %-  star
      ;~  pose
        ;~(plug (ifix [kel ker] (stag %| (most ace sym))) (easy ~))
      ::
        ;~  plug 
          (ifix [sel ser] (stag %& (stag %cltr (most ace wide:vez))))
          (easy ~)
        ==
      ::
        %+  ifix  [gal gar]
        ;~  pose
          ;~  plug
            (stag %& (cook coax (plus ;~(pose low hig))))
            ;~  pose
              ;~  pfix  ;~(plug sem ace)
                ;~  plug
                  %+  cook  |=(a=^cone a)
                  (stag %& (stag %cnts (stag [[~ 1] ~] loon:vez)))
                  (easy ~)
                ==
              ==
              (easy ~)
            ==
          ==
        ::
          ;~  plug
            (stag %& (stag %cnts (stag [[~ 1] ~] loon:vez)))
            (easy ~)
          ==
        ==
      ==
    ::
    ++  lark                                            ::  parse lark
      %+  cook  |=(a=^lark a)
      ;~  pose
        lute
      ::
        ;~  pfix  tis
          ;~  pose
            %+  cook
              |=  [a=@tas b=(list gene)]
              ^-  (list lath)
              :~  [%1 [%cltr b]]
                  [%0 %0 [%& %set] ~ [[%clsg [%dtpt %tas a] ~]]]
              ==
            ;~(plug sym (star ;~(pfix ace wide:vez)))
          ::
            %+  cook
              |=  a=@tas
              :~  [%0 %0 [%& %none] ~ [%bcts %null]]
                  [%0 %0 [%& %set] ~ [%clsg [%dtpt %tas a] ~]]
              ==
            ;~(pfix tis sym)
          ==
        ==
      ==
    ::
    ++  lamb
      %+  cook  |=(a=^lamb a)
      ;~  pose
        (stag %& sym)
        (stag %| (stag %clsg poor:vez))
      ==
    ::
    ++  loth
      %+  cook  |=(a=lath a)
      ;~  pfix  col
        %+  stag  %0
        ;~  plug
          ;~(pose ;~(pfix zap ;~(pose (cold %2 zap) (easy %1))) (easy %0))
          lamb
          cone
          (stag %clsg (star ;~(pfix ace wide:vez)))
        ==
      ==
    ::
    ++  lute 
      %+  cook 
        |=  a=(list lath)  ^+  a
        =+  b=(flop a)
        ?.(&(?=(^ b) ?=(2 -.i.b)) b [[%1 p.i.b] t.b])
      ;~  plug
        ;~(pose loth (stag %2 wide:vez))
        %-  star
        ;~  pose
          ;~(pfix ;~(plug sem ace) (stag %2 wide:vez))
          ;~(pfix ace loth)
        ==
      ==
    --
  ::
  ++  zu                                                ::  user level
    |%
    ++  fane                                            ::  deliver note 
      |=  [pux=path nog=nose tas=vase]
      ^-  vase
      %+  slam  tas
      ;:  slop
        [[%atom %da] now] 
        [pah.typ pux] 
        [noq.typ nog]
      ==
    ::
    ++  fapp                                            ::  launch app
      |=  [gum=vase arg=vase]
      ^-  vase
      %+  slam
        %+  slam  gum
        %+  slop 
          [[%atom %da] now] 
        [[%atom %%] (shax :(mix eny now (shax p.god)))]
      arg
    ::
    ++  felt                                            ::  type to calf
      |=  typ=type  ^-  calf
      ~(dole ut typ)
    ::
    ++  food                                            ::  standard subject
      |=  [war=(map ,@ta vase) vax=vase]
      ^-  vase
      ;:  slop
        ;:  slop
          %+  slop
            [[%atom %da] now]
          [[%atom %ta] ~(rent co [~ %da now])]
        ::
          %+  slop
            [[%atom %p] who]
          [[%atom %ta] hox]
        ::
          [liz.typ q.hit]
        ==
      ::
        =+  voy=(~(tap to q.sur) ~)
        |-  ^-  vase
        ?~(voy [[%atom %n] ~] (slop i.voy $(voy t.voy)))
      :: 
        ?~  war
          vax
        %-  slop
        :_  vax
        |-  ^-  vase
        ?+  war  !!     ::  XX some inference weirdness here?
          [* ~ ~]  [[%face p.n.war p.q.n.war] q.q.n.war]
          [* ~ ^]  (slop $(r.war ~) $(war r.war))
          [* ^ ~]  (slop $(l.war ~) $(war l.war))
          [* ^ ^]  :(slop $(r.war ~, l.war ~) $(war l.war) $(war r.war))
        ==
      ==
    ::
    ++  fuel
      |=  [zul=(list cone) vax=vase]
      =+  [hed=(slot 2 vax) tal=(slot 3 vax)]
      |-  ^-  vase
      ?~  zul  (slop hed tal)
      ?-  -.i.zul
        &  $(tal (slap tal p.i.zul))
        |  $(zul t.zul)
      ==
    ::
    ++  fuss                                            ::  gene and hash
      |=  [sot=? pax=path]
      ^-  (unit ,[p=@uvI q=gene])
      =+  ape=((hard apex) .^(%cy pax))
      =+  yab=(~(get by q.ape) %hoon)
      ?:  &(sot ?=(~ yab))  ~
      =+  mot=((hard ,@) .^(%cx (weld pax /hoon)))
      :+  ~  ?~(yab (sham mot) u.yab)
      (scan (trip mot) (ifix [gay gay] tall:vez(wer (juke pax))))
    --
  --
--
. ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::              section 4bC, shell vane                 ::
::
=|  $:  dez=(map duct brim)                             ::  state by ship
    ==                                                  ::
|=  [now=@da eny=@ sky=||(* (unit))]                    ::  current invocation
^?                                                      ::  opaque core
|%                                                      ::  poke/peek pattern
++  beat                                                ::  process move
  |=  [wru=(unit writ) tea=wire hen=duct fav=curd]
  =>  .(fav ((hard card) fav))
  ^-  [p=(list move) q=vane]
  ?:  ?=([%crud *] fav)
    [[[wru [/d hen] [%flog fav]] ~] ..^$]
  ?:  ?=(%wake -.fav)
    =+  ^=  fiy
        =|  fiy=(list ,[p=duct q=[p=@ud q=@ud r=wire]])
        |-  ^+  fiy
        ?~  dez  fiy
        =.  fiy  $(dez l.dez)
        =.  fiy  $(dez r.dez)
        |-  ^+  fiy
        ?~  q.n.dez  fiy
        %=    $
            q.n.dez  t.q.n.dez
            fiy
          |-  ^+  fiy
          ?~  tem.q.i.q.n.dez  fiy
          =.  fiy  $(tem.q.i.q.n.dez l.tem.q.i.q.n.dez)
          =.  fiy  $(tem.q.i.q.n.dez r.tem.q.i.q.n.dez)
          ?.  (lte q.n.tem.q.i.q.n.dez now)  fiy
          [[p.n.dez p.n.tem.q.i.q.n.dez] fiy]
        ==
    =|  wam=(list move)
    |-  ^-  [p=(list move) q=vane]
    ?~  fiy  [wam ..^^$]
    =+  dos=(need (~(get by dez) p.i.fiy))
    =+  beg=`brat`[[p.i.dos bred] q.i.dos]
    =+  yub=(lake:((be beg) now eny sky) p.i.fiy q.i.fiy)
    %=  $
      fiy      t.fiy
      wam      (weld p.yub wam)
      dez.^^$  (~(put by dez.^^$) p.i.fiy [[p.i.dos +.q.yub] t.dos])
    ==
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
      ?~  wru
        =+  ade=(adit hen)
        =+  hem=`helm`?-(ade %lead [%lead p.i.u.dus], %iron %iron, %gold %gold)
        [[[[~ hem p.i.u.dus] [[%b tea] hen] fav] ~] ..^$]
      =+  beg=`brat`[[p.i.u.dus bred] q.i.u.dus]
      =+  yub=(leap:((be beg) now eny sky) tea hen fav)
      :-  p.yub
      ..^$(dez (~(put by dez) hen [[p.i.u.dus +.q.yub] t.u.dus]))
  ::
      %init
    ?~  wru  !! 
    :-  :~  [wru hen fav]
            [wru [[%b tea] hen] [%hail ~]]
        ==
    ..^$(dez (~(put by dez) hen [[q.u.wru (bard q.u.wru)] u.dus]))
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
  =|  doz=(unit ,@da)
  |-  ^+  doz
  ?~  dez  doz
  =.  doz  $(dez l.dez)
  =.  doz  $(dez r.dez)
  |-  ^+  doz
  ?~  q.n.dez  doz
  %=    $
      q.n.dez  t.q.n.dez
      doz
    |-  ^+  doz
    ?~  tem.q.i.q.n.dez  doz
    =.  doz  $(tem.q.i.q.n.dez l.tem.q.i.q.n.dez)
    =.  doz  $(tem.q.i.q.n.dez r.tem.q.i.q.n.dez)
    (hunt doz ~ q.n.tem.q.i.q.n.dez)
  ==
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
  |=  [our=ship ren=@tas his=ship syd=disc lot=coin tyl=path]
  ^-  (unit)
  ~
::
++  stay  `vase`!>(dez)
--
