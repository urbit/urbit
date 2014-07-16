::
::  batz (4b), shell
::
|=  pit=vase
=>  =~
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::              section 4bA, shell models               ::
::
|%
++  ghat                                                ::  out result <-$
          $%  [%crud p=@tas q=(list tank)]              ::  error with trace
              [%hail ~]                                 ::  refresh
              [%helo p=path q=prod]                     ::  trigger prompt
              [%init p=@p]                              ::  report install
              [%line p=@t]                              ::  source line
              [%logo p=@]                               ::  logout
              [%note p=@tD q=tank]                      ::  show message
              [%save p=path q=@]                        ::  write atomic file
              [%talk p=tank]                            ::  show on console
              [%tell p=(list ,@t)]                      ::  dump lines
              [%veer p=@ta q=path r=@t]                 ::  install vane
              [%vega p=path]                            ::  reboot by path
              [%verb ~]                                 ::  reboot by path
              [%warn p=tape]                            ::  syslog
          ==                                            ::
++  kiss                                                ::  in request ->$
          $%  [%crud p=@tas q=(list tank)]              ::  error with trace
              [%hail ~]                                 ::  refresh
              [%hook ~]                                 ::  this term hung up
              [%harm ~]                                 ::  all terms hung up
              [%init p=@p]                              ::  report install
              [%kill p=~]                               ::  kill a task
              [%line p=@t]                              ::  source line
              [%ling ~]                                 ::  rotate interface
              [%limn ~]                                 ::  rotate ship
              [%make p=(unit ,@t) q=@ud r=@ s=?]        ::  wild license
              [%noop ~]                                 ::  no operation
              [%sith p=@p q=@uw r=?]                    ::  imperial generator
              [%wake ~]                                 ::  timer activate
              [%wart p=sock q=@tas r=path s=*]          ::  network request
          ==                                            ::
++  flog                                                ::  sent to %dill
          $%  [%crud p=@tas q=(list tank)]              ::
              [%text p=tape]                            ::
          ==                                            ::
++  hasp  ,[p=ship q=term]                              ::  see %gall
++  move  ,[p=duct q=(mold newt ghat)]                  ::  local move
++  newt                                                ::
          $%  $:  %a                                    ::  to %ames
          $%  [%cash p=@p q=buck]                       ::
              [%make p=(unit ,@t) q=@ud r=@ s=?]        ::
              [%sith p=@p q=@uw r=?]                    ::
              [%want p=sock q=path r=*]                 ::
          ==  ==                                        ::
              $:  %b                                    ::
          $%  [%hail ~]                                 ::  to %batz
              [%line p=@t]                              ::
          ==  ==                                        ::
              $:  %c                                    ::  to %clay
          $%  [%info p=@p q=@tas r=nori]                ::
              [%warp p=sock q=riff]                     ::
          ==  ==                                        ::
              $:  %d                                    ::  to %dill
          $%  [%flog p=flog]                            ::
          ==  ==                                        ::
              $:  %e                                    ::  to %eyre
          $%  [%band p=ship q=(list rout)]              ::
              [%that p=@ud q=love]                      ::
              [%them p=(unit hiss)]                     ::
          ==  ==                                        ::
              $:  %g                                    ::  to %gall
          $%  [%init p=ship]                            ::
              [%mess p=hasp q=ship r=cage]              ::
              [%nuke p=hasp q=ship]                     ::
              [%show p=hasp q=ship r=path]              ::
          ==  ==  ==                                    ::
++  rave                                                ::  see %clay
          $%  [& p=mood]                                ::  single request
              [| p=moat]                                ::  change range
          ==                                            ::
++  riff  ,[p=desk q=(unit rave)]                       ::  see %clay
++  sigh  ,[@tas p=sign]                                ::  sourced sign
++  sign                                                ::  in result $<-
          $%  [%crud p=@tas q=(list tank)]              ::  by any
              [%hail ~]                                 ::  by any
              [%helo p=path q=prod]                     ::  by %ames
              [%init p=@p]                              ::  by %ames
              [%mean p=(unit ,[p=term q=(list tank)])]  ::  by %gall
              [%nice ~]                                 ::  by %gall
              [%note p=@tD q=tank]                      ::  by %clay
              [%pipe p=(unit ,[p=tutu q=(list)])]       ::  by %batz
              [%rush p=logo q=*]                        ::
              [%rust p=logo q=*]                        ::
              [%thou p=httr]                            ::  by %eyre
              [%waft p=sock q=*]                        ::  by %ames
              [%went p=ship q=cape]                     ::  by %ames
              [%writ p=riot]                            ::  by %clay
          ==                                            ::
--
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
++  beau  ,[p=(unit ,@ud) q=(map wire goal) r=boor]     ::  next/want/thread
++  beef                                                ::  raw product
          $:  p=(list gilt)                             ::  actions
              q=(list slip)                             ::  requests
              r=boar                                    ::  state
          ==                                            ::
++  boar                                                ::  execution instance
          $%  [%n p=(unit coal) q=claw r=lath]          ::  new/ready
              [%r p=(unit worm)]                        ::  running/done
              [%t p=coal]                               ::  simple filter
          ==                                            ::
++  boor                                                ::  new thread
          $:  p=(map ,@ud kite)                         ::  dependencies
              q=(qeu ,[p=wire q=?(sign kiss)])          ::  waiting cards
              r=(qeu ,[p=wire q=nose])                  ::  pending notes
              s=boar                                    ::  execution
          ==                                            ::
++  brad                                                ::  session/dynamic
  $:  fog=(list ,@ud)                                   ::  task consoles
      fen=(map ,@tas ,@ud)                              ::  named tasks
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
      god=[p=@ud q=(map ,@ud task)]                     ::  task state
  ==                                                    ::
::                                                      ::
++  bran                                                ::  static "state"
  $:  nub=vase                                          ::
      $=  vax                                           ::  chestnut vases
    $:  sot=vase                                        ::  'slot'
    ==                                                  ::
      $=  gen                                           ::
    $:  yom=twig                                        ::  '*(set ,@tas)'
        zim=twig                                        ::  '*(map ,@tas ,*)'
    ==                                                  ::
      $=  typ                                           ::  chestnut types
    $:  gee=type                                        ::  '*twig'
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
    gee.typ  (pal (vice '*twig'))
    liz.typ  (pal (vice '*(list ,@t)'))
    pah.typ  (pal (vice '*path'))
    noq.typ  (pal (vice '*note'))
    tak.typ  (pal (vice '*tart'))
    vas.typ  (pal (vice '*vase'))
  ==
++  brat  ,[[who=ship bran] brad]                       ::  don't ask why
++  brim  (list ,[p=ship q=brad])                       ::  session
++  gyro  ,[p=@ud q=wire r=prod]                        ::  live prompt
++  task                                                ::
          $:  paq=(qeu gyro)                            ::  prompt queue
              wip=[p=@ud q=(map ,@ud beau)]             ::  processes
          ==                                            ::
--                                                      ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::              section 4bB, session engine             ::
::
|%
++  be                                                  ::  repl/shell
  |=  brat                                              ::  core state
  |=  [now=@da eny=@ sky=$+(* (unit))]
  =+  wen=(scot %da now)
  =+  wer=`path`[hox cwd wen cws]
  =+  rew=(flop wer)
  =+  vez=(vang | wer)
  |%
  ++  abet  `brat`+<.^$                                 ::    abet:be
  ++  fang                                              ::    fang:be
    |=  [gyp=@ud ted=@ud lap=wire]                      ::  validate waiter
    ^-  ?                                               ::  XX  hack
    =+  dog=(~(get by q.god) gyp)
    ?~  dog  |
    =+  gib=(~(get by q.wip.u.dog) ted)
    ?~  gib  |
    (~(has by q.u.gib) lap)
  ::
  ++  fanx                                              ::    fang:be
    |=  [gyp=@ud ted=@ud lap=wire]                      ::  validate waiter
    ^-  ?                                               ::  XX  hack
    =+  dog=(~(get by q.god) gyp)
    ?~  dog  |
    =+  gib=(~(get by q.wip.u.dog) ted)
    ?>  ?=(^ gib)
    ?.  ?=([%ma *] lap)  |
    (~(has by q.u.gib) t.lap)
  ::
  ++  fear                                              ::    fear:be
    |=  tea=wire                                        ::  validate wire
    ^-  ?                                               ::  XX  hack
    (fanx (feat tea))
  ::
  ++  feat                                              ::    feat:be
    |=  tea=wire                                        ::  decode wire
    ^-  [p=@ud q=@ud r=wire]
    ?>  ?=([@ @ *] tea)
    =+  [ped=(slay i.tea) wad=(slay i.t.tea)]
    ?>  &(?=([~ %$ %ud @] ped) ?=([~ %$ %ud @] wad))
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
  ++  flub                                              ::    flub:be
    |=  [gyp=@ud ted=@ud lap=wire]                      ::  delete timer
    ^+  +>
    ~&  [%flub tem]
    +>(tem (~(del by tem) [gyp ted lap]))
  ::
  ++  lake                                              ::    lake:be
    |=  [hen=duct gyp=@ud ted=@ud lap=wire]             ::  deliver wakeup
    ^-  [p=(list move) q=brat]
    abet:lash:abet:(glob:(past:(fest gyp hen) ted) lap [%wake ~])
  ::
  ++  lead                                              ::    lead:be
    |=  [tea=wire hen=duct]                             ::  route note
    ^-  [p=wire q=_ra:*fi]
    =+  fat=(feat tea)
    [r.fat (past:(fest p.fat hen) q.fat)]
  ::
  ++  lean                                              ::    lean:be
    |=  [tea=wire hen=duct sin=sign]                    ::  deliver card
    ^+  *fi
    =+  lay=(lead tea hen)
    ?>  ?=([%ma *] p.lay)
    abet:(glob:q.lay t.p.lay sin)
  ::
  ++  leap                                              ::    leap:be
    |=  [tea=wire hen=duct sin=sign]                    ::  accept response
    ^-  [p=(list move) q=brat]
    ?-  -.sin
      %crud  [[[hen [%slip %d %flog sin]] ~] +<.^^$]
      %hail  [[[hen %give sin] ~] +<.^^$]
      %helo  [~ +<.^^$]
      %init  [[[hen %give sin] ~] +<.^^$]
      %mean  [~ +<.^^$]
      %nice  [~ +<.^^$]
      %note  [[[hen %give sin] ~] +<.^^$]
      %pipe  !!
      %rush  ?.  (fear tea)  ::  legit
                [~ +<.^^$]
              abet:lash:(lean tea hen sin)
      %rust  ?.  (fear tea)  ::  legit
                [~ +<.^^$]
              abet:lash:(lean tea hen sin)
      %thou  ?.  (fear tea)  ::  legit
               [~ +<.^^$]
             abet:lash:(lean tea hen sin)
      %waft  ::  ~&  [%leap-waft sin]
             abet:lash:(lean tea hen sin)
      %went  ?.  (fear tea)
               ~&  [%went-fear tea]
               [~ +<.^^$]
             abet:lash:(lean tea hen sin)
      %writ  abet:lash:(loam tea hen +.sin)
    ==
  ::
  ++  lear                                              ::    lear:be
    |=  [hen=duct kyz=kiss]                             ::  handle request
    ^-  [p=(list move) q=brat]
    ?-  -.kyz
      %crud  [[[hen [%slip %d %flog kyz]] ~] +<.^^$]
      %hail  [[[hen [%give %helo prot]] ~] +<.^^$]
      %harm  [~ +<.^^$]
      %hook  ~&(%batz-hook [~ +<.^^$])
      %line  =+  gyp=?>(?=(^ fog) i.fog)
             ?:  &(=(0 gyp) =(%$ p.kyz))  $(kyz [%hail ~])
             =<  abet
             ?:  =(0 gyp)
               lash:(gill:(fist hen) p.kyz)
             lash:(como:(fest gyp hen) p.kyz)
      %kill  =+  gyp=?>(?=(^ fog) i.fog)
             ?.  =(0 gyp)
               abet:kill:(fest gyp hen)
             ?:  =(~[/[%$]/term/1] ?>(?=(^ hen) t.hen))   :: XX gross
               [[[hen [%give %logo ~]] ~] +<.^^$]
             [~ +<.^^$]
      %ling  ?>  ?=(^ fog)
             =>  .(fog (weld t.fog `(list ,@ud)`[i.fog ~]))
             [[[hen [%give %helo prot]] ~] +<.^^$]
      %noop  [~ +<.^^$]
      %wart  (lion hen +.kyz)
      ?(%init %limn %make %sith %wake)  !!              ::  handled earlier
    ==
  ::
  ++  leon                                              ::    leon:be
    |=  [hen=duct him=@p cak=@tas sic=path val=*]       ::  default handling
    ^-  [(list move) brat]
    ?+  cak  [~ +<.^^$]
      %hi  (levo hen "< " him val)
      %yu  (levo hen "> " ((hard ,[@p @]) val))
    ==
  ::
  ++  levo                                              ::    levo:be
    |=  [hen=duct pre=tape him=@p val=*]                ::  default message
    ^-  [(list move) brat]
    =+  fom=?:(=(0 val) "remains quietly present" (trip ((hard ,@) val)))
    :_  +<.^^$
    =+  fom=(trip ((hard ,@) val))
    ^-  (list move)
    :~  :-  hen
        :+  %slip  %d
        :+  %flog  %text
        ;:  weld
          pre
          (scow %p him)
          ": "
          (trip ((hard ,@) val))
        ==
    ==
  ::
  ++  loam                                              ::    loam:be
    |=  [tea=wire hen=duct rot=riot]                    ::  file response
    ^+  *fi
    =+(a=(lead tea hen) abet:(gall:q.a p.a rot))
  ::
  ++  lion                                              ::    lion:be
    |=  [hen=duct soc=sock cak=@tas sic=path val=*]       ::  handle request
    ^-  [(list move) brat]
    =+  yes=(~(get by sev) cak)
    ?~  yes  (leon hen q.soc cak sic val)
    =+  sey=(~(tap by u.yes) *(list ,[p=@ud q=@ud r=wire]))
    |-  ^-  [(list move) brat]
    ?~  sey  [~ +<.^^^$]
    =^  von  +<.^^^$
      =<  abet
      =<  lash
      =<  abet
      =<  abet
      %-  pong:(ox:(past:(fest p.i.sey hen) q.i.sey) r.i.sey)
      [%wart soc cak sic val]
    =^  vun  +<.^^^$  $(sey t.sey)
    [(weld von vun) +<.^^^$]
  ::
  ++  prot                                              ::    prot:be
    ^-  [p=path q=prod]                                 ::  get prompt
    ?>  ?=(^ fog)
    ?.  =(0 i.fog)
      perd:(fest i.fog ~)
    :-  /
    :+  %text
      ;:  weld
         (trip (rap 3 [hox '/' cwd ~]))
         ?~(loq "=" (scow u.loq))
         ?~(cws "" (spud cws))
         "> "
      ==
    ~
  ::
  ++  fi                                                ::    fi:be
    |=  [gyp=@ud hen=duct gyr=task]                     ::  process task
    =|  duv=(list move)
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
        (flop `_duv`?:(sam duv [[~ [%give %helo prot]] duv]))
      |=([p=duct q=(mold newt ghat)] [(weld p hen) q])
    ::
    ++  bitt  |=(lap=path [(scot %ud gyp) lap])         ::    bitt:fi:be
    ++  como                                            ::    como:fi:be
      |=  lin=@t                                        ::  command
      ^+  +>
      =+  ryg=~(top to paq.gyr)
      ?~  ryg
        +>.$
      abet:abet:(pong:(ox:(past p.u.ryg) q.u.ryg) [%line lin])
    ++  gill                                            ::    gill:fi:be
      |=  lin=@t                                        ::  input line
      ^+  +>
      =+  zif=((full lark:lo) [1 1] (trip lin))
      ?~  q.zif
        =+  duf=[p=~(rend co ~ %ud p.p.zif) q=~(rend co ~ %ud q.p.zif)]
        (warn "<syntax error at [{p.duf} {q.duf}]>")
      ?~  p.u.q.zif
        +>.$
      (lime u.p.u.q.zif)
    ::                                                  ::    hoop:fi:be
    ++  hoop                                            ::  delete prompt
      |=  [lap=wire ted=@ud]                            ::  XX ugly
      ^+  +>
      %=    +>
          duv  :_(duv `move`[~ %pass ~ %b [%hail ~]])
          paq.gyr
        %-  ~(gas to *(qeu gyro))
        %+  skip
          (~(tap to paq.gyr) *(list gyro))
        |=(a=gyro &(=(ted p.a) =(lap q.a)))
      ==
    ::
    ++  hoot                                            ::    hoot:fi:be
      |=  [lap=wire ted=@ud pod=prod]                   ::  install prompt
      ^+  +>
      %_  +>
        duv      :_(duv [~ %pass ~ %b [%hail ~]])
        paq.gyr  (~(put to paq.gyr) [ted lap pod])
      ==
    ::
    ++  kill
      ^+  .
      =+  pew=(sort (turn (~(tap by q.wip.gyr) ~) |=([p=@ud *] p)) lth)
      |-  ^+  ..kill
      ?~  pew  ..kill
      $(pew t.pew, ..kill abet:goon:(past i.pew))
    ::
    ++  lash                                            ::    lash:fi:be
      ^+  .                                             ::  execute task
      =+  pew=(sort (turn (~(tap by q.wip.gyr) ~) |=([p=@ud *] p)) lth)
      |-  ^+  ..lash
      ?~  pew  ..lash
      $(pew t.pew, ..lash abet:grip:(past i.pew))
    ::
    ++  lime                                            ::    lime:fi:be
      |=  kal=lark                                      ::  start task
      ^+  +>
      (limp q.kal)
    ::
    ++  limp                                            ::    limp:fi:be
      |=  kaw=(list lath)                               ::  start pipeline
      ^+  +>
      ?~  kaw  +>
      $(kaw t.kaw, +>.$ (pant i.kaw ?:(=(~ t.kaw) ~ [~ +(p.wip.gyr)])))
    ::
    ++  pant                                            ::    pant:fi:be
      |=  [lat=lath nex=(unit ,@ud)]                    ::  start thread
      %=  +>
          p.wip.gyr  +(p.wip.gyr)
          q.wip.gyr
        (~(put by q.wip.gyr) p.wip.gyr [nex ~ [~ ~ ~ %n ~ *claw lat]])
      ==
    ::
    ++  past                                            ::    past:fi:be
      |=  ted=@ud                                       ::  select thread
      ^+  ra
      =+  bek=(need (~(get by q.wip.gyr) ted))
      ~(. ra ted p.bek q.bek r.bek)
    ::
    ++  perd                                            ::    perd:fi:be
      ^-  [p=path q=prod]                               ::  produce prompt
      =+  top=~(top to paq.gyr)
      ?~(top [/ %none "[waiting...]" ~] [q.u.top r.u.top])
    ::
    ++  warn                                            ::    warn:fi:be
      |=  txt=tape                                      ::  send warning
      ^+  +>
      +>(duv :_(duv [~ [%give %warn txt]]))
    ::
    ++  ra                                              ::    ra:fi:be
      |_  $:  ted=@ud                                   ::  thread id
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
          ..ra(q.wip.gyr (~(del by q.wip.gyr) ted))
        ..ra(q.wip.gyr (~(put by q.wip.gyr) ted nex loz orb))
      ::
      ++  bist  |=(lap=path (bitt (scot %ud ted) lap))  ::  form path
      ++  bust                                          ::  slice coal
        |=  [axe=axis vux=coal]
        ^-  coal
        =<  q
        %+  slam  sot.vax
        (slop [[%atom %$] axe] [vas.typ vux])
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
        ?>  ?=([~ %$ %ud @] dup)
        =+  kit=(need (~(get by p.orb) q.p.u.dup))
        ?~  rot
          %_(+>.$ ..ra (warn (spud (meat kit))), s.orb [%r ~])
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
          [~ good:+>.$(sac t.kas)]
        [[~ q.u.lez] +>.$]
      ::
      ++  glib                                          ::  pending note
        |=  [lap=wire nob=nose]
        ^+  +>
        %_(+> r.orb (~(put to r.orb) [lap nob]))
      ::
      ++  glob                                          ::  extern
        |=  [lap=wire sik=?(sign kiss)]
        ^+  +>
        %_(+> q.orb (~(put to q.orb) [lap sik]))
      ::
      ++  glum                                          ::  blocked thread
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
        (weld q.sik `path`[?~(p.sik wen u.p.sik) r.sik])
      ::
      ++  gaur                                          ::  print skit
        |=  sik=skit
        ^+  +>
        %+  gram  ~
        :-  %give
        :+  %note  '^'
        :-  %leaf
        ;:  weld
          (spud q.sik)
          ?~(p.sik "=" (spud u.p.sik ~))
          (spud r.sik)
          "/hoon"
        ==
      ::
      ++  good                                          ::  print skits
        =+  kas=sac
        |-  ^+  +>
        ?~(kas +> (gaur:$(kas t.kas) i.kas))
      ::
      ++  goon                                          ::  kill
        ^+  .
        .(s.orb [%r ~])
      ::
      ++  grab                                          ::  chase simple path
        |=  lam=lamb  ^-  twig
        ?-  -.lam
          &  =+  tes=(sky [%cx hox %main wen %bin p.lam %hoon ~])
             (grad [hox ?^(tes %main cwd) wen %bin p.lam ~])
          |  p.lam
        ==
      ::
      ++  grad                                          ::  path to twig
        |=  pax=path  ^-  twig
        [%clsg (turn pax |=(a=@ta [%dtzy %ta a]))]
      ::
      ++  gram                                          ::  add action
        |=  mov=move
        %_(+> duv [mov duv])
      ::
      ++  gran                                          ::  add actions
        |=  vid=(list move)
        ^+  +>
        ?~(vid +> $(vid t.vid, +> (gram i.vid)))
      ::
      ++  gray                                          ::  process result
        |=  ton=toon
        ^-  [(unit) _+>]
        ?-  -.ton
          %0  [[~ p.ton] +>]
          %1  [~ (glum ((list path) p.ton) s.orb)]
          %2  [~ (gram(orb [~ ~ ~ %r ~]) ~ [%give %crud %exit p.ton])]
        ==
      ::
      ++  grid                                          ::  process result
        |=  [ton=toon fun=_|+(* +>)]
        ^+  +>
        ?-  -.ton
          %0  (fun p.ton)
          %1  (glum ((list path) p.ton) s.orb)
          %2  (gram(orb [~ ~ ~ %r ~]) ~ [%give %crud %exit p.ton])
        ==
      ::
      ++  grin                                          ::  process result
        |=  [ton=toon hog=boar]
        ^+  +>
        ?-  -.ton
          %0  (haul (fret p.ton))
          %1  (glum ((list path) p.ton) hog)
          %2  (gram(orb [~ ~ ~ %r ~]) ~ [%give %crud %exit p.ton])
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
          =+  gen=?:(?=(0 -.r.hog) s.r.hog p.r.hog)
          ?~  joy.q.hog
            =^  juy  +.$  glee
            ?~  juy  +.$
            $(s.orb hog(joy.q [~ (need (mang [food:zu war u.juy] sky))]))
          ?~  ran.q.hog
            =^  nur  +.$  (gray (mong [slap u.joy.q.hog gen] sky))
            ?~  nur  +.$
            $(s.orb hog(ran.q nur))
          ?:  ?=(1 -.r.hog)
             =.  +.$  (gybe ~ -.u.ran.q.hog +.u.ran.q.hog ~)
             $(s.orb [%r ~])
          ?:  ?=(2 -.r.hog)
            $(s.orb [%t u.ran.q.hog])
          ?~  pux.q.hog
            =^  wim  +.$
              (gray (mong [slap u.joy.q.hog (grab q.r.hog)] sky))
            ?~  wim  +.$
            $(s.orb hog(pux.q [~ ((hard path) +.u.wim)]))
          =+  ^=  mop  |-  ^-  (list ,@tas)
                       ?~  r.r.hog  ~
                       =+  mor=$(r.r.hog t.r.r.hog)
                       ?.(?=(| -.i.r.r.hog) mor (weld p.i.r.r.hog mor))
          ?~  jiv.q.hog
            =^  woh  +.$  (grow mop u.pux.q.hog)
            ?~  woh  +.$
            $(s.orb hog(jiv.q woh))
          ?~  kyq.q.hog
            =^  mux  +.$
              (gray (mong [fuel:zu r.r.hog u.jiv.q.hog] sky))
            ?~  mux  +.$
            $(s.orb hog(kyq.q mux))
          ?~  gam.q.hog
            =^  lez  +.$  (grok | u.pux.q.hog u.kyq.q.hog)
            ?~  lez  +.$
            $(s.orb hog(gam.q [~ q.u.lez]))
          %-  grin  :_  hog
          %-  mong  :_  sky
          [fapp:zu u.gam.q.hog u.ran.q.hog]
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
          |=  noy=tutu
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
        =+  sho=(scag 4 pax)
        =+  boy=(grub pax)
        |-  ^-  [(unit coal) _+>.^$]
        =^  hyq  +>.^$  (grim paw boy)
        ?~  hyq  [~ +>.^$]
        ?~  sho  [[~ q.u.hyq] +>.^$]
        %=  $
          sho  t.sho
          boy  q.u.hyq
          paw  (turn paw |=(a=path (weld a `path`[i.sho ~])))
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
        (gram ~ %pass tea %c [%warp [who him] ryf])
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
        |=  [ton=? cav=@ta gyp=@ud ted=@ud lap=wire]
        ^+  +>
        =+  ^=  yes  ^-  (set ,[p=@ud q=@ud r=wire])
            =+  yes=(~(get by sev) cav)
            ?~(yes ~ u.yes)
        %_    +>.$
            sev
          %+  ~(put by sev)  cav
          ?:  ton
            (~(put in yes) gyp ted lap)
          (~(del in yes) gyp ted lap)
        ==
      ::
      ++  gush
        |=  [wak=@da gyp=@ud ted=@ud lap=wire]
        ^+  +>
        +>.$(tem (~(put by tem) [gyp ted lap] wak))
      ::
      ++  gust
        |=  [gyp=@ud ted=@ud lap=wire]
        +>.$(tem (~(del by tem) [gyp ted lap]))
      ::
      ++  gybe                                          ::  pipe forward
        |=  pun=(unit ,[p=tutu q=(list)])
        ^+  +>
        ?~  nex
          ?~  pun  +>
          (gran (turn q.u.pun |=(a=* [~ %give (gyve p.u.pun a)])))
        +>.$(..ra abet:(glob:(past u.nex) ~ [%pipe pun]))
      ::
      ++  gyve                                          ::  print vase
        |=  [toy=tutu val=*]  ^-  ghat
        =+  caf=((hard calf) (need (mang [felt:zu toy] sky)))
        ::  ?:  =([~ [%atom %t]] caf)
        ::  [%tell ((hard ,@t) val) ~]
        ::  ?:  =([~ %wall] caf)
        ::  [%tell ((hard (list ,@t)) val)]
        [%talk (dish:ut caf val)]
      ::
      ++  haft                                          ::  process gift
        |=  guf=gilt
        ^+  +>
        ?-    -.guf
            %$   (gybe ~ +<.guf +>.guf ~)
            %mu  (gybe ~ +<.guf ((hard (list)) +>.guf))
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
            %$   !!
            %cc  ?~  p.gud  +>.^$(loq p.gud)
                 ~&  [%cc p.gud]
                 =+  hyz=(sky %cy gasp(loq p.gud))
                 ?~  hyz  ~|(%case-none !!)
                 +>.^$(loq p.gud)
            %ck  +>.^$(cwd p.gud)
            %cs  +>.^$(cws p.gud)
            %de  (gram ~ %give %note '#' q.gud)
            %ex  =.  +>.^$  guff
                 +>.^$(s.orb [%n p.gud *claw q.gud])
            %ha  (gram ~ %give %crud %soft [p.gud ~])
            %ho  (gram ~ %give %crud %soft p.gud)
            %la  (gram ~ %give %talk p.gud)
            %lo  (gran (turn p.gud |=(a=tank [~ %give %talk a])))
            %mu  !!
            %mx  |-  ^+  +>.^^$
                 ?~  p.gud  +>.^^$
                 $(p.gud t.p.gud, +>.^^$ ^$(gud i.p.gud))
            %ok  (gram ~ %pass ~ %c %info who p.gud q.gud)
            %sc  good:+>.^$(sac ?~(p.gud ?~(sac ~ +.sac) [u.p.gud sac]))
            %sp  !!
            %sq  =+  tea=(bist %ma r.gud)
                 %+  gram  ~
                 [%pass tea %a [%want [who p.gud] [%q q.gud %b tea] s.gud]]
            %sr  (gram ~ %pass ~ %a [%want [who p.gud] [%r q.gud] r.gud])
            %te  (gram ~ %give %tell p.gud)
            %th  (gram ~ %pass ~ %e %that p.gud q.gud)
            %tq  =+  tea=(bist %ma p.gud)
                 (gram ~ %pass tea %e [%them ~ q.gud])
            %va  !!
            %xx  =+  gah=((soft ghat) p.gud)
                 ?~  gah
                   ~&  [%batz-xx (,@tas -.p.gud)]
                   !!
                 (gram ~ %give u.gah)
            %xy  ?.  ?=([@ ~] p.gud) 
                   ~&  [%batz-xyz p.gud]
                   !!
                 =+  hug=((soft newt) [i.p.gud q.gud])
                 ?~  hug
                   ~&  [%batz-xy (,@tas -.q.gud)]
                   !!
                 (gram ~ %pass ~ u.hug)
            %xz  =+  tea=(bist %ma /chat/hi/hey)
                 (gram ~ %pass tea %g %mess p.gud q.gud r.gud !>(s.gud))
            %zz  =+  tea=(bist %ma q.gud)
                 ?.  ?=([@ ~] p.gud) 
                   ~&  [%batz-zzz p.gud]
                   !!
                 =+  hug=((soft newt) [i.p.gud r.gud])
                 ?~  hug
                   ~&  [%batz-zz (,@tas -.r.gud)]
                   !!
                 (gram ~ %pass tea u.hug)
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
          %$   +>
          %do  +>
          %eg  (gulf (bist %ma lap) p.gal)
          %es  ::  ~&  %es-loss
               (gull (bist %ma lap) p.gal q.gal ~)
          %gr  +>
          %hp  +>
          %ht  (gram ~ %pass (bist [%ma lap]) %e [%band who ~])
          %lq  (gump | p.gal gyp ted lap)
          %ow  +>
          %rt  +>
          %up  +>(..ra (hoop lap ted))
          %wa  (gust gyp ted lap)
        ==
      ::
      ++  moor                                          ::  start goal
        |=  [lap=wire gal=goal]
        ^+  +>
        ?-    -.gal
          %$   +>
          %do  !!
          %eg  (gulp (bist %ma lap) p.gal)
          %es  ::  ~&  %es-moor
               (gull (bist %ma lap) p.gal q.gal [~ r.gal])
          %gr  +>
          %hp  +>
          %ht  (gram ~ %pass [%b (bist [%ma lap])] %e [%band who p.gal])
          %lq  (gump & p.gal [gyp ted lap])
          %ow  +>
          %rt  +>
          %up  +>(..ra (hoot lap ted p.gal))
          %wa  (gush p.gal gyp ted lap)
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
        ++  pong                                        ::  accept card
          |=  sik=?(sign kiss)
          ^+  +>
          ?>  ?=(^ lug)
          ?-    -.u.lug
              ~
            ?>  ?=(%pipe -.sik)
            +>.$(+>.$ (glib lap [%$ p.sik]))
          ::
              %do  !!
          ::
              %eg
            ?>  ?=(%writ -.sik)
            +>.$(lug ~, +>.$ (glib lap [%eg +.sik]))
          ::
              %es
            ?>  ?=(%writ -.sik)
            =+  ^=  goh  ^-  (unit goal)
                ?~  p.sik  `(unit goal)`~
                ?-  -.r.u.lug
                    %&  ~
                    %|
                  ^-  (unit goal)
                  :-  ~
                  ?>  ?=(%ud -.q.p.u.p.sik)
                  %=    u.lug
                      p.p.r
                    ?>  ?|  !=(%ud -.p.p.r.u.lug)
                            =(p.p.p.r.u.lug p.q.p.u.p.sik)
                        ==
                    [%ud +(p.q.p.u.p.sik)]
                  ==
                ==
            =.  loz  ?~(goh (~(del by loz) lap) (~(put by loz) lap u.goh))
            %=  +>.$
              lug   goh
              gul   goh
              +>.$  (glib lap [%eg +.sik])
            ==
          ::
              %gr
            ?>  ?=(?(%rush %rust) -.sik)
            +>.$(+>.$ (glib lap [%gr +.sik]))
          ::
              %hp
            ?>  ?=(%thou -.sik)
            +>.$(+>.$ (glib lap [%hp +.sik]))
          ::
              %ht  !!
          ::  ?>  ?=(%thee -.sik)
          ::  +>.$(+>.$ (glib lap [%ht +.sik]))
          ::
              %lq
            ?>  ?=(%wart -.sik)
            +>.$(+>.$ (glib lap [%lq q.p.sik r.sik s.sik]))
          ::
              %rt
            ?:  ?=(%went -.sik)
              ?.  ?=(%dead q.sik)  +>.$
              +>.$(+>.$ (glib lap [%rt ~]))
            ?>  ?=(%waft -.sik)
            +>.$(+>.$ (glib lap [%rt ~ q.sik]))
          ::
              %up
            ?>  ?=(%line -.sik)
            +>.$(+>.$ (glib lap [%up +.sik]))
          ::
              %ow
            ?>  ?=(%went -.sik)
            +>.$(+>.$ (glib lap [%ow q.sik]))
          ::
              %wa
            ?>  ?=(%wake -.sik)
            +>.$(+>.$ (glib lap [%wa ~]))
          ==
        --
      --
    --
  ::
  ++  lo                                                ::  command parsers
    |%
    ++  coax                                            ::  parse flags
      |=  coo=tape  ^-  twig
      :+  %cnts  [[~ 1] ~]
      |-  ^-  tram
      ?~  coo  ~
      :_  $(coo t.coo)
      ?:  &((gte i.coo 'a') (lte i.coo 'z'))
        [[i.coo ~] [%dtzy %f &]]
      ?>  &((gte i.coo 'A') (lte i.coo 'Z'))
      [[(sub i.coo 32) ~] [%dtzy %f |]]
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
                  (stag %& (stag %cnts (stag [[~ 1] ~] lobo:vez)))
                  (easy ~)
                ==
              ==
              (easy ~)
            ==
          ==
        ::
          ;~  plug
            (stag %& (stag %cnts (stag [[~ 1] ~] lobo:vez)))
            (easy ~)
          ==
        ==
      ==
    ::
    ++  lark                                            ::  parse lark
      %+  cook  |=(a=(unit ^lark) a)
      ;~  pose
        (cold ~ ;~(plug col col (star (shim 32 126))))
        %+  ifix  [(star ace) (star ace)]
        %+  stag  ~
        %+  stag  ~
        ;~  pose
          lute
        ::
          ;~  pfix  tis
            ;~  pose
              %+  cook
                |=  [a=@tas b=(list twig)]
                ^-  (list lath)
                :~  [%1 [%cltr b]]
                    [%0 %0 [%& %set] ~ [[%clsg [%dtzy %tas a] ~]]]
                ==
              ;~(plug sym (star ;~(pfix ace wide:vez)))
            ::
              %+  cook
                |=  a=@tas
                :~  [%0 %0 [%& %none] ~ [%bczp %null]]
                    [%0 %0 [%& %set] ~ [%clsg [%dtzy %tas a] ~]]
                ==
              ;~(pfix tis sym)
            ==
          ==
        ==
        gay
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
        ;~(pose (stag %2 wide:vez) loth)
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
        [[%atom %$] (shax :(mix eny now (shax p.god)))]
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
    ++  fuss                                            ::  twig and hash
      |=  [sot=? pax=path]
      ^-  (unit ,[p=@uvI q=twig])
      =+  haw=(weld pax `path`/hoon)
      =+  arc=((hard arch) .^(%cy haw))
      ?:  &(sot ?=(~ q.arc))  ~
      =+  mot=((hard ,@) .^(%cx haw))
      :+  ~  ?~(q.arc (sham mot) u.q.arc)
      ~|  `path`haw
      (scan (trip mot) (ifix [gay gay] tall:vez(wer pax)))
    --
  --
--
.  ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::              section 4bC, shell vane                 ::
::
=|  $:  %0
        big=(unit ,@p)                                  ::  major ship
        dez=(map duct brim)                             ::  state by ship
    ==                                                  ::
|=  [now=@da eny=@ ski=sled]                            ::  current invocation
^?                                                      ::  opaque core
|%                                                      ::  poke/peek pattern
++  call                                                ::  handle request
  |=  $:  hen=duct
          hic=(hypo (hobo kiss))
      ==
  =>  %=    .                                           ::  XX temporary
          q.hic
        ^-  kiss
        ?:  ?=(%soft -.q.hic)
          ::  ~&  [%dill-call-soft (,@tas `*`-.p.q.hic)]
          ((hard kiss) p.q.hic)
        ?:  (~(nest ut -:!>(*kiss)) | p.hic)  q.hic
        ~&  [%batz-call-flub (,@tas `*`-.q.hic)]
        ((hard kiss) q.hic)
      ==
  ^-  [p=(list move) q=_..^$]
  =+  ska=(slod ski)
  =+  sky=|=(* `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  ?:  ?=([%crud *] q.hic)
    [[[hen [%slip %d %flog q.hic]] ~] ..^$]
  ?:  ?=(%wake -.q.hic)
    =+  ^=  fiy
        =|  fiy=(list ,[p=duct q=ship r=[p=@ud q=@ud r=wire]])
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
          [[p.n.dez p.i.q.n.dez p.n.tem.q.i.q.n.dez] fiy]
        ==
    =|  wam=(list move)
    |-  ^-  [p=(list move) q=_..^^$]
    ?~  fiy  [wam ..^^$]
    =+  dos=(need (~(get by dez) p.i.fiy))
    =+  suy=|-(`brad`?~(dos !! ?.(=(q.i.fiy p.i.dos) $(dos t.dos) q.i.dos)))
    =+  geb=((be [[q.i.fiy bred] suy]) now eny sky)
    =+  ^=  yub  ^-  [p=(list move) q=brat]
        ?.  (fang:geb r.i.fiy)
          ~&  [%time-lost r.i.fiy]
          [~ abet:(flub:geb r.i.fiy)]
        (lake:geb p.i.fiy r.i.fiy)
    %=  $
      fiy      t.fiy
      wam      (weld p.yub wam)
      dez.^^$  (~(put by dez.^^$) p.i.fiy [[q.i.fiy +.q.yub] +.dos])
    ==
  |-  ^-  [p=(list move) q=_..^^$]
  =+  dus=(~(get by dez) hen)
  ?~  dus
    ?+    -.q.hic
      ~&  [%take-none -.q.hic ~ hen]
      ~|([%take-none -.q.hic] !!)
    ::
        %hail
      ?~  big
        ~&  [%call-hail-soon hen]
        [~ ..^^$]
      $(dez (~(put by dez) hen [[u.big (bard u.big)] ~]))
    ::
        %init
      ::  ~&  [%take-init p.q.hic hen]
      =.  big  ?~  big
                 `p.q.hic
               `(min p.q.hic u.big)
      =+  bos=(sein p.q.hic)
      :-  :-  [hen [%give q.hic]]
          ?:  =(bos p.q.hic)  ~
          :_  ~
          [hen [%slip %b %line (rap 3 ":{(scow %p bos)}/main=/bin/update")]]
      ..^^$(dez (~(put by dez) hen [[p.q.hic (bard p.q.hic)] ~]))
    ::
        ?(%make %sith)
      [[[hen [%pass ~ %a q.hic]] ~] ..^^$]
    ==
  ?>  ?=(^ u.dus)
  =+  beg=`brat`[[p.i.u.dus bred] q.i.u.dus]
  =+  yub=(lear:((be beg) now eny sky) hen q.hic)
  :-  p.yub
  ..^^$(dez (~(put by dez) hen [[p.i.u.dus +.q.yub] t.u.dus]))
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
++  load
  |=  old=[%0 big=(unit ,@p) dez=(map duct brim)]
  ^+  ..^$
  ..^$(big big.old, dez dez.old)
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas his=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair logo ,*)))
  ~
::
++  stay  [%0 big dez]
++  take                                                ::  accept response
  |=  [tea=wire hen=duct hin=(hypo sigh)]
  ^-  [p=(list move) q=_..^$]
  ::  ~&  [%batz-take -.p.q.hin [%tea tea] [%hen hen]]
  =+  ska=(slod ski)
  =+  sky=|=(* `(unit)`=+(a=(ska +<) ?~(a ~ ?~(u.a ~ [~ u.u.a]))))
  ?:  ?=([%crud *] p.q.hin)
    [[[hen [%slip %d %flog p.q.hin]] ~] ..^$]
  =+  dus=(~(get by dez) hen)
  ?~  dus
    ?+    -.p.q.hin
      ~&([%take-none -.p.q.hin] !!)
    ::
        %hail
      ?~  big
        ~&  [%take-hail-soon hen]
        [~ ..^$]
      ::  ~&  [%batz-take-console hen]
      $(dez (~(put by dez) hen [[u.big (bard u.big)] ~]))
    ::
        %init
      ::  ~&  [%take-init p.p.q.hin hen]
      =.  big  ?~  big
                 `p.p.q.hin
               `(min p.p.q.hin u.big)
      =+  bos=(sein p.p.q.hin)
      :-  :-  [hen [%give p.q.hin]]
          ?:  =(bos p.p.q.hin)  ~
          :_  ~
          [hen [%slip %b %line (rap 3 ":{(scow %p bos)}/main=/bin/update")]]
      ..^$(dez (~(put by dez) hen [[p.p.q.hin (bard p.p.q.hin)] ~]))
    ==
  ?>  ?=(^ u.dus)
  ?:  ?=(%init -.p.q.hin)
    =+  bos=(sein p.p.q.hin)
    :-  :*  [hen %give p.q.hin]
            [[[%b ~] hen] [%sick %hail ~]]
            ?:  =(bos p.p.q.hin)  ~
            :_  ~
            [[/b hen] [%sick %line (rap 3 ":{(scow %p bos)}/main=/bin/update")]]
        ==
    ..^$(dez (~(put by dez) hen [[p.p.q.hin (bard p.p.q.hin)] u.dus]))
  =+  beg=`brat`[[p.i.u.dus bred] q.i.u.dus]
  =+  yub=(leap:((be beg) now eny sky) tea hen p.q.hin)
  :-  p.yub
  ..^$(dez (~(put by dez) hen [[p.i.u.dus +.q.yub] t.u.dus]))
--
