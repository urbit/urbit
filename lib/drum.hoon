::                                                      ::  ::
::::  /hoon/drum/lib                                    ::  ::
  ::                                                    ::  ::
/?    310                                               ::  version
/-    sole
/+    sole
[. ^sole]
!:                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%                                                      ::  ::
++  drum-part  {$drum $0 drum-pith}                     ::
++  drum-pith                                           ::
  $:  eel+(set gill)                                    ::  connect to 
      ray+(set well)                                    ::  
      fur+(map dude (unit server))                      ::  servers
      bin+(map bone source)                             ::  terminals
  ==                                                    ::
++  drum-start  well                                    ::  start (local) server
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
++  server                                              ::  running server
  $:  syd+desk                                          ::  app identity
      cas+case                                          ::  boot case
  ==                                                    ::
++  source                                              ::  input device
  $:  edg+$_(80)                                        ::  terminal columns
      off+@ud                                           ::  window offset
      kil+(unit (list @c))                              ::  kill buffer
      inx+@ud                                           ::  ring index
      fug+(map gill (unit target))                      ::  connections
      mir+(pair @ud (list @c))                          ::  mirrored terminal
  ==                                                    ::
++  master                                              ::  master buffer
  $:  liv+?                                             ::  master is live
      tar+target                                        ::  master target
  ==                                                    ::
++  history                                             ::  past input
  $:  pos+@ud                                           ::  input position
      num+@ud                                           ::  number of entries
      lay+(map @ud (list @c))                           ::  editing overlay
      old+(list (list @c))                              ::  entries proper
  ==                                                    ::
++  search                                              ::  reverse-i-search
  $:  pos+@ud                                           ::  search position
      str+(list @c)                                     ::  search string
  ==                                                    ::
++  target                                              ::  application target
  $:  ris+(unit search)                                 ::  reverse-i-search
      hit+history                                       ::  all past input
      pom+sole-prompt                                   ::  static prompt
      inp+sole-command                                  ::  input state
  ==                                                    ::
++  ukase                                               ::  master command
  $%  {$add p+(list gill)}                              ::  attach to
      {$del p+(list gill)}                              ::  detach from
      {$new p+(list well)}                              ::  create
  ==                                                    ::
--
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  deft-apes                                           ::  default servers
  |=  our+ship
  %-  ~(gas in *(set well))
  =+  myr=(clan our)
  ?:  =(%pawn myr)
    [[%base %talk] [%base %dojo] ~]
  ?:  =(%earl myr)
    [[%home %dojo] ~]
  [[%home %talk] [%home %dojo] ~]
::
++  deft-fish                                           ::  default connects
  |=  our+ship
  %-  ~(gas in *(set gill))
  ^-  (list gill)
  =+  myr=(clan our)
  ?:  =(%earl myr)
    [[(sein our) %talk] [our %dojo] ~]
  [[our %talk] [our %dojo] ~]
::
++  deft-mast                                           ::  default master
  |=  our+ship
  ^-  master
  :*  %&
      *(unit search)
      *history
      [%& %sole "{(scow %p our)}# "]
      *sole-command
  ==
::
++  deft-pipe                                           ::  default source
  |=  our+ship                                          ::
  ^-  source                                            ::  
  :*  80                                                ::  edg
      0                                                 ::  off
      ~                                                 ::  kil
      0                                                 ::  inx
      ~                                                 ::  fug
      [0 ~]                                             ::  mir
  ==
::
++  deft-tart  *target                                  ::  default target
++  drum-port                                           ::  initial part
  |=  our+ship
  ^-  drum-part 
  :*  %drum
      %0
      (deft-fish our)                                   ::  eel
      (deft-apes our)                                   ::  ray
      ~                                                 ::  fur
      ~                                                 ::  bin
  ==                                                    ::
::
++  drum-path                                           ::  encode path
  |=  gyl+gill
  [%drum %phat (scot %p p.gyl) q.gyl ~]
::
++  drum-phat                                           ::  decode path
  |=  way+wire  ^-  gill
  ?>(?=({@ @ $~} way) [(slav %p i.way) i.t.way])
--
!:
::::
  ::
|=  {bowl drum-part}                                  ::  main drum work
=+  (fall (~(get by bin) ost) (deft-pipe our))
=*  dev  -
=>  |%                                                ::  arvo structures
    ++  pear                                          ::  request
      $%  {$sole-action p+sole-action}                ::
          {$talk-command command:talk}                ::
      ==                                              ::
    ++  lime                                          ::  update
      $%  {$dill-blit dill-blit}                      ::
      ==                                              ::
    ++  card                                          ::  general card
      $%  {$conf wire dock $load ship term}           ::
          {$diff lime}                                ::
          {$peer wire dock path}                      ::
          {$poke wire dock pear}                      ::
          {$pull wire dock $~}                        ::
      ==                                              ::
    ++  move  (pair bone card)                        ::  user-level move
    --
|_  {moz+(list move) biz+(list dill-blit)}
++  diff-sole-effect-phat                             ::
  |=  {way+wire fec+sole-effect}  
  =<  se-abet  =<  se-view
  =+  gyl=(drum-phat way)
  ?:  (se-aint gyl)  +>.$
  (se-diff gyl fec)
::
++  peer                                              ::
  |=   pax+path  =<  se-abet
  ^+  +>
  ?.  ?|  =(our src)                                  ::  ourself
          &(=(%duke (clan our)) =(our (sein src)))    ::  or our own yacht
      ==                                              ::
    ~|  [%drum-unauthorized our/our src/src]          ::  very simplistic
    !!
  se-view:(se-text "[{<src>}, driving {<our>}]")
::
++  poke-dill-belt                                    ::
  |=  bet+dill-belt   
  =<  se-abet  =<  se-view
  (se-belt bet)
::
++  poke-start                                        ::
  |=  wel+well  
  =<  se-abet  =<  se-view
  (se-born wel)
::
++  poke-link                                         ::
  |=  gyl+gill
  =<  se-abet  =<  se-view
  (se-link gyl)
::
++  poke-unlink                                       ::
  |=  gyl+gill
  =<  se-abet  =<  se-view
  (se-klin gyl)
::
:: ++  poke-exit                                         ::
::   |=(~ se-abet:(se-blit `dill-blit`[%qit ~]))  ::  XX find bone
:: ::
++  reap-phat                                         ::
  |=  {way+wire saw+(unit tang)}  
  =<  se-abet  =<  se-view
  =+  gyl=(drum-phat way)
  ?~  saw
    (se-join gyl)
  (se-dump:(se-drop & gyl) u.saw)
::
++  take-coup-phat                                    ::
  |=  {way+wire saw+(unit tang)}  
  =<  se-abet  =<  se-view
  ?~  saw  +>
  =+  gyl=(drum-phat way)
  ?:  (se-aint gyl)  +>.$
  =.  u.saw  :_(u.saw >[%drum-coup-fail src ost gyl]<)
  (se-dump:(se-drop & gyl) u.saw)
::
++  take-onto                                         ::
  |=  {way+wire saw+(each suss tang)}  
  =<  se-abet  =<  se-view
  ?>  ?=({@ @ $~} way)
  ?>  (~(has by fur) i.t.way)
  =+  wel=`well`[i.way i.t.way]
  ?-  -.saw
    {$|}  (se-dump p.saw)
    {$&}  ?>  =(q.wel p.p.saw)
          ::  =.  +>.$  (se-text "live {<p.saw>}")
          +>.$(fur (~(put by fur) q.wel `[p.wel %da r.p.saw]))  
  == 
::
++  quit-phat                                         ::
  |=  way+wire  
  =<  se-abet  =<  se-view
  =+  gyl=(drum-phat way)
  ~&  [%drum-quit src ost gyl]
  (se-drop %| gyl)
::                                                    ::  ::
::::                                                  ::  ::
  ::                                                  ::  ::
++  se-abet                                           ::  resolve
  ^-  (quip move *drum-part)
  ?.  se-ably
    =.  .  se-adit
    [(flop moz) +>+>+<+]
  =.  .  se-subze:se-adze:se-adit
  :_  %_(+>+>+<+ bin (~(put by bin) ost `source`+>+<))
  ^-  (list move)
  %+  welp  (flop moz) 
  ^-  (list move)
  ?~  biz  ~
  [ost %diff %dill-blit ?~(t.biz i.biz [%mor (flop biz)])]~
::
++  se-ably  (~(has by sup) ost)                      ::  caused by console
++  se-adit                                           ::  update servers
  ^+  .
  %+  roll  (~(tap in ray))
  =<  .(con +>)
  |=  {wel+well con+$_(..se-adit)}  ^+  con
  =.  +>.$  con
  =+  hig=(~(get by fur) q.wel) 
  ?:  &(?=(^ hig) |(?=($~ u.hig) =(p.wel syd.u.u.hig)))  +>.$
  =.  +>.$  (se-text "activated app {(trip p.wel)}/{(trip q.wel)}")
  %-  se-emit(fur (~(put by fur) q.wel ~))
  [ost %conf [%drum p.wel q.wel ~] [our q.wel] %load our p.wel]
::
++  se-adze                                           ::  update connections
  ^+  .
  %+  roll  (~(tap in eel))
  =<  .(con +>)
  |=  {gil+gill con+$_(.)}  ^+  con
  =.  +>.$  con
  ?:  (~(has by fug) gil)
    +>.$
  (se-peer gil)
::
++  se-subze                                          ::  downdate connections
  =<  .(dev (~(got by bin) ost))
  =.  bin  (~(put by bin) ost dev)
  ^+  .
  %-  ~(rep by bin)
  =<  .(con +>)
  |=  {{ost+bone dev+source} con+$_(.)}  ^+  con
  =+  xeno=se-subze-local:%_(con ost ost, dev dev)
  xeno(ost ost.con, dev dev.con, bin (~(put by bin) ost dev.xeno))
::
++  se-subze-local
  ^+  .
  %-  ~(rep by fug)
  =<  .(con +>)
  |=  {{gil+gill *} con+$_(.)}  ^+  con
  =.  +>.$  con
  ?:  (~(has in eel) gil)
    +>.$
  (se-nuke gil)
::
++  se-aint                                           ::  ignore result
  |=  gyl+gill
  ^-  ?
  ?.  (~(has by bin) ost)  &
  =+  gyr=(~(get by fug) gyl)
  |(?=($~ gyr) ?=({$~ $~} gyr))
::
++  se-alas                                           ::  recalculate index
  |=  gyl+gill
  ^+  +>
  =+  [xin=0 wag=se-amor]
  ?:  =(~ wag)  +>.$(inx 0)
  |-  ^+  +>.^$
  ?~  wag  +>.^$(inx 0)
  ?:  =(i.wag gyl)  +>.^$(inx xin)
  $(wag t.wag, xin +(xin))
::
++  se-amor                                           ::  live targets
  ^-  (list gill)
  (skim (~(tap in eel)) |=(gill ?=({$~ $~ *} (~(get by fug) +<))))
::
++  se-anon                                           ::  rotate index
  =+  wag=se-amor
  ?~  wag  +
  ::  ~&  [%se-anon inx/inx wag/wag nex/(mod +(inx) (lent se-amor))]
  +(inx (mod +(inx) (lent se-amor)))
::
++  se-agon                                           ::  current gill
  ^-  (unit gill)
  =+  wag=se-amor
  ?~  wag  ~
  `(snag inx se-amor)
::
++  se-belt                                           ::  handle input
  |=  bet+dill-belt
  ^+  +>
  ?:  ?=($rez -.bet)
    +>(edg (dec p.bet))
  ?:  ?=($yow -.bet)
    ~&  [%no-yow -.bet]
    +>
  =+  gul=se-agon
  =+  tur=`(unit (unit target))`?~(gul ~ (~(get by fug) u.gul))
  ?:  |(=(~ gul) =(~ tur) =([~ ~] tur))  (se-blit %bel ~)
  =+  taz=~(. ta [& (need gul)] `target`(need (need tur)))
  =<  ta-abet
  ?-  -.bet
    $aro  (ta-aro:taz p.bet)
    $bac  ta-bac:taz
    $cru  (ta-cru:taz p.bet q.bet)
    $ctl  (ta-ctl:taz p.bet)
    $del  ta-del:taz
    $hey  taz(mir [0 ~])
    $met  (ta-met:taz p.bet)
    $ret  ta-ret:taz
    $txt  (ta-txt:taz p.bet)
  ==
::
++  se-born                                           ::  new server
  |=  wel+well
  ^+  +>
  ?:  (~(has in ray) wel)
    (se-text "[already running {<p.wel>}/{<q.wel>}]")
  +>(ray (~(put in ray) wel), eel (~(put in eel) [our q.wel]))
::
++  se-drop                                           ::  disconnect
  |=  {pej+? gyl+gill}
  ^+  +>
  =+  lag=se-agon
  ?.  (~(has by fug) gyl)  +>.$
  =.  fug  (~(del by fug) gyl)
  =.  eel  ?.(pej eel (~(del in eel) gyl))
  =.  +>.$  ?.  &(?=(^ lag) !=(gyl u.lag))  
              +>.$(inx 0)
            (se-alas u.lag)
  =.  +>.$  (se-text "[unlinked from {<gyl>}]")
  ?:  =(gyl [our %dojo])                              ::  undead dojo
    (se-link gyl)
  +>.$
::
++  se-dump                                           ::  print tanks
  |=  tac+(list tank)
  ^+  +>
  ?.  se-ably  (se-talk tac)
  =+  wol=`wall`(zing (turn (flop tac) |=(a+tank (~(win re a) [0 edg]))))
  |-  ^+  +>.^$
  ?~  wol  +>.^$
  $(wol t.wol, +>.^$ (se-blit %out (tuba i.wol)))
::
++  se-joke                                           ::  prepare connection
  |=  gyl+gill
  ^+  +>
  =+  lag=se-agon
  ?~  lag  +>.$
  ?:  =(~ fug)  +>.$
  (se-alas(fug (~(put by fug) gyl ~)) u.lag)
::
++  se-join                                           ::  confirm connection
  |=  gyl+gill
  ^+  +>
  =.  +>  (se-text "[linked to {<gyl>}]")
  ?>  =(~ (~(got by fug) gyl))
  (se-alas(fug (~(put by fug) gyl `*target)) gyl)
::
++  se-nuke                                           ::  teardown
  |=  gyl+gill
  ^+  +>
  (se-drop:(se-pull gyl) & gyl)
::
++  se-like                                           ::  act in master
  |=  kus+ukase
  ?-    -.kus
      $add  
    |-  ^+  +>.^$
    ?~  p.kus  +>.^$
    $(p.kus t.p.kus, +>.^$ (se-link i.p.kus))
  ::
      $del
    |-  ^+  +>.^$
    ?~  p.kus  +>.^$
    $(p.kus t.p.kus, +>.^$ (se-nuke i.p.kus))
  ::
      $new
    |-  ^+  +>.^$
    ?~  p.kus  +>.^$
    $(p.kus t.p.kus, +>.^$ (se-born i.p.kus))
  ==
::
++  se-plot                                           ::  status line
  ^-  tape
  =+  lag=se-agon
  =+  ^=  pry
      |=  gill  ^-  tape
      =+((trip q.+<) ?:(=(our p.+>-) - :(welp (scow %p p.+>-) "/" -)))
  =+  ^=  yey
      |=  gill  ^-  tape
      =+((pry +<) ?:(=(lag `+>-) ['*' -] -))
  =+  ^=  yal  ^-  (list tape)
      %+  weld
        ^-  (list tape)
        %+  turn  (~(tap by fug))
        |=  {a+gill b+(unit target)}
        =+  c=(yey a)
        ?~(b ['?' c] c)
      ^-  (list tape)
      %+  turn  (skip (~(tap by fur)) |=({term *} (~(has by fug) [our +<-])))
      |=({term *} ['-' (pry our +<-)])
  |-  ^-  tape
  ?~  yal  ~
  ?~  t.yal  i.yal
  :(welp i.yal ", " $(yal t.yal))
::
++  se-klin                                           ::  disconnect app
  |=  gyl+gill
  +>(eel (~(del in eel) gyl))
::
++  se-link                                           ::  connect to app
  |=  gyl+gill
  +>(eel (~(put in eel) gyl))
::
++  se-blit                                           ::  give output
  |=  bil+dill-blit
  +>(biz [bil biz])
::
++  se-show                                           ::  show buffer, raw
  |=  lin+(pair @ud (list @c))
  ^+  +>
  ?:  =(mir lin)  +>
  =.  +>  ?:(=(q.mir q.lin) +> (se-blit %pro q.lin))
  =.  +>  ?:(=(p.mir p.lin) +> (se-blit %hop p.lin))
  +>(mir lin)
::
++  se-just                                           ::  adjusted buffer 
  |=  lin+(pair @ud (list @c))
  ^+  +>
  =.  off  ?:((lth p.lin edg) 0 (sub p.lin edg))
  (se-show (sub p.lin off) (scag edg (slag off q.lin)))
::
++  se-view                                           ::  flush buffer
  =+  gul=se-agon
  ?~  gul  +
  =+  gyr=(~(get by fug) u.gul)
  ?~  gyr  +>
  ?~  u.gyr  +>
  %-  se-just
  ~(ta-vew ta [& u.gul] u.u.gyr)
::
++  se-emit                                           ::  emit move
  |=  mov+move
  %_(+> moz [mov moz])
::
++  se-talk  
  |=  tac+(list tank) 
  ^+  +>
  (se-emit 0 %poke /drum/talk [our %talk] (said:talk our %drum now eny tac))
::
++  se-text                                           ::  return text
  |=  txt+tape
  ^+  +>
  ?.  se-ably  (se-talk [%leaf txt]~)
  (se-blit %out (tuba txt))
::
++  se-poke                                           ::  send a poke
  |=  {gyl+gill par+pear}
  (se-emit ost %poke (drum-path gyl) gyl par)
::
++  se-peer                                           ::  send a peer
  |=  gyl+gill
  (se-emit(fug (~(put by fug) gyl ~)) ost %peer (drum-path gyl) gyl /sole)
::
++  se-pull                                           ::  cancel subscription
  |=  gyl+gill
  (se-emit ost %pull (drum-path gyl) gyl ~)
::
++  se-tame                                           ::  switch connection
  |=  gyl+gill
  ^+  ta
  ~(. ta [& gyl] (need (~(got by fug) gyl)))
::
++  se-diff                                           ::  receive results
  |=  {gyl+gill fec+sole-effect}
  ^+  +>
  ta-abet:(ta-fec:(se-tame gyl) fec)
::
++  ta                                                ::  per target
  |_  $:  $:  liv+?                                   ::  don't delete
              gyl+gill                                ::  target app
          ==                                          ::
          target                                      ::  target state
      ==                                              ::
  ++  ta-abet                                         ::  resolve
    ^+  ..ta
    ?.  liv  
      ?:   (~(has in (deft-fish our)) gyl)
        (se-blit qit/~)
      (se-nuke gyl)
    ..ta(fug (~(put by fug) gyl ``target`+<+))
  ::
  ++  ta-poke  |=(a+pear +>(..ta (se-poke gyl a)))    ::  poke gyl
  ::
  ++  ta-act                                          ::  send action
    |=  act+sole-action
    ^+  +>
    (ta-poke %sole-action act)
  ::
  ++  ta-aro                                          ::  hear arrow
    |=  key+?($d $l $r $u)
    ^+  +>
    ?-  key
      $d  =.  ris  ~
          ?.  =(num.hit pos.hit) 
            (ta-mov +(pos.hit))
          ?:  =(0 (lent buf.say.inp))
            ta-bel
          (ta-hom:ta-nex %set ~)
      $l  ?:  =(0 pos.inp)  ta-bel
          +>(pos.inp (dec pos.inp), ris ~)
      $r  ?:  =((lent buf.say.inp) pos.inp)
            ta-bel
          +>(pos.inp +(pos.inp), ris ~)
      $u  =.  ris  ~
          ?:(=(0 pos.hit) ta-bel (ta-mov (dec pos.hit)))
    ==
  ::
  ++  ta-bel  .(+> (se-blit %bel ~))                  ::  beep
  ++  ta-cat                                          ::  mass insert
    |=  {pos+@ud txt+(list @c)}
    ^-  sole-edit
    :-  %mor
    |-  ^-  (list sole-edit)
    ?~  txt  ~
    [[%ins pos i.txt] $(pos +(pos), txt t.txt)]
  ::
  ++  ta-cut                                          ::  mass delete
    |=  {pos+@ud num+@ud}
    ^-  sole-edit
    :-  %mor
    |-(?:(=(0 num) ~ [[%del pos] $(num (dec num))]))
  ::
  ++  ta-det                                          ::  send edit
    |=  ted+sole-edit
    ^+  +>
    (ta-act %det [[his.ven.say.inp own.ven.say.inp] (sham buf.say.inp) ted])
  ::
  ++  ta-bac                                          ::  hear backspace
    ^+  .
    ?^  ris
      ?:  =(~ str.u.ris)
        ta-bel
      .(str.u.ris (scag (dec (lent str.u.ris)) str.u.ris))
    ?:  =(0 pos.inp)
      (ta-act %clr ~)
      :: .(+> (se-blit %bel ~))
    =+  pre=(dec pos.inp)
    (ta-hom %del pre)
  ::
  ++  ta-ctl                                          ::  hear control
    |=  key+@ud
    ^+  +>
    ?+    key  ta-bel
        $a  +>(pos.inp 0, ris ~)
        $b  (ta-aro %l)
        $c  ta-bel(ris ~)
        $d  ?:  &(=(0 pos.inp) =(0 (lent buf.say.inp)))
              +>(liv |)
            ta-del
        $e  +>(pos.inp (lent buf.say.inp))
        $f  (ta-aro %r)
        $g  ?~  ris  ta-bel
            (ta-hom(pos.hit num.hit, ris ~) [%set ~])
        $k  =+  len=(lent buf.say.inp)
            ?:  =(pos.inp len)
              ta-bel 
            %-  ta-hom(kil `(slag pos.inp buf.say.inp), ris ~)
            (ta-cut pos.inp (sub len pos.inp))
        $l  +>(+> (se-blit %clr ~))
        $n  (ta-aro %d)
        $p  (ta-aro %u)
        $r  ?~  ris 
              +>(ris `[pos.hit ~]) 
            ?:  =(0 pos.u.ris)
              ta-bel
            (ta-ser ~)
        $t  =+  len=(lent buf.say.inp)
            ?:  |(=(0 pos.inp) (lth len 2))
              ta-bel
            =+  sop=?:(=(len pos.inp) (dec pos.inp) pos.inp) 
            =.  pos.inp  +(sop)
            =.  ris  ~
            %-  ta-hom
            :~  %mor
                [%del sop]
                [%ins (dec sop) (snag sop buf.say.inp)]
            ==
        $u  ?:  =(0 pos.inp)
              ta-bel
            %-  ta-hom(kil `(scag pos.inp buf.say.inp), ris ~)
            (ta-cut 0 pos.inp)
        $v  ta-bel
        $x  +>(+> se-anon)
        $y  ?~  kil  ta-bel
            (ta-hom(ris ~) (ta-cat pos.inp u.kil))
    ==
  ::
  ++  ta-cru                                          ::  hear crud
    |=  {lab+@tas tac+(list tank)}
    =.  +>+>  (se-text (trip lab))
    (ta-tan tac)
  ::
  ++  ta-del                                          ::  hear delete
    ^+  .
    ?:  =((lent buf.say.inp) pos.inp)
      .(+> (se-blit %bel ~))
    (ta-hom %del pos.inp)
  ::
  ++  ta-erl                                          ::  hear local error
    |=  pos+@ud 
    ta-bel(pos.inp (min pos (lent buf.say.inp)))
  ::
  ++  ta-err                                          ::  hear remote error
    |=  pos+@ud
    (ta-erl (~(transpose sole say.inp) pos))
  ::
  ++  ta-fec                                          ::  apply effect
    |=  fec+sole-effect
    ^+  +>
    ?-    -.fec
      $bel  ta-bel
      $blk  +>
      $clr  +>(+> (se-blit fec))
      $det  (ta-got +.fec)
      $err  (ta-err +.fec)
      $mor  |-  ^+  +>.^$
            ?~  p.fec  +>.^$
            $(p.fec t.p.fec, +>.^$ ^$(fec i.p.fec))
      $nex  ta-nex
      $pro  (ta-pro +.fec)
      $tan  (ta-tan p.fec)
      $sag  +>(+> (se-blit fec))
      $sav  +>(+> (se-blit fec))
      $txt  $(fec [%tan [%leaf p.fec]~])
      $url  +>(+> (se-blit fec))
    ==
  ::
  ++  ta-dog                                          ::  change cursor
    |=  ted+sole-edit
    %_    +>
        pos.inp
      =+  len=(lent buf.say.inp)
      %+  min  len
      |-  ^-  @ud
      ?-  -.ted
        $del  ?:((gth pos.inp p.ted) (dec pos.inp) pos.inp)
        $ins  ?:((gte pos.inp p.ted) +(pos.inp) pos.inp)
        $mor  |-  ^-  @ud
              ?~  p.ted  pos.inp
              $(p.ted t.p.ted, pos.inp ^$(ted i.p.ted))
        $nop  pos.inp
        $set  len
      ==
    ==
  ::
  ++  ta-got                                          ::  apply change
    |=  cal+sole-change
    =^  ted  say.inp  (~(receive sole say.inp) cal)
    (ta-dog ted)
  ::
  ++  ta-hom                                          ::  local edit
    |=  ted+sole-edit
    ^+  +>
    =.  +>  (ta-det ted)
    =.  +>  (ta-dog(say.inp (~(commit sole say.inp) ted)) ted)
    +>
  ::
  ++  ta-met                                          ::  meta key
    |=  key+@ud
    ~&  [%ta-met key]
    +>
  ::
  ++  ta-mov                                          ::  move in history
    |=  sop+@ud
    ^+  +>
    ?:  =(sop pos.hit)  +>
    %+  %=  ta-hom
          pos.hit  sop
          lay.hit  %+  ~(put by lay.hit)  
                     pos.hit 
                   buf.say.inp
        ==
      %set
    %-  (bond |.((snag (sub num.hit +(sop)) old.hit)))
    (~(get by lay.hit) sop)
  ::
  ++  ta-nex                                          ::  advance history
    %_  .
      num.hit  +(num.hit)
      pos.hit  +(num.hit)
      ris  ~
      lay.hit  ~
      old.hit  [buf.say.inp old.hit]
    ==
  ::
  ++  ta-pro                                          ::  set prompt
    |=  pom+sole-prompt
    +>(pom pom(cad :(welp (scow %p p.gyl) ":" (trip q.gyl) cad.pom)))
  ::
  ++  ta-ret                                          ::  hear return
    (ta-act %ret ~)
  ::
  ++  ta-ser                                          ::  reverse search
    |=  ext+(list @c)
    ^+  +>
    ?:  |(?=($~ ris) =(0 pos.u.ris))  ta-bel
    =+  sop=?~(ext (dec pos.u.ris) pos.u.ris)
    =+  tot=(weld str.u.ris ext)
    =+  dol=(slag (sub num.hit sop) old.hit)
    =+  ^=  ser
        =+  ^=  beg
            |=  {a+(list @c) b+(list @c)}  ^-  ?
            ?~(a & ?~(b | &(=(i.a i.b) $(a t.a, b t.b))))
        |=  {a+(list @c) b+(list @c)}  ^-  ?
        ?~(a & ?~(b | |((beg a b) $(b t.b))))
    =+  ^=  sup  
        |-  ^-  (unit @ud)
        ?~  dol  ~
        ?:  (ser tot i.dol)
          `sop
        $(sop (dec sop), dol t.dol)
    ?~  sup  ta-bel
    (ta-mov(str.u.ris tot, pos.u.ris u.sup) (dec u.sup))
  ::
  ++  ta-tan                                          ::  print tanks
    |=  tac+(list tank)
    =+  wol=`wall`(zing (turn (flop tac) |=(a+tank (~(win re a) [0 edg]))))
    |-  ^+  +>.^$
    ?~  wol  +>.^$
    $(wol t.wol, +>+>.^$ (se-text i.wol))
  ::
  ++  ta-txt                                          ::  hear text
    |=  txt+(list @c)
    ^+  +>
    ?^  ris
      (ta-ser txt)
    %-  ta-hom
    :-  %mor
    |-  ^-  (list sole-edit)
    ?~  txt  ~
    [[%ins pos.inp i.txt] $(pos.inp +(pos.inp), txt t.txt)]
  ::
  ++  ta-vew                                          ::  computed prompt
    |-  ^-  (pair @ud (list @c))
    ?^  ris
      %=    $
          ris  ~
          cad.pom 
        :(welp "(reverse-i-search)'" (tufa str.u.ris) "': ")
      ==
    =-  [(add pos.inp (lent p.vew)) (weld (tuba p.vew) q.vew)]
    ^=  vew  ^-  (pair tape (list @c))
    ?:  vis.pom  [cad.pom buf.say.inp]
    :-  ;:  welp
          cad.pom
          ?~  buf.say.inp  ~
          ;:  welp
            "<"
            (scow %p (end 4 1 (sham buf.say.inp)))
            "> "
          ==
        ==
    =+  len=(lent buf.say.inp) 
    |-  ^-  (list @c) 
    ?:(=(0 len) ~ [`@c`'*' $(len (dec len))])
  --
--
