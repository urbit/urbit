::                                                      ::  ::
::::  /hoon/drum/hood/lib                               ::  ::
  ::                                                    ::  ::
/?    310                                               ::  version
/-    *sole
/+    sole
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%                                                      ::  ::
++  part      {$drum $2 pith-2}                         ::
++  part-old  {$drum $1 pith-1}                         ::
::                                                      ::
++  pith-1                                              ::       pre-style
  %+  cork  pith-2                                      ::
  |:($:pith-2 +<(bin ((map bone source-1))))            ::
::                                                      ::
++  source-1                                            ::
  %+  cork  source                                      ::
  |:($:source +<(mir ((pair @ud (list @c)))))           ::  style-less mir
::                                                      ::
++  pith-2                                              ::
  $:  eel/(set gill:gall)                               ::  connect to
      ray/(set well:gall)                               ::
      fur/(map dude:gall (unit server))                 ::  servers
      bin/(map bone source)                             ::  terminals
  ==                                                    ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
++  server                                              ::  running server
  $:  syd/desk                                          ::  app identity
      cas/case                                          ::  boot case
  ==                                                    ::
++  kill                                                ::  kill ring
  $:  pos/@ud                                           ::  ring position
      num/@ud                                           ::  number of entries
      max/_60                                           ::  max entries
      old/(list (list @c))                              ::  entries proper
  ==                                                    ::
++  source                                              ::  input device
  $:  edg/_80                                           ::  terminal columns
      off/@ud                                           ::  window offset
      kil/kill                                          ::  kill buffer
      inx/@ud                                           ::  ring index
      fug/(map gill:gall (unit target))                 ::  connections
      mir/(pair @ud stub)                               ::  mirrored terminal
  ==                                                    ::
++  history                                             ::  past input
  $:  pos/@ud                                           ::  input position
      num/@ud                                           ::  number of entries
      lay/(map @ud (list @c))                           ::  editing overlay
      old/(list (list @c))                              ::  entries proper
  ==                                                    ::
++  search                                              ::  reverse-i-search
  $:  pos/@ud                                           ::  search position
      str/(list @c)                                     ::  search string
  ==                                                    ::
++  target                                              ::  application target
  $:  $=  blt                                           ::  curr & prev belts
        %+  pair
          (unit dill-belt:dill)
        (unit dill-belt:dill)
      ris/(unit search)                                 ::  reverse-i-search
      hit/history                                       ::  all past input
      pom/sole-prompt                                   ::  static prompt
      inp/sole-command                                  ::  input state
  ==                                                    ::
--
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%
++  deft-apes                                           ::  default servers
  |=  [our/ship lit/?]
  %-  ~(gas in *(set well:gall))
  ^-  (list well:gall)
  ::  boot all default apps off the home desk
  ::
  =-  (turn - |=(a=term home+a))
  ^-  (list term)
  %+  welp
    :~  %dojo
        %spider
        %eth-watcher
        %azimuth-tracker
        %ping
        %goad
    ==
  ?:  lit
    ~
  :~  %acme
      %lens
      %clock
      %dojo
      %launch
      %publish
      %weather
      %group-store
      %group-hook
      %permission-store
      %permission-hook
      %permission-group-hook
      %invite-store
      %invite-hook
      %invite-view
      %chat-store
      %chat-hook
      %chat-view
      %chat-cli
      %soto
      %contact-store
      %contact-hook
      %contact-view
      %link-store
      %link-proxy-hook
      %link-listen-hook
      %link-view
      %metadata-store
  ==
::
++  deft-fish                                           ::  default connects
  |=  our/ship
  %-  ~(gas in *(set gill:gall))
  ^-  (list gill:gall)
  [[our %dojo] [our %chat-cli]~]
::
++  make                                                ::  initial part
  |=  our/ship
  ^-  part
  :*  %drum
      %2
      eel=(deft-fish our)
      ray=~
      fur=~
      bin=~
  ==
::
::
++  en-gill                                           ::  gill to wire
  |=  gyl/gill:gall
  ^-  wire
  [%drum %phat (scot %p p.gyl) q.gyl ~]
::
++  de-gill                                           ::  gill from wire
  |=  way/wire  ^-  gill:gall
  ?>(?=({@ @ ~} way) [(slav %p i.way) i.t.way])
--
::
::::
  ::
|=  {hid/bowl:gall part}                          ::  main drum work
=/  ost  0
=+  (~(gut by bin) ost *source)
=*  dev  -
|_  {moz/(list card:agent:gall) biz/(list dill-blit:dill)}
++  diff-sole-effect-phat                             ::  app event
  |=  {way/wire fec/sole-effect}
  =<  se-abet  =<  se-view
  =+  gyl=(de-gill way)
  ?:  (se-aint gyl)  +>.$
  (se-diff gyl fec)
::
++  peer                                              ::
  |=  pax/path
  ~|  [%drum-unauthorized our+our.hid src+src.hid]    ::  ourself
  ?>  (team:title our.hid src.hid)               ::  or our own moon
  =<  se-abet  =<  se-view
  (se-text "[{<src.hid>}, driving {<our.hid>}]")
::
++  poke-set-boot-apps                                ::
  |=  lit/?
  ^-  (quip card:agent:gall part)
  ::  We do not run se-abet:se-view here because that starts the apps,
  ::  and some apps are not ready to start (eg Talk crashes because the
  ::  terminal has width 0).  It appears the first message to drum must
  ::  be the peer.
  ::
  [~ +<+.^$(ray (deft-apes our.hid lit))]
::
++  poke-dill-belt                                    ::  terminal event
  |=  bet/dill-belt:dill
  =<  se-abet  =<  se-view
  (se-belt bet)
::
++  poke-dill-blit                                    ::  terminal output
  |=  bit/dill-blit:dill
  se-abet:(se-blit-sys bit)
::
++  poke-start                                        ::  start app
  |=  wel/well:gall
  =<  se-abet  =<  se-view
  (se-born wel)
::
++  poke-link                                         ::  connect app
  |=  gyl/gill:gall
  =<  se-abet  =<  se-view
  (se-link gyl)
::
++  poke-unlink                                       ::  disconnect app
  |=  gyl/gill:gall
  =<  se-abet  =<  se-view
  (se-drop:(se-pull gyl) & gyl)
::
++  poke-exit                                         ::  shutdown
  |=  ~
  se-abet:(se-blit-sys `dill-blit:dill`[%qit ~])
::
++  poke-put                                          ::  write file
  |=  {pax/path txt/@}
  se-abet:(se-blit-sys [%sav pax txt])                ::
::
++  poke
  |=  [=mark =vase]
  ?+  mark  ~|([%poke-drum-bad-mark mark] !!)
    %drum-put            =;(f (f !<(_+<.f vase)) poke-put)
    %drum-link           =;(f (f !<(_+<.f vase)) poke-link)
    %drum-unlink         =;(f (f !<(_+<.f vase)) poke-unlink)
    %drum-exit           =;(f (f !<(_+<.f vase)) poke-exit)
    %drum-start          =;(f (f !<(_+<.f vase)) poke-start)
    %drum-set-boot-apps  =;(f (f !<(_+<.f vase)) poke-set-boot-apps)
  ==
::
++  on-load
  |=  %1
  =<  se-abet  =<  se-view
  (se-born %home %goad)
::
++  reap-phat                                         ::  ack connect
  |=  {way/wire saw/(unit tang)}
  =<  se-abet  =<  se-view
  =+  gyl=(de-gill way)
  ?~  saw
    (se-join gyl)
  ::  Don't print stack trace because we probably just crashed to
  ::  indicate we don't connect to the console.
  ::
  (se-drop & gyl)
::
++  take                                              ::
  |=  [=wire =sign-arvo]
  %+  take-onto  wire
  ?>  ?=(%onto +<.sign-arvo)
  +>.sign-arvo
::
++  take-coup-phat                                    ::  ack poke
  |=  {way/wire saw/(unit tang)}
  =<  se-abet  =<  se-view
  ?~  saw  +>
  =+  gyl=(de-gill way)
  ?:  (se-aint gyl)  +>.$
  %-  se-dump:(se-drop:(se-pull gyl) & gyl)
  :_  u.saw
  >[%drum-coup-fail src.hid gyl]<
::
++  take-onto                                         ::  ack start
  |=  {way/wire saw/(each suss:gall tang)}
  =<  se-abet  =<  se-view
  ?>  ?=({@ @ ~} way)
  ?>  (~(has by fur) i.t.way)
  =/  wel/well:gall  [i.way i.t.way]
  ?-  saw
    {%| *}  (se-dump p.saw)
    {%& *}  ?>  =(q.wel p.p.saw)
            ::  =.  +>.$  (se-text "live {<p.saw>}")
            +>.$(fur (~(put by fur) q.wel `[p.wel %da r.p.saw]))
  ==
::
++  take-agent
  |=  [=wire =sign:agent:gall]
  ?+  wire  ~|([%drum-bad-take-agent wire -.sign] !!)
      [%drum %phat *]
    ?-  -.sign
        %poke-ack   (take-coup-phat t.t.wire p.sign)
        %watch-ack  (reap-phat t.t.wire p.sign)
        %kick       (quit-phat t.t.wire)
        %fact
      %+  diff-sole-effect-phat  t.t.wire
      ?>  ?=(%sole-effect p.cage.sign)
      !<(sole-effect q.cage.sign)
   ==
 ==
::
++  quit-phat                                         ::
  |=  way/wire
  =<  se-abet  =<  se-view
  =+  gyl=(de-gill way)
  ~&  [%drum-quit src.hid gyl]
  (se-drop %| gyl)
::                                                    ::  ::
::::                                                  ::  ::
  ::                                                  ::  ::
++  se-abet                                           ::  resolve
  ^-  (quip card:agent:gall part)
  =*  pith  +<+.$
  =.  .  se-subze:se-adze:se-adit
  :_  pith(bin (~(put by bin) ost dev))
  ^-  (list card:agent:gall)
  ?~  biz  (flop moz)
  :_  (flop moz)
  =/  =dill-blit:dill  ?~(t.biz i.biz [%mor (flop biz)])
  [%give %fact ~[/drum] %dill-blit !>(dill-blit)]
::
++  se-adit                                           ::  update servers
  ^+  .
  ::  ensure dojo connects after talk
  =*  dojo-on-top  |=([a=* b=*] |(=(%dojo a) &(!=(%dojo b) (aor a b))))
  %+  roll  (sort ~(tap in ray) dojo-on-top)
  =<  .(con +>)
  |:  $:{wel/well:gall con/_..se-adit}  ^+  con
  =.  +>.$  con
  =+  hig=(~(get by fur) q.wel)
  ?:  &(?=(^ hig) |(?=(~ u.hig) =(p.wel syd.u.u.hig)))  +>.$
  =.  +>.$  (se-text "activated app {(trip p.wel)}/{(trip q.wel)}")
  %-  se-emit(fur (~(put by fur) q.wel ~))
  =/  =wire  [%drum p.wel q.wel ~]
  [%pass wire %arvo %g %conf [our.hid q.wel] our.hid p.wel]
::
++  se-adze                                           ::  update connections
  ^+  .
  %+  roll  ~(tap in eel)
  =<  .(con +>)
  |:  $:{gil/gill:gall con/_.}  ^+  con
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
  |:  $:{{ost/bone dev/source} con/_.}  ^+  con
  =+  xeno=se-subze-local:%_(con ost ost, dev dev)
  xeno(ost ost.con, dev dev.con, bin (~(put by bin) ost dev.xeno))
::
++  se-subze-local
  ^+  .
  %-  ~(rep by fug)
  =<  .(con +>)
  |:  $:{{gil/gill:gall *} con/_.}  ^+  con
  =.  +>.$  con
  ?:  (~(has in eel) gil)
    +>.$
  (se-nuke gil)
::
++  se-aint                                           ::  ignore result
  |=  gyl/gill:gall
  ^-  ?
  ?.  (~(has by bin) ost)  &
  =+  gyr=(~(get by fug) gyl)
  |(?=(~ gyr) ?=(~ u.gyr))
::
++  se-alas                                           ::  recalculate index
  |=  gyl/gill:gall
  =+  [xin=0 wag=se-amor]
  |-  ^+  +>.^$
  ?~  wag  +>.^$(inx 0)
  ?:  =(i.wag gyl)  +>.^$(inx xin)
  $(wag t.wag, xin +(xin))
::
++  se-amor                                           ::  live targets
  ^-  (list gill:gall)
  %+  skim  ~(tap in eel)
  |=(a/gill:gall ?=({~ ~ *} (~(get by fug) a)))
::
++  se-anon                                           ::  rotate index
  =+  wag=se-amor
  ?~  wag  +
  ::  ~&  [%se-anon inx+inx wag+wag nex+(mod +(inx) (lent se-amor))]
  +(off 0, inx (mod +(inx) (lent wag)))
::
++  se-agon                                           ::  current gill
  ^-  (unit gill:gall)
  =+  wag=se-amor
  ?~  wag  ~
  ~|  [inx=inx wag=wag fug=fug eel=eel]
  `(snag inx `(list gill:gall)`wag)
::
++  se-belt                                           ::  handle input
  |=  bet/dill-belt:dill
  ^+  +>
  ?:  ?=({?($cru $hey $rez $yow) *} bet)              ::  target-agnostic
    ?-  bet
      {$cru *}  (se-dump:(se-text (trip p.bet)) q.bet)
      {$hey *}  +>(mir [0 ~])                         ::  refresh
      {$rez *}  +>(edg (dec p.bet))                   ::  resize window
      {$yow *}  ~&([%no-yow -.bet] +>)
    ==
  =+  gul=se-agon
  ?:  |(?=(~ gul) (se-aint u.gul))
    (se-blit %bel ~)
  ta-abet:(ta-belt:(se-tame u.gul) bet)
::
++  se-born                                           ::  new server
  |=  wel/well:gall
  ^+  +>
  ?:  (~(has in ray) wel)
    (se-text "[already running {<p.wel>}/{<q.wel>}]")
  %=  +>
    ray  (~(put in ray) wel)
    eel  (~(put in eel) [our.hid q.wel])
  ==
::
++  se-drop                                           ::  disconnect
  |=  {pej/? gyl/gill:gall}
  ^+  +>
  =+  lag=se-agon
  ?.  (~(has by fug) gyl)  +>.$
  =.  fug  (~(del by fug) gyl)
  =.  eel  ?.(pej eel (~(del in eel) gyl))
  =.  +>.$  ?.  &(?=(^ lag) !=(gyl u.lag))
              +>.$(inx 0)
            (se-alas u.lag)
  =.  +>.$  (se-text "[unlinked from {<gyl>}]")
  ?:  =(gyl [our.hid %dojo])                          ::  undead dojo
    (se-link gyl)
  +>.$
::
++  se-tab                                            ::  print tab completions
  |=  tl/(list {=cord =tank})
  ^+  +>
  =/  lots  (gth (lent tl) 10)
  =/  long
    ?:  lots
      0
    (roll (turn tl |=([=term *] (met 3 term))) max)
  %-  se-dump
  %-  flop
  ^-  (list tank)
  :-  leaf+"-----"
  %+  turn  tl
  |=  [=term =type=tank]
  ?:  lots
    leaf+(trip term)
  =/  type-text  ~(ram re type-tank)
  =/  spaces  (trip (fil 3 (sub long (met 3 term)) ' '))
  =/  =tape  "{(trip term)} {spaces} {type-text}"
  ::  If type is too long and not the only result, abbreviate
  ::
  ?:  (gth (lent type-text) edg)
    ?:  ?=([* ~] tl)
      :+  %rose
        ["" "" ""]
      ~[leaf+(trip term) type-tank]
    leaf+(weld (scag (sub edg 3) tape) "...")
  leaf+tape
::
++  se-dump                                           ::  print tanks
  |=  tac/(list tank)
  ^+  +>
  =/  wol/wall
    (zing (turn (flop tac) |=(a/tank (~(win re a) [0 edg]))))
  |-  ^+  +>.^$
  ?~  wol  +>.^$
  ?.  ((sane %t) (crip i.wol))  :: XX upstream validation
    ~&  bad-text+<`*`i.wol>
    $(wol t.wol)
  $(wol t.wol, +>.^$ (se-blit %out (tuba i.wol)))
::
++  se-join                                           ::  confirm connection
  |=  gyl/gill:gall
  ^+  +>
  =.  +>  (se-text "[linked to {<gyl>}]")
  ?>  ?=(~ (~(got by fug) gyl))
  (se-alas(fug (~(put by fug) gyl `*target)) gyl)
::
++  se-nuke                                           ::  teardown connection
  |=  gyl/gill:gall
  ^+  +>
  (se-drop:(se-pull gyl) & gyl)
::
++  se-klin                                           ::  disconnect app
  |=  gyl/gill:gall
  +>(eel (~(del in eel) gyl))
::
++  se-link                                           ::  connect to app
  |=  gyl/gill:gall
  +>(eel (~(put in eel) gyl))
::
++  se-blit                                           ::  give output
  |=  bil/dill-blit:dill
  +>(biz [bil biz])
::
++  se-blit-sys                                       ::  output to system
  |=  bil/dill-blit:dill  ^+  +>
  (se-emit %give %fact ~[/drum] %dill-blit !>(bil))
::
++  se-show                                           ::  show buffer, raw
  |=  lin/(pair @ud stub)
  ^+  +>
  =.  p.lin  (add p.lin (lent-stye:klr q.lin))
  ?:  =(mir lin)  +>
  =.  +>  ?:(=(p.mir p.lin) +> (se-blit %hop p.lin))
  =.  +>  ?:(=(q.mir q.lin) +> (se-blit %pom q.lin))
  +>(mir lin)
::
++  se-just                                           ::  adjusted buffer
  |=  {pom/stub lin/(pair @ud (list @c))}
  ^+  +>
  =/  pol  (lent-char:klr pom)
  =/  pos  (add pol p.lin)
  ?:  (gte (div (mul pol 100) edg) 35)      :: old style (long prompt)
    =/  off  ?:((lte pos edg) 0 (sub pos edg))
    %+  se-show
      (sub pos off)
    (swag:klr [off edg] (welp pom [*stye q.lin]~))
  =/  end  (sub edg pol)
  =.  off  ?:  (gth p.lin (add end off))
             (sub p.lin end)
           ?:  (lth p.lin off)
             (min p.lin (dec off))
           off
  %+  se-show
    (sub pos off)
  (welp pom [*stye (swag [off end] q.lin)]~)
::
++  se-view                                           ::  flush buffer
  ^+  .
  =+  gul=se-agon
  ?:  |(?=(~ gul) (se-aint u.gul))  +
  (se-just ta-vew:(se-tame u.gul))
::
++  se-emit
  |=  card:agent:gall
  %_(+> moz [+< moz])
::
++  se-text                                           ::  return text
  |=  txt/tape
  ^+  +>
  ?.  ((sane %t) (crip txt))  :: XX upstream validation
    ~&  bad-text+<`*`txt>
    +>
  (se-blit %out (tuba txt))
::
++  se-poke                                           ::  send a poke
  |=  {gyl/gill:gall par/cage}
  (se-emit %pass (en-gill gyl) %agent gyl %poke par)
::
++  se-peer                                           ::  send a peer
  |=  gyl/gill:gall
  %-  se-emit(fug (~(put by fug) gyl ~))
  [%pass (en-gill gyl) %agent gyl %watch /sole/drum]
::
++  se-pull                                           ::  cancel subscription
  |=  gyl/gill:gall
  (se-emit %pass (en-gill gyl) %agent gyl %leave ~)
::
++  se-tame                                           ::  switch connection
  |=  gyl/gill:gall
  ^+  ta
  ~(. ta gyl (need (~(got by fug) gyl)))
::
++  se-diff                                           ::  receive results
  |=  {gyl/gill:gall fec/sole-effect}
  ^+  +>
  ta-abet:(ta-fec:(se-tame gyl) fec)
::
++  ta                                                ::  per target
  |_  {gyl/gill:gall target}                         ::  app and state
  ++  ta-abet                                         ::  resolve
    ^+  ..ta
    ..ta(fug (~(put by fug) gyl ``target`+<+))
  ::
  ++  ta-poke  |=(a/cage +>(..ta (se-poke gyl a)))    ::  poke gyl
  ::
  ++  ta-act                                          ::  send action
    |=  act/sole-action
    ^+  +>
    (ta-poke %sole-action !>(act))
  ::
  ++  ta-aro                                          ::  hear arrow
    |=  key/?($d $l $r $u)
    ^+  +>
    =.  ris  ~
    ?-  key
      $d  ?.  =(num.hit pos.hit)
            (ta-mov +(pos.hit))
          ?:  =(0 (lent buf.say.inp))
            ta-bel
          (ta-hom:ta-nex %set ~)
      $l  ?:  =(0 pos.inp)  ta-bel
          +>(pos.inp (dec pos.inp))
      $r  ?:  =((lent buf.say.inp) pos.inp)
            ta-bel
          +>(pos.inp +(pos.inp))
      $u  ?:(=(0 pos.hit) ta-bel (ta-mov (dec pos.hit)))
    ==
  ::
  ++  ta-bel                                          ::  beep
    .(..ta (se-blit %bel ~), q.blt ~)                 ::  forget belt
  ::
  ++  ta-belt                                         ::  handle input
    |=  bet/dill-belt:dill
    ^+  +>
    ?<  ?=({?($cru $hey $rez $yow) *} bet)            ::  target-specific
    =.  blt  [q.blt `bet]                             ::  remember belt
    ?-  bet
      {$aro *}  (ta-aro p.bet)
      {$bac *}  ta-bac
      {$ctl *}  (ta-ctl p.bet)
      {$del *}  ta-del
      {$met *}  (ta-met p.bet)
      {$ret *}  ta-ret
      {$txt *}  (ta-txt p.bet)
    ==
  ::
  ++  ta-det                                          ::  send edit
    |=  ted/sole-edit
    ^+  +>
    %^    ta-act
        %drum
      %det
    [[his.ven.say.inp own.ven.say.inp] (sham buf.say.inp) ted]
  ::
  ++  ta-bac                                          ::  hear backspace
    ^+  .
    ?^  ris
      ?:  =(~ str.u.ris)
        ta-bel
      .(str.u.ris (scag (dec (lent str.u.ris)) str.u.ris))
    ?:  =(0 pos.inp)
      ?~  buf.say.inp
        (ta-act %drum %clr ~)
      ta-bel
    (ta-hom %del (dec pos.inp))
  ::
  ++  ta-ctl                                          ::  hear control
    |=  key/@ud
    ^+  +>
    =.  ris  ?.(?=(?($g $r) key) ~ ris)
    ?+    key    ta-bel
        $a  +>(pos.inp 0)
        $b  (ta-aro %l)
        $c  ta-bel
        $d  ?^  buf.say.inp
              ta-del
            ?:  (~(has in (deft-fish our.hid)) gyl)
              +>(..ta (se-blit qit+~))                ::  quit pier
            +>(..ta (se-klin gyl))                    ::  unlink app
        $e  +>(pos.inp (lent buf.say.inp))
        $f  (ta-aro %r)
        $g  ?~  ris  ta-bel
            (ta-hom(pos.hit num.hit, ris ~) [%set ~])
        $i  ta-tab
        $k  =+  len=(lent buf.say.inp)
            ?:  =(pos.inp len)
              ta-bel
            (ta-kil %r [pos.inp (sub len pos.inp)])
        $l  +>(..ta (se-blit %clr ~))
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
            =+  sop=(sub pos.inp ?:(=(len pos.inp) 2 1))
            (ta-hom (rep:edit [sop 2] (flop (swag [sop 2] buf.say.inp))))
        $u  ?:  =(0 pos.inp)
              ta-bel
            (ta-kil %l [0 pos.inp])
        $v  ta-bel
        $w  ?:  =(0 pos.inp)
              ta-bel
            =+  sop=(ta-pos %l %ace pos.inp)
            (ta-kil %l [(sub pos.inp sop) sop])
        $x  +>(..ta se-anon)
        $y  ?:  =(0 num.kil)
              ta-bel
            (ta-hom (cat:edit pos.inp ta-yan))
    ==
  ::
  ++  ta-del                                          ::  hear delete
    ^+  .
    ?:  =((lent buf.say.inp) pos.inp)
      ta-bel
    (ta-hom %del pos.inp)
  ::
  ++  ta-erl                                          ::  hear local error
    |=  pos/@ud
    ta-bel(pos.inp (min pos (lent buf.say.inp)))
  ::
  ++  ta-err                                          ::  hear remote error
    |=  pos/@ud
    (ta-erl (~(transpose sole say.inp) pos))
  ::
  ++  ta-fec                                          ::  apply effect
    |=  fec/sole-effect
    ^+  +>
    ?-  fec
      {$bel *}  ta-bel
      {$blk *}  +>
      {$clr *}  +>(..ta (se-blit fec))
      {$det *}  (ta-got +.fec)
      {$err *}  (ta-err p.fec)
      {$klr *}  +>(..ta (se-blit %klr (make:klr p.fec)))
      {$mor *}  |-  ^+  +>.^$
                ?~  p.fec  +>.^$
                $(p.fec t.p.fec, +>.^$ ^$(fec i.p.fec))
      {$nex *}  ta-nex
      {$pro *}  (ta-pro +.fec)
      {$tab *}  +>(..ta (se-tab p.fec))
      {$tan *}  +>(..ta (se-dump p.fec))
      {$sag *}  +>(..ta (se-blit fec))
      {$sav *}  +>(..ta (se-blit fec))
      {$txt *}  +>(..ta (se-text p.fec))
      {$url *}  +>(..ta (se-blit fec))
    ==
  ::
  ++  ta-dog                                          ::  change cursor
    |=  ted/sole-edit
    %_    +>
        pos.inp
      =+  len=(lent buf.say.inp)
      %+  min  len
      |-  ^-  @ud
      ?-  ted
        {$del *}  ?:((gth pos.inp p.ted) (dec pos.inp) pos.inp)
        {$ins *}  ?:((gte pos.inp p.ted) +(pos.inp) pos.inp)
        {$mor *}  |-  ^-  @ud
                  ?~  p.ted  pos.inp
                  $(p.ted t.p.ted, pos.inp ^$(ted i.p.ted))
        {$nop *}  pos.inp
        {$set *}  len
      ==
    ==
  ::
  ++  ta-off                                          ::  reset buffer offset
    |=  ted/sole-edit
    =?  off  (any:edit ted |=(a/sole-edit ?=($set -.a)))  0
    +>
  ::
  ++  ta-got                                          ::  apply change
    |=  cal/sole-change
    =^  ted  say.inp  (~(receive sole say.inp) cal)
    (ta-dog:(ta-off ted.cal) ted)
  ::
  ++  ta-hom                                          ::  local edit
    |=  ted/sole-edit
    ^+  +>
    =.  +>  (ta-det:(ta-off ted) ted)
    (ta-dog(say.inp (~(commit sole say.inp) ted)) ted)
  ::
  ++  ta-jump                                         ::  buffer pos
    |=  {dir/?($l $r) til/?($ace $edg $wrd) pos/@ud}
    ^-  @ud
    %-  ?:(?=($l dir) sub add)
    [pos (ta-pos dir til pos)]
  ::
  ++  ta-kil                                          ::  kill selection
    |=  {dir/?($l $r) sel/{@ @}}
    ^+  +>
    =+  buf=(swag sel buf.say.inp)
    %.  (cut:edit sel)
    %=  ta-hom
        kil
      ?.  ?&  ?=(^ old.kil)
              ?=(^ p.blt)
              ?|  ?=({$ctl ?($k $u $w)} u.p.blt)
                  ?=({$met ?($d $bac)} u.p.blt)
          ==  ==
        %=  kil                                       ::  prepend
          num  +(num.kil)
          pos  +(num.kil)
          old  (scag max.kil `(list (list @c))`[buf old.kil])
        ==
      %=  kil                                         ::  cumulative yanks
        pos  num.kil
        old  :_  t.old.kil
             ?-  dir
               $l  (welp buf i.old.kil)
               $r  (welp i.old.kil buf)
      ==     ==
    ==
  ::
  ++  ta-met                                          ::  meta key
    |=  key/@ud
    ^+  +>
    =.  ris  ~
    ?+    key    ta-bel
      $dot  ?.  &(?=(^ old.hit) ?=(^ i.old.hit))      ::  last "arg" from hist
              ta-bel
            =+  old=`(list @c)`i.old.hit
            =+  sop=(ta-jump(buf.say.inp old) %l %ace (lent old))
            (ta-hom (cat:edit pos.inp (slag sop old)))
            ::
      $bac  ?:  =(0 pos.inp)                          ::  kill left-word
              ta-bel
            =+  sop=(ta-pos %l %edg pos.inp)
            (ta-kil %l [(sub pos.inp sop) sop])
            ::
      $b    ?:  =(0 pos.inp)                          ::  jump left-word
              ta-bel
            +>(pos.inp (ta-jump %l %edg pos.inp))
            ::
      $c    ?:  =(pos.inp (lent buf.say.inp))         ::  capitalize
              ta-bel
            =+  sop=(ta-jump %r %wrd pos.inp)
            %-  ta-hom(pos.inp (ta-jump %r %edg sop))
            %+  rep:edit  [sop 1]
            ^-  (list @c)  ^-  (list @)               :: XX unicode
            (cuss `tape``(list @)`(swag [sop 1] buf.say.inp))
            ::
      $d    ?:  =(pos.inp (lent buf.say.inp))         ::  kill right-word
              ta-bel
            (ta-kil %r [pos.inp (ta-pos %r %edg pos.inp)])
            ::
      $f    ?:  =(pos.inp (lent buf.say.inp))         ::  jump right-word
              ta-bel
            +>(pos.inp (ta-jump %r %edg pos.inp))
            ::
      $r    %-  ta-hom(lay.hit (~(put by lay.hit) pos.hit ~))
            :-  %set                                  ::  revert hist edit
            ?:  =(pos.hit num.hit)  ~
            (snag (sub num.hit +(pos.hit)) old.hit)
            ::
      $t    =+  a=(ta-jump %r %edg pos.inp)           ::  transpose words
            =+  b=(ta-jump %l %edg a)
            =+  c=(ta-jump %l %edg b)
            ?:  =(b c)
              ta-bel
            =+  next=[b (sub a b)]
            =+  prev=[c (ta-pos %r %edg c)]
            %-  ta-hom(pos.inp a)
            :~  %mor
                (rep:edit next (swag prev buf.say.inp))
                (rep:edit prev (swag next buf.say.inp))
            ==
            ::
      ?($u $l)                                        ::  upper/lower case
            ?:  =(pos.inp (lent buf.say.inp))
              ta-bel
            =+  case=?:(?=($u key) cuss cass)
            =+  sop=(ta-jump %r %wrd pos.inp)
            =+  sel=[sop (ta-pos %r %edg sop)]
            %-  ta-hom
            %+  rep:edit  sel
            ^-  (list @c)  ^-  (list @)               :: XX unicode
            (case `tape``(list @)`(swag sel buf.say.inp))
            ::
      $y    ?.  ?&  ?=(^ old.kil)                     ::  rotate & yank
                    ?=(^ p.blt)
                    ?|  ?=({$ctl $y} u.p.blt)
                        ?=({$met $y} u.p.blt)
                ==  ==
              ta-bel
            =+  las=(lent ta-yan)
            =.  pos.kil  ?:(=(1 pos.kil) num.kil (dec pos.kil))
            (ta-hom (rep:edit [(sub pos.inp las) las] ta-yan))
    ==
  ::
  ++  ta-mov                                          ::  move in history
    |=  sop/@ud
    ^+  +>
    ?:  =(sop pos.hit)  +>
    %-  %=  ta-hom
          pos.hit  sop
          lay.hit  (~(put by lay.hit) pos.hit buf.say.inp)
        ==
    :-  %set
    %.  (~(get by lay.hit) sop)
    (bond |.((snag (sub num.hit +(sop)) old.hit)))
  ::
  ++  ta-nex                                          ::  advance history
    ^+  .
    =.  ris  ~
    =.  lay.hit  ~
    ?:  ?|  ?=(~ buf.say.inp)
            &(?=(^ old.hit) =(buf.say.inp i.old.hit))
        ==
      .(pos.hit num.hit)
    %_  .
      num.hit  +(num.hit)
      pos.hit  +(num.hit)
      old.hit  [buf.say.inp old.hit]
    ==
  ::
  ++  ta-pos                                          ::  buffer pos offset
    |=  {dir/?($l $r) til/?($ace $edg $wrd) pos/@ud}
    ^-  @ud
    %-  ?-  til  $ace  ace:offset
                 $edg  edg:offset
                 $wrd  wrd:offset
        ==
    ?-  dir  $l  (flop (scag pos buf.say.inp))
             $r  (slag pos buf.say.inp)
    ==
  ::
  ++  ta-pro                                          ::  set prompt
    |=  pom/sole-prompt
    %_    +>
        pom
      %_    pom
          cad
        ;:  welp
          ?.  ?=($earl (clan:title p.gyl))
            (cite:title p.gyl)
          (scow %p p.gyl)
        ::
          ":"
          (trip q.gyl)
          cad.pom
        ==
      ==
    ==
  ::
  ++  ta-ret                                          ::  hear return
    (ta-act %drum %ret ~)
  ::
  ++  ta-tab                                          ::  hear tab
    (ta-act %drum %tab pos.inp)
  ::
  ++  ta-ser                                          ::  reverse search
    |=  ext/(list @c)
    ^+  +>
    ?:  |(?=(~ ris) =(0 pos.u.ris))
      ta-bel
    =+  sop=?~(ext (dec pos.u.ris) pos.u.ris)
    =+  tot=(weld str.u.ris ext)
    =+  dol=(slag (sub num.hit sop) old.hit)
    =/  sup
        |-  ^-  (unit @ud)
        ?~  dol  ~
        ?^  (find tot i.dol)
          `sop
        $(sop (dec sop), dol t.dol)
    ?~  sup  ta-bel
    (ta-mov(str.u.ris tot, pos.u.ris u.sup) (dec u.sup))
  ::
  ++  ta-txt                                          ::  hear text
    |=  txt/(list @c)
    ^+  +>
    ?^  ris
      (ta-ser txt)
    (ta-hom (cat:edit pos.inp txt))
  ::
  ++  ta-vew                                          ::  computed prompt
    ^-  {pom/stub lin/(pair @ud (list @c))}
    =;  vew/(pair (list @c) styx)
      [(make:klr q.vew) pos.inp p.vew]
    ?:  vis.pom
      :-  buf.say.inp                                 ::  default prompt
      ?~  ris
        cad.pom
      :(welp "(reverse-i-search)'" (tufa str.u.ris) "': ")
    :-  (reap (lent buf.say.inp) `@c`'*')             ::  hidden input
    %+  welp
      cad.pom
    ?~  buf.say.inp  ~
    :(welp "<" (scow %p (end 4 1 (sham buf.say.inp))) "> ")
  ::
  ++  ta-yan                                          ::  yank
    (snag (sub num.kil pos.kil) old.kil)
  --
++  edit                                              ::  produce sole-edits
  |%
  ++  cat                                             ::  mass insert
    |=  {pos/@ud txt/(list @c)}
    ^-  sole-edit
    :-  %mor
    |-  ^-  (list sole-edit)
    ?~  txt  ~
    [[%ins pos i.txt] $(pos +(pos), txt t.txt)]
  ::
  ++  cut                                             ::  mass delete
    |=  {pos/@ud num/@ud}
    ^-  sole-edit
    :-  %mor
    |-  ^-  (list sole-edit)
    ?:  =(0 num)  ~
    [[%del pos] $(num (dec num))]
  ::
  ++  rep                                             ::  mass replace
    |=  {{pos/@ud num/@ud} txt/(list @c)}
    ^-  sole-edit
    :~  %mor
        (cut pos num)
        (cat pos txt)
    ==
  ++  any                                             ::  matches?
    |=  {a/sole-edit b/$-(sole-edit ?)}
    ^-  ?
    ?.  ?=($mor -.a)  (b a)
    (lien p.a |=(c/sole-edit ^$(a c)))
  --
++  offset                                            ::  calculate offsets
  |%
  ++  alnm                                            ::  alpha-numeric
    |=  a/@  ^-  ?
    ?|  &((gte a '0') (lte a '9'))
        &((gte a 'A') (lte a 'Z'))
        &((gte a 'a') (lte a 'z'))
    ==
  ::
  ++  ace                                             ::  next whitespace
    |=  a/(list @)
    =|  {b/_| i/@ud}
    |-  ^-  @ud
    ?~  a  i
    =/  c  !=(32 i.a)
    =.  b  |(b c)
    ?:  &(b !|(=(0 i) c))  i
    $(i +(i), a t.a)
  ::
  ++  edg                                             ::  next word boundary
    |=  a/(list @)
    =|  {b/_| i/@ud}
    |-  ^-  @ud
    ?~  a  i
    =/  c  (alnm i.a)
    =.  b  |(b c)
    ?:  &(b !|(=(0 i) c))  i
    $(i +(i), a t.a)
  ::
  ++  wrd                                             ::  next or current word
    |=  a/(list @)
    =|  i/@ud
    |-  ^-  @ud
    ?:  |(?=(~ a) (alnm i.a))  i
    $(i +(i), a t.a)
  --
::
++  klr                                               ::  styx/stub engine
  =,  dill
  |%
  ++  make                                            ::  stub from styx
    |=  a/styx  ^-  stub
    =|  b/stye
    %+  reel
    |-  ^-  stub
    %-  zing  %+  turn  a
    |=  a/$@(@t (pair styl styx))
    ?@  a  [b (tuba (trip a))]~
    ^$(a q.a, b (styd p.a b))
    ::
    |=  {a/(pair stye (list @c)) b/stub}
    ?~  b  [a ~]
    ?.  =(p.a p.i.b)  [a b]
    [[p.a (weld q.a q.i.b)] t.b]
  ::
  ++  styd                                            ::  stye from styl
    |=  {a/styl b/stye}  ^+  b                        ::  with inheritance
    :+  ?~  p.a  p.b
        ?~  u.p.a  ~
        (~(put in p.b) u.p.a)
     (fall p.q.a p.q.b)
     (fall q.q.a q.q.b)
  ::
  ++  lent-stye
    |=  a/stub  ^-  @
    (roll (lnts-stye a) add)
  ::
  ++  lent-char
    |=  a/stub  ^-  @
    (roll (lnts-char a) add)
  ::
  ++  lnts-stye                                       ::  stub pair head lengths
    |=  a/stub  ^-  (list @)
    %+  turn  a
    |=  a/(pair stye (list @c))
    ;:  add                        ::  presumes impl of cvrt:ansi in %dill
        (mul 5 2)                  ::  bg
        (mul 5 2)                  ::  fg
        =+  b=~(wyt in p.p.a)      ::  effect
        ?:(=(0 b) 0 (mul 4 +(b)))
    ==
  ::
  ++  lnts-char                                       ::  stub pair tail lengths
    |=  a/stub  ^-  (list @)
    %+  turn  a
    |=  a/(pair stye (list @c))
    (lent q.a)
  ::
  ++  brek                                            ::  index + incl-len of
    |=  {a/@ b/(list @)}                              ::  stub pair w/ idx a
    =|  {c/@ i/@}
    |-  ^-  (unit (pair @ @))
    ?~  b  ~
    =.  c  (add c i.b)
    ?:  (gte c a)
      `[i c]
    $(i +(i), b t.b)
  ::
  ++  slag                                            ::  slag stub, keep stye
    |=  {a/@ b/stub}
    ^-  stub
    =+  c=(lnts-char b)
    =+  i=(brek a c)
    ?~  i  b
    =+  r=(^slag +(p.u.i) b)
    ?:  =(a q.u.i)
      r
    =+  n=(snag p.u.i b)
    :_  r  :-  p.n
    (^slag (sub (snag p.u.i c) (sub q.u.i a)) q.n)
  ::
  ++  scag                                            ::  scag stub, keep stye
    |=  {a/@ b/stub}
    ^-  stub
    =+  c=(lnts-char b)
    =+  i=(brek a c)
    ?~  i  b
    ?:  =(a q.u.i)
      (^scag +(p.u.i) b)
    %+  welp
      (^scag p.u.i b)
    =+  n=(snag p.u.i b)
    :_  ~  :-  p.n
    (^scag (sub (snag p.u.i c) (sub q.u.i a)) q.n)
  ::
  ++  swag                                            ::  swag stub, keep stye
    |=  {{a/@ b/@} c/stub}
    (scag b (slag a c))
  --
--
