::                                                      ::  ::
::::  /hoon/helm/lib                                    ::  ::
  ::                                                    ::  ::
/?    310                                               ::  version
/-    sole
/+    talk
[. sole]
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%                                                      ::  ::
++  helm-part  {$helm $0 helm-pith}                     ::  helm state
++  helm-pith                                           ::  helm content
  $:  bur/(unit (pair ship mace:ames))                 ::  requesting ticket
      hoc/(map bone helm-session)                       ::  consoles
  ==                                                    ::  
++  helm-session                                        ::
  $:  say/sole-share                                    ::  console state
      mud/(unit (sole-dialog @ud))                      ::  console dialog
  ==                                                    ::  
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
++  hood-begin                                          ::  begin command
  $:  his/@p                                            ::  identity
      tic/@p                                            ::  ticket
      eny/@t                                            ::  entropy
      ges/gens:ames                                    ::  description
  ==                                                    ::
++  hood-init                                           ::  report init
  $:  him/ship                                          ::
  ==                                                    ::
++  hood-reset                                          ::  reset command
  $~                                                    ::
++  helm-verb                                           ::  reset command
  $~                                                    ::
++  hood-reload                                         ::  reload command
  (list term)                                           ::
--                                                      ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|=  {bowl:gall helm-part}                              ::  main helm work
=+  sez=(fall (~(get by hoc) ost) *helm-session)
=>  |%                                                  ::  arvo structures
    ++  card                                            ::
      $%  {$cash wire p/@p q/buck:ames}                ::
          {$conf wire dock $load ship term}             ::
          {$flog wire flog:dill}                       ::
          {$funk wire @p @p @}                          ::   
          {$serv wire ?(desk beam)}                     ::
          {$poke wire dock pear}                        ::
          {$wont wire sock path *}                      ::  send message
      ==                                                ::
    ++  move  (pair bone card)                          ::  user-level move
    ++  pear                                            ::  poke fruit
      $%  {$hood-unsync desk ship desk}                 ::
          {$talk-command command:talk}                  ::
          {$ask-mail cord}                              ::
          {$helm-hi cord}                               ::
      ==                                                ::
    --
|_  moz/(list move)
++  abet                                              ::  resolve
  [(flop moz) %_(+>+>+<+ hoc (~(put by hoc) ost sez))]
::
++  emit  |=(card %_(+> moz [[ost +<] moz]))          ::  return card
++  emil                                              ::  return cards
  |=  (list card) 
  ^+  +>
  ?~(+< +> $(+< t.+<, +> (emit i.+<)))
::
++  poke-begin                                        ::  make/send keypair
  |=  hood-begin  =<  abet
  ?>  ?=($~ bur)
  =+  buz=(shaz :(mix (jam ges) eny))
  =+  loy=(pit:nu:crub:crypto 512 buz)
  %-  emit(bur `[his [0 sec:ex:loy]~])
  [%wont /helm/ticket [our (sein:title his)] /a/ta his tic ges pub:ex:loy]
::
++  poke-spawn
  |=  {him/ship key/@pG}  =<  abet
  %-  emit
  [%funk ~ our him (shax key)]
::
++  poke-init                                         ::  initialize
  |=  him/ship  =<  abet
  (emit %flog /helm %crud %hax-init leaf+(scow %p him) ~)
::
++  poke-mass
  |=  $~  =<  abet
  (emit %flog /heft %crud %hax-heft ~)
::
++  poke-send-hi  
  |=  {her/ship mes/(unit tape)}  =<  abet
  %^  emit  %poke  /helm/hi/(scot %p her)
  [[her %hood] %helm-hi ?~(mes '' (crip u.mes))]
::
++  poke-send-ask
  |=  mel/cord  =<  abet
  %^  emit  %poke  /helm/ask/(scot %p ~marzod)
  [[~marzod %ask] %ask-mail mel]
::
++  poke-serve
  |=  top/?(desk beam)  =<  abet
  (emit %serv /helm/serv top)
::
++  poke-hi  |=(mes/@t abet:(emit %flog /di %text "< {<src>}: {(trip mes)}"))
++  coup-hi
  |=  {pax/path cop/(unit tang)}  =<  abet
  ?>  ?=({@t $~} pax)
  (emit %flog ~ %text "hi {(trip i.pax)} {?~(cop "" "un")}successful")
::
++  coup-ask
  |=  {pax/path cop/(unit tang)}  =<  abet
  ?>  ?=({@t $~} pax)
  (emit %flog ~ %text "ask {<src>} {?~(cop "" "un")}successful")
::
++  poke-reload  |=(all/(list term) (poke-reload-desk %home all))
++  poke-reload-desk                                 ::  reload vanes
  |=  {syd/desk all/(list term)}  =<  abet
  %-  emil
  %-  flop
  %+  turn  all
  =+  top=`path`/(scot %p our)/[syd]/(scot %da now)
  =/  van/(list {term $~})
    :-  zus=[%zuse ~]
    (~(tap by dir:.^(arch %cy (welp top /sys/vane))))
  |=  nam/@tas
    =.  nam
    ?.  =(1 (met 3 nam))
      nam
    =+  ^-  zaz/(list {p/knot $~})
        (skim van |=({a/term $~} =(nam (end 3 1 a))))
    ?>  ?=({{@ $~} $~} zaz)
    `term`p.i.zaz
  =+  tip=(end 3 1 nam)
  =+  zus==('z' tip) 
  =+  way=?:(zus (welp top /sys/[nam]) (welp top /sys/vane/[nam]))
  =+  fil=.^(@ %cx (welp way /hoon))
  [%flog /reload [%veer ?:(=('z' tip) %$ tip) way fil]]
::
++  poke-invite                                       ::  send invite; fake
  |=  {who/@p myl/@t}  =<  abet
  %^  emit  %poke  /helm/invite
  :-  [our %talk]
  (said:talk our %helm now eny [%leaf "invited: {<who>} at {(trip myl)}"]~)
::
++  poke-reset                                        ::  reset system
  |=  hood-reset  =<  abet
  %-  emil
  %-  flop  ^-  (list card) 
  =+  top=`path`/(scot %p our)/home/(scot %da now)/sys
  =+  hun=.^(@ %cx (welp top /hoon/hoon))
  =+  arv=.^(@ %cx (welp top /arvo/hoon))  
  :-  [%flog /reset [%velo `@t`hun `@t`arv]]
  :-  =+  way=(weld top `path`/zuse)
      [%flog /reset %veer %$ way .^(@ %cx (welp way /hoon))]
  %+  turn
    ^-  (list {p/@tas q/@tas})
    :~  [%a %ames]
        [%b %behn]
        [%c %clay]
        [%d %dill]
        [%e %eyre]
        [%f %ford]
        [%g %gall]
        [%j %jael]
    ==
  |=  {p/@tas q/@tas}
  =+  way=`path`(welp top /vane/[q])
  =+  txt=.^(@ %cx (welp way /hoon))
  [%flog /reset %veer p way txt]
::
++  poke-wyll                                         ::  hear certificate
  |=  wil/(unit wyll:ames)
  ?>  ?=(^ bur)
  ?>  ?=(^ wil)  
  =<  abet
  %-  emil(bur ~)
  :~  [%cash /helm p.u.bur q.u.bur u.wil]
      [%poke /helm [our %hood] %hood-unsync %base (sein:title our) %kids]
  ==
::
++  poke-verb                                         ::  toggle verbose
  |=  $~  =<  abet
  (emit %flog /helm %verb ~)
::
++  take-onto                                         ::  result of %conf
  |=  saw/(each suss:gall tang)  =<  abet
  %-  emit
  ?-   -.saw
    $|  [%flog ~ %crud %onto `tang`p.saw]
    $&  [%flog ~ %text "<{<p.saw>}>"]
  ==
::
++  take-note                                         ::  result of %init
  |=  {way/wire chr/@tD tan/tank}  =<  abet
  (emit %flog ~ %text chr ' ' ~(ram re tan))
::
++  take-woot                                         ::  result of %wont
  |=  {way/wire her/ship cop/coop}  =<  abet
  (emit %flog ~ %text "woot: {<[way cop]>}")
--
