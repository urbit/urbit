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
  $:  bur/(unit (pair ship mace))                       ::  requesting ticket
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
      ges/gens                                          ::  description
  ==                                                    ::
++  hood-init                                           ::  report init
  $:  him/ship                                          ::
  ==                                                    ::
++  hood-reset                                          ::  reset command
  $~                                                    ::
++  hood-deset                                          ::  reset command
  $~                                                    ::
++  helm-verb                                           ::  reset command
  $~                                                    ::
++  hood-reload                                         ::  reload command
  (list term)                                           ::
--                                                      ::
::                                                      ::  ::
::::                                                    ::  ::
  !:                                                    ::  ::
|=  {bowl helm-part}                                  ::  main helm work
=+  sez=(fall (~(get by hoc) ost) *helm-session)
=>  |%                                                ::  arvo structures
    ++  card                                          ::
      $%  {$cash wire p/@p q/buck}                    ::
          {$conf wire dock $load ship term}           ::
          {$flog wire flog}                           ::
          {$poke wire dock pear}                      ::
          {$wont wire sock path *}                    ::  send message
      ==                                              ::
    ++  move  (pair bone card)                        ::  user-level move
    ++  pear                                          ::  poke fruit
      $%  {$hood-unsync desk ship desk}               ::
          {$talk-command command:talk}                ::
          {$ask-mail cord}                            ::
          {$helm-hi cord}                             ::
      ==                                              ::
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
  =+  buz=(shax :(mix (jam ges) eny))
  =+  loy=(bruw 2.048 buz)
  %-  emit(bur `[his [0 sec:ex:loy]~])
  [%wont /helm/ticket [our (sein his)] /a/ta his tic ges pub:ex:loy]
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
  %^  emit  %poke  /helm/ask/(scot %p ~doznec)
  [[~doznec %ask] %ask-mail mel]
::
++  poke-hi  |=(mes/@t abet:(emit %flog /di %text "< {<src>}: {(trip mes)}"))
++  coup-hi
  |=  {pax/path cop/(unit tang)}  =<  abet
  ?>  ?=({@t $~} pax)
  (emit %flog ~ %text "hi {(trip i.pax)} {?~(cop "" "un")}succesful")
::
++  poke-reload  |=(all/(list term) (poke-reload-desk %home all))
++  poke-reload-desk                                 ::  reload vanes
  |=  {syd/desk all/(list term)}  =<  abet
  %-  emil
  %-  flop
  %+  turn  all
  =+  top=`path`/(scot %p our)/[syd]/(scot %da now)/arvo
  =+  ark=(arch .^(%cy top))
  =+  van=(~(tap by dir.ark))
  |=  nam/@tas
  =.  nam
    ?.  =(1 (met 3 nam))
      nam
    =+  ^-  zaz/(list {p/knot $~})
        (skim van |=({a/term $~} =(nam (end 3 1 a))))
    ?>  ?=({{@ $~} $~} zaz)
    `term`p.i.zaz
  =+  tip=(end 3 1 nam)
  =+  way=(welp top /[nam])
  =+  fil=(@ .^(%cx (welp way /hoon)))
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
  =+  top=`path`/(scot %p our)/home/(scot %da now)/arvo
  :-  [%flog /reset %vega (weld top `path`/hoon)]
  %+  turn
    ^-  (list {p/@tas q/@tas})
    :~  [%$ %zuse]
        [%a %ames]
        [%b %behn]
        [%c %clay]
        [%d %dill]
        [%e %eyre]
        [%f %ford]
        [%g %gall]
    ==
  |=  {p/@tas q/@tas}
  =+  way=`path`(welp top /[q])
  =+  txt=((hard @) .^(%cx (welp way /hoon)))
  [%flog /reset %veer p way txt]
::
++  poke-deset                                        ::  deset system
  |=  hood-deset  =<  abet
  %-  emil
  %-  flop  ^-  (list card) 
  =+  top=`path`/(scot %p our)/home/(scot %da now)/arvo
  :-  [%flog /deset %vega (weld top `path`/hoon)]
  ~
::
++  poke-will                                         ::  hear certificate
  |=  wil/(unit will)
  ?>  ?=(^ bur)
  ?>  ?=(^ wil)  
  =<  abet
  %-  emil(bur ~)
  :~  [%cash /helm p.u.bur q.u.bur u.wil]
      [%poke /helm [our %hood] %hood-unsync %base (sein our) %kids]
  ==
::
++  poke-verb                                         ::  toggle verbose
  |=  $~  =<  abet
  (emit %flog /helm %verb ~)
::
++  take-onto                                         ::  result of %conf
  |=  saw/(each suss tang)  =<  abet
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
