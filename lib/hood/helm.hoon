::                                                      ::  ::
::::  /hoon/helm/hood/lib                               ::  ::
  ::                                                    ::  ::
/?    310                                               ::  version
/-    sole, hall
[. sole]
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%                                                      ::  ::
++  part  {$helm $0 pith}                               ::  helm state
++  pith                                                ::  helm content
  $:  bur/(unit (pair ship mace:ames))                  ::  requesting ticket
      hoc/(map bone session)                            ::  consoles
  ==                                                    ::
++  session                                             ::
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
++  hood-nuke                                           ::  block/unblock
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
|=  {bowl:gall part}                                   ::  main helm work
=+  sez=(fall (~(get by hoc) ost) *session)
=>  |%                                                  ::  arvo structures
    ++  card                                            ::
      $%  {$cash wire p/@p q/buck:ames}                 ::
          {$conf wire dock $load ship term}             ::
          {$flog wire flog:dill}                        ::
          {$funk wire @p @p @}                          ::
          {$nuke wire ship}                             ::
          {$serv wire ?(desk beam)}                     ::
          {$poke wire dock pear}                        ::
          {$want wire sock path *}                      ::  send message
      ==                                                ::
    ++  move  (pair bone card)                          ::  user-level move
    ++  pear                                            ::  poke fruit
      $%  {$hood-unsync desk ship desk}                 ::
          {$ask-mail cord}                              ::
          {$helm-hi cord}                               ::
          {$drum-start well:gall}                       ::
          {$hall-action action:hall}                    ::
      ==                                                ::
    --
|_  moz/(list move)
++  abet                                              ::  resolve
  [(flop moz) %_(+<+.$ hoc (~(put by hoc) ost sez))]
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
  ~&  [%poke-begin our his]
  =+  buz=(shaz :(mix (jam ges) eny))
  =+  loy=(pit:nu:crub:crypto 512 buz)
  %-  emit(bur `[his [0 sec:ex:loy]~])
  [%want /helm/ticket [our (sein:title his)] /a/ta his tic ges pub:ex:loy]
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
++  poke-nuke                                         ::  initialize
  |=  him/ship  =<  abet
  (emit %nuke /helm him)
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
++  poke-hi
  |=  mes/@t
  ~|  %poke-hi-fail
  ?:  =(%fail mes)
    ~&  %poke-hi-fail
    !!
  abet:(emit %flog /di %text "< {<src>}: {(trip mes)}")
::
++  poke-atom
  |=  ato/@
  =+  len=(scow %ud (met 3 ato))
  =+  gum=(scow %p (mug ato))
  =<  abet
  (emit %flog /di %text "< {<src>}: atom: {len} bytes, mug {gum}")
::
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
    ~(tap by dir:.^(arch %cy (welp top /sys/vane)))
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
++  poke-reset                                        ::  reset system
  |=  hood-reset  =<  abet
  %-  emil
  %-  flop  ^-  (list card)
  =+  top=`path`/(scot %p our)/home/(scot %da now)/sys
  :-  [%flog /reset %vega (weld top /hoon) (weld top /arvo)]
  %+  turn
    ^-  (list {p/@tas q/path})
    :~  [%$ /zuse]
        [%a /vane/ames]
        [%b /vane/behn]
        [%c /vane/clay]
        [%d /vane/dill]
        [%e /vane/eyre]
        [%f /vane/ford]
        [%g /vane/gall]
    ==
  |=  {p/@tas q/path}
  =+  way=`path`(welp top q)
  =+  txt=.^(@ %cx (welp way /hoon))
  [%flog /reset %veer p way txt]
::
++  poke-meset                                        ::  reset system (new)
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
++  poke-will                                         ::  hear certificate
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
++  take-woot                                         ::  result of %want
  |=  {way/wire her/ship cop/coop}  =<  abet
  (emit %flog ~ %text "woot: {<[way cop]>}")
::
++  poke-tlon-init-stream
  ::  creates stream channel and makes it pull from
  ::  urbit-meta on {met}.
  |=  met/ship  =<  abet
  %-  emil
  %-  flop
  :~  ^-  card
      :^  %poke  /helm/web/stream/create  [our %hall]
      :-  %hall-action
      :-  %create
      [%stream 'stream relay channel' %channel]
    ::
      :^  %poke  /helm/web/stream/filter  [our %hall]
      :-  %hall-action
      :-  %filter
      [%stream | |]
    ::
      :^  %poke  /helm/web/stream/source  [our %hall]
      :-  %hall-action
      :-  %source
      [%stream & [[[met %urbit-meta] `[da+(sub now ~d1) ~]] ~ ~]]
  ==
::
++  poke-tlon-add-fora
  ::  makes the local urbit-meta pull from {for}'s fora
  ::  notification channels.
  |=  for/ship  =<  abet
  %-  emil
  :~  :^  %poke  /helm/web/fora/source  [our %hall]
      :-  %hall-action
      :-  %source
      [%urbit-meta & [[[for %fora-posts] `[da+now ~]] ~ ~]]
    ::
      :^  %poke  /helm/web/fora/source  [our %hall]
      :-  %hall-action
      :-  %source
      [%urbit-meta & [[[for %fora-comments] `[da+now ~]] ~ ~]]
  ==
::
++  poke-tlon-add-stream
  ::  makes the local urbit-meta pull from {web}'s stream.
  |=  web/ship  =<  abet
  %-  emit
  :^  %poke  /helm/web/stream/source  [our %hall]
  :+  %hall-action  %source
  [%urbit-meta & [[[web %stream] `[da+now ~]] ~ ~]]
--
