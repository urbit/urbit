::                                                      ::  ::
::::  /hoon/helm/hood/lib                               ::  ::
  ::                                                    ::  ::
/?    310                                               ::  version
/-    sole, hall
/+    pill
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
|%                                                      ::  ::
++  part  {$helm $0 pith}                               ::  helm state
++  pith                                                ::  helm content
  $:  hoc/(map bone session)                            ::  consoles
  ==                                                    ::
++  session                                             ::
  $:  say/sole-share:sole                               ::  console state
      mud/(unit (sole-dialog:sole @ud))                 ::  console dialog
      mass-timer/{way/wire nex/@da tim/@dr}
  ==                                                    ::
::                                                      ::  ::
::::                                                    ::  ::
  ::                                                    ::  ::
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
|:  $:{bowl:gall part}                                  ::  main helm work
=+  sez=(fall (~(get by hoc) ost) $:session)
=>  |%                                                  ::  arvo structures
    ++  card                                            ::
      $%  [%bonk wire ~]                                ::
          {$conf wire dock $load ship term}             ::
          {$flog wire flog:dill}                        ::
          [%mint wire our=ship p=ship q=safe:rights:jael]
          {$nuke wire ship}                             ::
          {$serv wire ?(desk beam)}                     ::
          {$poke wire dock pear}                        ::
          {$rest wire @da}                              ::
          {$wait wire @da}                              ::
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
=+  moz=((list move))
|%
++  abet                                              ::  resolve
  [(flop moz) %_(+<+.$ hoc (~(put by hoc) ost sez))]
::
++  emit  |=(card %_(+> moz [[ost +<] moz]))          ::  return card
++  emil                                              ::  return cards
  |=  (list card)
  ^+  +>
  ?~(+< +> $(+< t.+<, +> (emit i.+<)))
::
++  poke-rekey                                        ::  rotate private keys
  |=  des=@t
  =/  sed=(unit seed:able:jael)
    %+  biff
      (bind (slaw %uw des) cue)
    (soft seed:able:jael)
  =<  abet
  ?~  sed
    ~&  %invalid-private-key
    +>.$
  ?.  =(our who.u.sed)
    ~&  [%wrong-private-key-ship who.u.sed]
    +>.$
  =/  lyf=life  .^(@ud j+/(scot %p our)/life/(scot %da now)/(scot %p our))
  ?.  =(+(lyf) lyf.u.sed)
    ~&  [%wrong-private-key-life expected=+(lyf) actual=lyf.u.sed]
    +>.$
  ::  our new private key, as a +tree of +rite
  ::
  =/  rit  (sy [%jewel (my [lyf.u.sed key.u.sed] ~)] ~)
  (emit %mint / our our rit)
::
++  poke-nuke                                         ::  initialize
  |=  him/ship  =<  abet
  (emit %nuke /helm him)
::
++  poke-mass
  |=  ~  =<  abet
  (emit %flog /heft %crud %hax-heft ~)
::
++  poke-automass
  |=  recur=@dr
  =.  mass-timer.sez
    [/helm/automass (add now recur) recur]
  abet:(emit %wait way.mass-timer.sez nex.mass-timer.sez)
::
++  poke-cancel-automass
  |=  ~
  abet:(emit %rest way.mass-timer.sez nex.mass-timer.sez)
::
++  poke-bonk
  |=  ~
  ~&  .^((unit @da) %a /(scot %p our)/time/(scot %da now)/(scot %p our))
  %-  %-  slog  :_  ~  .^(tank %b /(scot %p our)/timers/(scot %da now))
  abet:(emit %bonk /bonk ~)
::
++  take-wake-automass
  |=  [way=wire ~]
  =.  nex.mass-timer.sez  (add now tim.mass-timer.sez)
  =<  abet
  %-  emil
  :~  [%flog /heft %crud %hax-heft ~]
      [%wait way.mass-timer.sez nex.mass-timer.sez]
  ==
::
++  poke-send-hi
  |=  {her/ship mes/(unit tape)}  =<  abet
  %^  emit  %poke  /helm/hi/(scot %p her)
  [[her %hood] %helm-hi ?~(mes '' (crip u.mes))]
::
++  poke-send-ask
  |=  mel/cord
  abet
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
  ?>  ?=({@t ~} pax)
  (emit %flog ~ %text "hi {(trip i.pax)} {?~(cop "" "un")}successful")
::
++  coup-ask
  |=  {pax/path cop/(unit tang)}  =<  abet
  ?>  ?=({@t ~} pax)
  (emit %flog ~ %text "ask {<src>} {?~(cop "" "un")}successful")
::
++  poke-reload  |=(all/(list term) (poke-reload-desk %home all))
++  poke-reload-desk                                 ::  reload vanes
  |:  $:{syd/desk all/(list term)}  =<  abet
  %-  emil
  %-  flop
  %+  turn  all
  =+  top=`path`/(scot %p our)/[syd]/(scot %da now)
  =/  van/(list {term ~})
    :-  zus=[%zuse ~]
    ~(tap by dir:.^(arch %cy (welp top /sys/vane)))
  |=  nam/@tas
    =.  nam
    ?.  =(1 (met 3 nam))
      nam
    =+  ^-  zaz/(list {p/knot ~})
        (skim van |=({a/term ~} =(nam (end 3 1 a))))
    ?>  ?=({{@ ~} ~} zaz)
    `term`p.i.zaz
  =+  tip=(end 3 1 nam)
  =+  zus==('z' tip)
  =+  way=?:(zus (welp top /sys/[nam]) (welp top /sys/vane/[nam]))
  =+  fil=.^(@ %cx (welp way /hoon))
  [%flog /reload [%veer ?:(=('z' tip) %$ tip) way fil]]
::  +poke-reset:  send %lyra to initiate kernel upgrade
::
::    And reinstall %zuse and the vanes with %veer.
::    Trigger with |reset.
::
++  poke-reset
  |=  hood-reset
  =<  abet
  %-  emil  %-  flop
  ^-  (list card)
  =/  top=path  /(scot %p our)/home/(scot %da now)/sys
  =/  hun  .^(@ %cx (welp top /hoon/hoon))
  =/  arv  .^(@ %cx (welp top /arvo/hoon))
  :-  [%flog /reset [%lyra `@t`hun `@t`arv]]
  %+  turn
    (module-ova:pill top)
  |=(a=[wire flog:dill] [%flog a])
::
++  poke-verb                                         ::  toggle verbose
  |=  ~  =<  abet
  (emit %flog /helm %verb ~)
::
++  take-onto                                         ::  result of %conf
  |=  saw/(each suss:gall tang)  =<  abet
  %-  emit
  ?-   -.saw
    %|  [%flog ~ %crud %onto `tang`p.saw]
    %&  [%flog ~ %text "<{<p.saw>}>"]
  ==
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
