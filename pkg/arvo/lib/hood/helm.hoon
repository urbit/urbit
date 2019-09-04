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
=+  sez=(~(gut by hoc) ost $:session)
=>  |%                                                  ::  arvo structures
    ++  card                                            ::
      $%  [%bonk wire ~]                                ::
          {$conf wire dock ship term}                   ::
          {$flog wire flog:dill}                        ::
          {$nuke wire ship}                             ::
          [%serve wire binding:eyre generator:eyre]     ::
          {$poke wire dock pear}                        ::
          {$rest wire @da}                              ::
          {$wait wire @da}                              ::
          {$rekey wire life ring}                       ::
          {$moon wire ship udiff:point:able:jael}       ::
      ==                                                ::
    ++  move  (pair bone card)                          ::  user-level move
    ++  pear                                            ::  poke fruit
      $%  {$hood-unsync desk ship desk}                 ::
          {$helm-hi cord}                               ::
          {$drum-start well:gall}                       ::
          {$hall-action action:hall}                    ::
      ==                                                ::
    --
=+  moz=((list move))
=|  moi=(list move:agent:mall)
|%
++  abet                                              ::  resolve
  [(flop moz) %_(+<+.$ hoc (~(put by hoc) ost sez))]
::
++  abei
  [(flop moi) %_(+<+.$ hoc (~(put by hoc) ost sez))]
::
++  emit  |=(card %_(+> moz [[ost +<] moz]))          ::  return card
++  emii
  |=  (wind internal-note:mall internal-gift:mall)
  %_(+> moi [[ost +<] moi])
::
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
  (emit %rekey / lyf.u.sed key.u.sed)
::
++  poke-moon                                        ::  rotate moon keys
  |=  sed=(unit [=ship =udiff:point:able:jael])
  =<  abet
  ?~  sed
    +>.$
  (emit %moon / u.sed)
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
  |=  [way=wire error=(unit tang)]
  ?^  error
    %-  (slog u.error)
    ~&  %helm-wake-automass-fail
    abet
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
  =<  abei
  %^  emii  %pass  /di
  :+  %meta  %d
  !>  [%flog %text "< {<src>}: atom: {len} bytes, mug {gum}"]
::
++  coup-hi
  |=  {pax/path cop/(unit tang)}  =<  abet
  ?>  ?=({@t ~} pax)
  (emit %flog ~ %text "hi {(trip i.pax)} {?~(cop "" "un")}successful")
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
++  poke-serve
  |=  [=binding:eyre =generator:eyre]  =<  abet
  (emit %serve /helm/serv binding generator)
::
++  take-bound
  |=  [wir=wire success=? binding=binding:eyre]  =<  abet
  (emit %flog ~ %text "bound: {<success>}")
::
--
