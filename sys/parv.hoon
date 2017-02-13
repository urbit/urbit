!:                                                      ::  /sys/arvo
::                                                      ::  !%reference/2
::  %arvo: arvo microkernel.
::
=<  ::  this lifecycle wrapper makes the arvo door
    ::  (multi-armed core) look like a gate (function
    ::  or single-armed core), to fit urbit's formal
    ::  lifecycle function.  a practical interpreter 
    ::  can ignore it.
    ::
    |=  {now/@da ovo/ovum}
    ^+  .
    ~>  %slog.[0 leaf+"arvo-event"]
    .(+> +:(poke now ovo))
=>  
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (1) public molds
::                                                      ::  ::
|%
++  arms  (map chip dope)                               ::  stated identity
++  card  {p/@tas q/*}                                  ::  tagged event
++  chip                                                ::  standard identity
  $?  $giv                                              ::  given name
      $fam                                              ::  surname
      $had                                              ::  fictitious name
      $mid                                              ::  middle name
      $gen                                              ::  generational suffix
  ==                                                    ::
++  dope  (pair @tas @t)                                ::  term/unicode pair
++  duct  (list wire)                                   ::  causal history
++  ovum  (pair wire card)                              ::  input or output
++  ruby  @pG                                           ::  64-bit passcode
++  wire  path                                          ::  cause
--  =>
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (2) state molds
::                                                      ::  ::
|% 
++  evil                                                ::  evolvable state
  |*  {span/_span twig/_twig vase/_vase}                ::  injected molds
  |%                                                    ::
  ++  boot  (pair hoof (unit hoof))                     ::  arvo/hoon boot src
  ++  hoof  @t                                          ::  hoon source file
  ++  mall                                              ::  any arvo version
    $?  {$293 mast}                                     ::  kelvin 293, current
    ==                                                  ::
  ++  mast                                              ::  system state
    $:  $=  gut                                         ::  abdomen
        $:  run/(list move)                             ::  worklist
            out/(list ovum)                             ::  unix output
            but/(unit boot)                             ::  reboot 
        ==                                              ::
        $=  hax                                         ::  thorax
        $:  sac/worm                                    ::  compiler cache
            kit/toys                                    ::  common nouns
        ==                                              ::
        $=  bug                                         ::  insect brain
        $:  noc/@ta                                     ::  process nonce
            ver/(qual @tas @ud @ud @ud)                 ::  vendor/version
        ==                                              ::
        $=  mal                                         ::  mammal brain
        $:  off/?                                       ::  not yet booted
            lac/?                                       ::  not verbose
            yor/vase                                    ::  %york, vane models
            zus/vase                                    ::  %zuse, user lib
            van/(map term vase)                         ::  vanes
        ==                                              ::
        $=  rep                                         ::  reptile brain
        $:  orb/@p                                      ::  ship
            nym/arms                                    ::  name information
            roy/(map @ud ruby)                          ::  start secrets
            fat/(map path (pair term noun))             ::  boot filesystem
    ==  ==                                              ::
  ++  mill  (each vase milo)                            ::  vase or metavase
  ++  milo  {p/* q/*}                                   ::  untyped metavase
  ++  move  (pair duct part)                            ::  vane move
  ++  part                                              ::  arvo vane move
    $%  {$give p/mill}                                  ::  vane "return"
        {$pass p/wire q/(pair term mill)}               ::  vane "call"
    ==                                                  ::
  ++  toys                                              ::  reflexive constants
    $:  typ/span                                        ::  -:!>(*span)
        duc/span                                        ::  -:!>(*duct)
        pah/span                                        ::  -:!>(*path)
        mev/span                                        ::  -:!>([%meta *vase])
    ==                                                  ::
  ++  worm                                              ::  compiler cache
    $:  nes/(set ^)                                     ::  ++nest
        pay/(map (pair span twig) span)                 ::  ++play
        mit/(map (pair span twig) (pair span nock))     ::  ++mint
    ==                                                  ::
  --                                                    ::
++  live  (evil)                                        ::  modern molds
++  vile  (evil typo twit vise)                         ::  old molds
++  wasp                                                ::  arvo effect
  $%  {$walk ~}                                         ::  finish mammal brain
      {$what p/(list (pair path (pair term noun)))}     ::  put reptile files
      {$whom p/@p q/arms r/(map @ud ruby)}              ::  put reptile identity
      {$woke ~}                                         ::  finish booting
  ==                                                    ::
--  =>
|%
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (2) engines
::                                                      ::  ::
|%
::                                                      ::  ++le
++  le                                                  ::  deep engine
  =+  [now=*@da mast:live]
  =*  ::  
      ::  sys: system state
      ::
      sys  ->
  |%
  ::                                                    ::  ++abet:le
  ++  abet                                              ::  complete cycle
    ^-  {(pair (unit (pair @t @t)) (list move)) _sys}
    :-  [but.gut flop 
    [[but.gut (flop out.gut) sys(out.gut ~)]
  ::                                                    ::  ++base:le
  ++  base                                              ::  upgrade vane
    |=  $:  ::  lal: vane name `%gall`
            ::  src: vane source
            ::
            lal/term 
            src/
        ==
    ::  lay: name prefix `%g`
    ::
    =+  lay=(end 3 1 lal)
  ::                                                    ::  ++boss:le
  ++  boss                                              ::  apply upgrades
    |=  $:  ::  yor: sys/york upgrade
            ::  zus: sys/zuse upgrade
            ::  van: all vane upgrades
            ::
            $:  yor/(unit hoof)
                zus/(unit hoof)
            ==
            van/(map term hoof)
        ==
    ::  bas: vase 
    ::
    =+  bas=!>(..arms)
    ::
    ::  
    ::
  ::                                                    ::  ++emit:le
  ++  emit                                              ::  emit move
    |=  mov/move  
    +>(run.gut [mov run.gut]) 
  ::                                                    ::  ++pike:le
  ++  pike                                              ::  event to %pass
    |=  $:  ::  lay: event route
            ::  now: date
            ::  ovo: input ovum
            ::
            lay/@tas
            now/@da
            ovo/ovum
        ==
    ^+  +>
    ::  print event if in verbose mode
    ::
    ~?  &(!lac.mal !=(%belt -.q.ovo))  [%unix -.q.ovo p.ovo]
    ::
    ::  vax: card as vase
    ::
    =^  vax  +>  (open q.ovo)
    ::
    ::  hen: fundamental cause (unix input channel)
    ::  tea: local cause (unix i/o)
    ::  mov: action (pass into event route)
    ::
    =*  hen  `duct`[p.ovo ~]
    =*  tea  `wire`[%$ %unix ~]
    =*  mov  `move`[%pass tea lay %& vax]
    ::
    ::  push move on stack
    ::
    (emit mov)
  ::                                                    ::  ++open:le
  ++  open                                              ::  input card to move
    |=  fav/card
    ^-  {vase _+>}
    ?<  off.mal
    ::
    ::  gat: mold for correct unix task
    ::  vax: molded card
    ::
    =^  gat  sac.gut  (~(slap wa sac.gut) bud.mal [%limb %unix-task])
    =/  vax  (slam gat [%noun fav])
    ~|  [%le-open -.fav]
    ?>  =(fav q.vax)
    [vax +>.$]
  ::                                                    ::  ++poke:le
  ++  poke                                              ::  event from unix
    |=  $:  ::  now: event date
            ::  ovo: event
            ::
            now/@da
            ovo/ovum
        ==
    ^+  sys
    ~|  [%poke -.ovo]
    ::
    ::  the event is either vane input or an arvo action (wasp).
    ::  we default to treating it as a wasp.
    ::
    ::  XX: this logic will be directed in the event structure itself.
    ::
    ?+  -.ovo  
      =/  wap  ((hard wasp) ovo)
      =*  tea  `wire`[%$ %init ~]
      =*  hen  `duct`[tea [p.ovo ~]]
      =*  mov  `move`[hen %give %& !>(wap)]
      (emit mov)
    ::
      $belt  (pike %d now ovo)
      $blew  (pike %d now ovo)
      $born  (pike %e now ovo)
      $hail  (pike %d now ovo)
      $hear  (pike %a now ovo)
      $hook  (pike %d now ovo)
      $into  (pike %c now ovo)
      $they  (pike %e now ovo)
      $this  (pike %e now ovo)
      $thus  (pike %e now ovo)
    ==
  ::                                                    ::  ++work:le
  ++  work                                              ::  main loop
    =*  ken  +
    ^+  ken
    ::
    ::  terminate when stack is empty
    ::
    ?~  run.gut  ken
    ::
    ::  mov: top move on stack
    ::  hen: cause of move
    ::  act: action in move
    ::
    =*  mov  `move`i.run.gut
    =*  hen  `duct`p.mov
    =*  egg  `part`q.mov
    ::
    ::  pop top move off stack
    ::
    =.  run.gut  t.run.gut
    ::
    ::  interpret top move
    ::
    ?-    -.egg
    ::
    ::  %give: event return
    ::
        $give
      ::
      ::  the duct can't be empty
      ::
      ?>  ?=(^ p.mov)
      ::
      ::  tea: top wire on duct
      ::  hen: rest of duct
      ::
      =/  tea  i.p.mov
      =*  hen  t.p.mov
      ::
      ::  route gift by wire
      ::
      ?:  ?=({%$ *} tea)
        ::
        ::  gift returned on arvo wire
        ::
        ?:  ?=({%unix $~} t.tea)
          ::
          ::  gift returned to unix i/o
          ::
          (unix hen XXp.egg)
        ?>  ?=({%arvo $~} t.tea)
        ::
        ::  gift returned to arvo control
        ::
        (wasp hen XXp.egg)
      ::
      ::  gift returned to calling vane
      ::
      ?>  ?=({@tas *} tea)
      (read hen i.tea t.tea p.egg)
    ::
    ::  %pass: event call
    ::
        $pass
      (send [p.egg hen] p.q.egg q.egg)
    ==
    ::                                                  ::  ++buzz:le
    ++  wasp                                            ::  arvo effect
      |=  {hen/duct wap/wasp}
      ^+  +>
      ?+    -.wap  !!
      ::
      ::  $what: install boot files in reptile brain
      ::
          $what
        ::
        ::  we are rebooting if hoon or arvo change
        ::
        =/  but
            ^-  (unit boot)
            =/  new-hoon  (~(get by p.wap) /sys/hoon)
            =/  new-arvo  (~(get by p.wap) /sys/arvo)
            =*  old-hoon  (~(get by fat.rep) /sys/hoon)
            =*  old-arvo  (~(get by fat.rep) /sys/arvo)
            =.  new-hoon  ?~  new-hoon  ~
                          =+  old=old-hoon
                          ?~  old  ~
                          ?:(=(old new-hoon) ~ new-hoon)
            =.  new-arvo  ?^  new-hoon
                          ?^(new-arvo new-arvo old-arvo)
                          ?~  new-arvo  ~
                          =+  old=old-arvo
                          ?~  old  ~
                          ?:(=(old new-arvo) ~ new-arvo)
            ?~  new-arvo  ~
            `[u.new-arvo new-hoon]
        ::
        ::  if rebooting, set boot trigger and re
        ::
        ?^
        =|  act  $: ::  sys: hoon files in sys
                    ::  van: hoon files in sys/van
                    ::
                    sys/(map term hoof)
                    van/(map term hoof)
                 ==
        |-  ^+  +>.^$
        ?~  p.wap  (boss sys van)
        ?:  =(b


      ::
      ::  $whom: install identity information and complete boot process
      ::
          $whom
        =*  mol  $:  orb/@p
                     nym/arms
                     roy/(map @ud ruby)
                 ==
        =+  dat=((hard mol) +.ovo)
        =:  orb.rep  orb.dat
            nym.rep  nym.dat
            roy.rep  (~(uni by roy.rep) roy.rep roy.dat)
          ==
        !!
      ==
    ::                                                  ::  ++we-gift:work:le
    ++  we-gift 
      |=  $:  ::  hen: cause
              ::  lay: vane to return to
              ::  hil:
              ::  
              hen/duct
              lay/term
              hil/mill
          ==
      ^+  ken
      !!
    ++  we-wasp
      |=  $:  ::  hen: cause 
              ::  
      ^_
    ++  we-
    --
  --
--
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (3) surface
::                                                      ::  ::
|_  
::                                                      ::  ++come
++  come                                                ::  upgrade from old
  |=  
  ^-  {(list ovum) _+>}
  ~&  %hoon-come
  =^  rey  +>+  (^come +<)
  [rey +>.$]
::
    ++  keep  |=(* (^keep ((hard {@da path}) +<)))      ::  4
    ++  load  |=  {@ (list ovum) pane}                  ::  86
              ^-  {(list ovum) _+>}
              ~&  %hoon-load
              =^  rey  +>+  (^load +<)
              [rey +>.$]
    ++  peek  |=(* (^peek ((hard {@da path}) +<)))      ::  87
    ++  poke  |=  *                                     ::  42
              ^-  {(list ovum) *}
              =>  .(+< ((hard {now/@da ovo/ovum}) +<))
              =^  ova  +>+  (^poke now ovo)
              |-  ^-  {(list ovum) *}
              ?~  ova
                [~ +>.^$]
              ?:  ?=($verb -.q.i.ova)
                $(ova t.ova, lac !lac)
              ?:  ?=($veer -.q.i.ova)
                $(ova t.ova, +>+.^$ (veer now q.i.ova))
              ?:  ?=($velo -.q.i.ova)
                (fall (velo now t.ova ({@ @} +.q.i.ova)) [~ +>.^$])
              ?:  ?=(?($init $veal) -.q.i.ova)
                =+  avo=$(ova t.ova, +>+.^$ (boot (@ +.q.i.ova)))
                [[i.ova -.avo] +.avo]
              ?:  ?=($mass -.q.i.ova)
                =+  avo=$(ova t.ova)
                :_  +.avo
                :_  -.avo
                %=    i.ova
                    q.q
                  :-  %userspace
                  :-  %|
                  :~  hoon+`pit
                      zuse+`mast
                      hoon-cache+`p.niz
                      q.q.i.ova
                      dot+`.
                  ==
                ==
              =+(avo=$(ova t.ova) [[i.ova -.avo] +.avo])
    ++  wish  |=(* (^wish ((hard @ta) +<)))             ::  20

--
