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
++  bait                                                ::  analysis state
  $:  now/@da                                           ::  date
      eny/@uvJ                                          ::  entropy
      sky/roof                                          ::  namespace
  ==                                                    ::
++  card  {p/@tas q/*}                                  ::  tagged event
++  case                                                ::  version
  $%  {$da p/@da}                                       ::  date
      {$tas p/@tas}                                     ::  label
      {$ud p/@ud}                                       ::  sequence
  ==                                                    ::
++  chip                                                ::  standard identity
  $?  $giv                                              ::  given name
      $fam                                              ::  surname
      $had                                              ::  fictitious name
      $mid                                              ::  middle name
      $gen                                              ::  generational suffix
  ==                                                    ::
++  desk  @tas                                          ::  ship desk case spur
++  dope  (pair @tas @t)                                ::  term/unicode pair
++  duct  (list wire)                                   ::  causal history
++  ovum  (pair wire card)                              ::  input or output
++  plum  (pair term noun)                              ::  deep file
++  ruby  @pG                                           ::  64-bit passcode
++  roof                                                ::  namespace
  $-  $:  lyc/(unit (set ship))                         ::  leakset
          car/term                                      ::  perspective
          bem/beam                                      ::  path
      ==                                                ::
  %-  unit                                              ::  ~: unknown
  %-  unit                                              ::  ~ ~: invalid
  cask                                                  ::  marked vase
::                                                      ::
++  ship  @p                                            ::  network identity
++  wire  path                                          ::  cause
--  =>
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (2) state molds
::                                                      ::  ::
|% 
++  evil                                                ::  evolvable state
  |*  {span/_span twig/_twig vase/_vase}                ::  injected molds
  |%                                                    ::
  ++  boot  (pair (unit hoof) hoof)                     ::  hoon/arvo boot src
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
            eny/@uvJ                                    ::  512-bit entropy
            yor/vase                                    ::  %york, vane models
            zus/vase                                    ::  %zuse, user lib
            van/(map term (pair term vase))             ::  vanes
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
++  vane                                                ::  kernel module
  $_  ^?                                                ::  purely decorative
  |%                                                    ::
  ++  load  $-(* _+>)                                   ::  reload
  ++  stay  **                                          ::  preserve
  ++  work                                              ::  operate
    |_  bait
    ++  doze  *@da
    ++  scry  roof
    ++  spin
      |_  $:  ::  hen: cause of this 
              ::  moz: 
              ::
              hen/duct
              moz/(list move)
          ==
      ++  call
        |=  fav/card
        ^+  +>
        !!
      ++  take
        |=  {tea/wire fav/card}
        ^+  +>
        !!
      --
    --
  --
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
  ::                                                  ::  ++va:le
  ++  va                                              ::  vane engine
    |_  $:  ::  lay: short name, eg %a
            ::  way: long name, eg %ames
            ::  vax: vane or vane builder
            ::  
            lay/term
            way/term
            vax/vase
        ==
    ::                                                ::  ++va-abet:va:le
    ++  va-abet                                       ::  resolve
      ^+  ..va
      ..va(van.mal (~(put by van.mal) lay [way vax]))
    ::                                                ::  ++va-abut:va:le
    ++  va-abut                                       ::  deploy
      ^+  +>
      +>(+< [lay (~(got by van.mal) lay))
    ::                                                ::  ++va-apex:va:le
    ++  va-apex                                       ::  boot/reboot
      |=  src/hoof
      ^+  +>
      =+  bun=(~(get by van.mal) lay)
      ?~  bun
        (va-create src)
      (va-update(vax u.bun) src)
    ::                                                ::  ++va-active:va:le
    ++  va-active                                     ::  make activated vane
      |=  {ski
    ::                                                ::  ++va-create:va:le 
    ++  va-create                                     ::  compile new vase
      |=  src/hoof
      ^+  +>
      ::  no existing vase; compile new vase
      ::
      ~&  [%vase-compile lay way `@p`(mug src)]
      =.  vax  (slam zus.mal (ream src))
      ?:  off  
        ::  not booted yet, wait to activate
        +>
      ::  already booted; activate now
      ::
      +>(vax (slam vax !>(orb.rep)))
    ::                                                ::  ++va-settle:va:le
    ++  va-settle                                     ::  initialize ship
      ^+  +>
      .(vax (slap vax !>(orb.rep)))
    ::                                                ::  ++va-update
    ++  va-update                                     ::  replace existing
      |=  src/hoof
      ?:  off
        ::  replacing unbooted, weird
        ::
        (va-create src)
      =+  nex=(slap (slam zus.mal (ream src)) !>(orb.rep))
      =+  out=(slap 
    --
  ::                                                  ::  ++vale:le
  ++  vale                                            ::  load existing vane
    |=  lay/term
    ~(. va lay (~(got by van.mal lay)))
  ::                                                  ::  ++warp:le
  ++  warp                                            ::  arvo effect
    |=  {hen/duct wap/wasp}
    ^+  +>
    ?+  -.wap  !!
      $what  (what hen p.wap)
      $whom  (whom hen p.wap q.wap r.wap)
    ==
  ::                                                  ::  ++wilt:le
  ++  wilt                                            ::  deep file as source
    |=  pet/plum
    ^-  hoof
    ?>(?=({$hoon @tas} pet) +.pet)
  ::                                                  ::  ++wise:le
  ++  wise                                            ::  load/reload vane
    |=  {lay/term way/term src/hoof}
    ^+  +>
    =+  bun=(~(get by van) lay)
    ?~  bun
      ::
      ::  no existing vase; compile new vase
      ::
      ~&  [%vase-compile lay way `@p`(mug src)]
      =+  vax=(slam zus.mal (ream src))
      ::
      ::  if we are already live, activate new vase
      ::
  ::                                                  ::  ++what:le
  ++  what                                            ::  write deep storage
    |=  {hen/duct fal/(list (pair path plum))}
    ^+  +>
    ::  dev: collated `fal`
    ::
    =+  =|  dev
          $:  ::  use: non-system files
              ::  new: system installs 
              ::  rep: system replacements
              ::
              use/(map path plum)
              new/(map path plum)
              rez/(map path plum)
          ==
        |-  ^+  dev
        ?~  fal  dev
        ::  
        ::  pax: path of this file
        ::  pet: value of this file
        ::
        =+  [pax pet]=[p q]:i.fal
        =.  fal  t.fal
        ::
        ::  old: current value in deep storage
        ::
        =+  old=(~(get by fat.rep) pax)
        ::
        ::  ignore unchanged data
        ::
        ?:  =(old `pet)  $
        ::
        ::  classify as user, system install or replacement
        ::
        ?.  ?=({$sys *} pax)
          $(use (~(put by use.dev) pax pet)
        ?~  old
          $(new (~(put by new.dev) pax pet))
        $(rez (~(put by rez.dev) pax pet))
    ::
    ::  adopt user changes, which have no systems impact
    ::
    =.  fat.rep  (~(uni by fat.rep) use.rez)
    ::
    ::  but: kernel reboot operation, if any
    ::
    =/  but
      ^-  (unit boot)
      =/  hun=(~(get by rez.dev) /sys/hoon)
      =/  arv=(~(get by rez.dev) /sys/arvo)
      ?~  hun
        ?~  arv  ~
        ::
        ::  light reboot, arvo only
        ::
        `[~ (wilt u.arv)]
      ::
      ::  heavy reboot, hoon and arvo
      ::
      `[`(wilt hun) (wilt q:?^(arv u.arv (~(got by fat.rep) /sys/arvo)))]
    ?^  but
      ::  stop working and set up reboot
      ::
      %=  +>.$
        ::  set boot hook for termination
        ::
        but.gut  ?>(=(~ but.gut) but)
        ::
        ::  execute write after reboot
        ::
        run.rep  ::  syt: all systems changes
                 ::
                 =*  syt  (~(tap by (~(uni by rez.dev) new.dev)))
                 :_  run.rep
                 `move`[hen %give %& !>([%what syt])]
        ::
        ::  delete reboot source files from deep
        ::  storage, so install causes vane upgrade,
        ::  and *does not* cause repeat kernel upgrade.
        ::
        fat.rep  ?~  p.but  fat.rep
                 (~(del by (~(del by fat.rep) /sys/hoon) /sys/arvo))
      ==
    ::  keep working after vane upgrades
    ::
    =<  work
    ::
    ::  job: plan for upgrading 
    ::
    =/  job
      ^-  $:  yor/(unit hoof)
              zus/(unit hoof)
              van/(list (trel @tas @tas hoof))
          ==
      =-  [yor zus van]
      ::  yor: reload shared structures
      ::  zus: reload shared library
      ::
      =/  yor  (bind (~(get by rez.dev) /sys/york) wilt)
      =/  zus  (bind (~(get by rez.dev) /sys/zuse) wilt)
      ::
      ::  %york is the subject of %zuse
      ::
      =.  zus  ?^(zus zus ?~(yor ~ `(wilt (~(get by fat.rep) /sys/zuse))))
      ::
      ::  van: all vane upgrades, as [initial name source]
      ::
      =/  van
        ::  zyr: all system file replacements
        ::
        =/  rad  
        =/  zyr  (~(tap by rez.dev))
        |-  ^-  (list (pair @tas hoof))
        ?^  zyr
          ::  mor: process rest of `zyr`
          ::
          =/  mor  $(zyr t.zyr)
          ?.  ?=({$sys $van @tas $~} p.i.zyr) 
            mor
          ::
          ::  replaced vane in `/sys/vane/*/[nam]`
          ::
          =*  nam  `term`i.t.t.p.i.zyr
          :_(mor [(end 3 1 nam) nam (wilt q.i.zyr)])
        ::
        ::  reload compiled vanes if needed
        ::
        ?.  |((~(has by new.dev) /sys/hoon) ?=(^ zus))
          ::
          ::  we didn't replace compiler, %york or %zuse
          ~
        ::
        ::  get source code for current running vanes
        ::
        %+  turn  (~(tap by van.mal))
        |=  {lay/term way/term vax/vase}
        [lay way (wilt (~(got by fat.rep) [%sys %van way ~]))
    ::
    ::  upgrade %york, vane shared structures
    ::
    =>  ?~  yor  .
        %=    .
           yor.mal  ~&  [%york-boot `@p`(mug u.yor.job)]
                     (slap !>(..arms) (ream u.yor.job))
        ==
    ::
    ::  upgrade %zuse, vane shared libraries
    ::
    =>  ?~  zus  .
        %=    .
           zus.mal  ~&  [%zuse-boot `@p`(mug u.zus.job)]
                     (slap yor.mal (ream u.zus.job))
        ==
    ::
    ::  upgrade all indicated vanes
    ::
    |-  ^+  +>.^$
    ?~  van.job  +>.^$
    ~&  [%vane-boot p.i.van.job `@p`(mug q.i.van.job)]
    $(van.job t.van.job, +>.^$ (wise i.van.job))
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
