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
    ::  XX  .(+> +:(poke now ovo))    REMOVE BEFORE USE
    .
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
  cage                                                  ::  marked vase
::                                                      ::
++  ship  @p                                            ::  network identity
++  wire  path                                          ::  cause
--  =>
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (2) state molds
::                                                      ::  ::
|% 
++  boot  (pair (unit hoof) hoof)                       ::  hoon/arvo boot src
++  evil                                                ::  evolvable state
  |*  {span/_span twig/_twig vase/_vase}                ::  injected molds
  |%                                                    ::
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
++  hoof  @t                                            ::  hoon source file
++  live  (evil)                                        ::  modern molds
++  vane                                                ::  kernel module
  $_  ^?                                                ::  totally decorative
  |%                                                    ::
  ++  load  $-(* _+>)                                   ::  reload
  ++  stay  **                                          ::  preserve
  ++  plow                                              ::
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
  $%  {$walk $~}                                        ::  finish mammal brain
      {$what p/(list (pair path (pair term noun)))}     ::  put reptile files
      {$whom p/@p q/arms r/(map @ud ruby)}              ::  put reptile identity
      {$woke $~}                                        ::  finish booting
  ==                                                    ::
--
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (2) engines
::                                                      ::  ::
|%
::
::::
  ::
::                                                      ::  ++me
++  me                                                  ::  cached analyses
  =|  {sac/worm kit/toys}
  ::
  ::
  =*  sys  -
  |%
  ::
  ++  abet
  ::
  ++  refine-moves
    |=  vax/vase
    ^-  {(list move:live) _+>+<}
    !!
  ::                                                    ::  ++reduce-card:me
  ++  reduce-card                                       ::
        ::  mex: vase of card
        ::
        mex/vase
    ^-  {
    ^-  (unit (pair mill worm))                       ::
    =^  hip  sac  (~(nell wa sac) p.mex)          ::
    ?.  hip  ~                                        ::  a card is a cell
    ?.  ?=($meta -.q.mex)  `[[%& mex] sac]          ::  ordinary card
    =^  tiv  sac  (~(slot wa sac) 3 mex)          ::
    =^  hip  sac  (~(nell wa p.sew) p.tiv)          ::
    ?.  hip  ~                                        ::  a vase is a cell
    =^  vax  p.sew  (~(slot wa p.sew) 2 tiv)          ::
    =^  hip  p.sew  (~(nest wa p.sew) typ.vil p.vax)  ::
    ?.  hip  ~                                        ::  vase head is span
    %+  biff                                          ::
      =+  mut=(milt q.tiv)                            ::  card span, value
      |-  ^-  (unit (pair milt worm))                 ::
      ?.  ?=({$meta p/* q/milt} q.mut)  `[mut p.sew]  ::  ordinary metacard
      =^  hip  p.sew  (~(nets wa p.sew) mev.vil p.mut)::
      ?.  hip  ~                                      ::  meta-metacard
      $(mut +.q.mut)                                  ::  descend into meta
    |=(a/(pair milt worm) `[[%| p.a] q.a])            ::  milt to mill

  --
::                                                      ::  ++le
++  le                                                  ::  deep engine
  =+  [now=*@da *mast:live]
  =*  ::  
      ::  sys: system state
      ::
      sys  ->
  |%
  ::                                                    ::  ++abet:le
  ++  abet                                              ::  complete cycle
    ^-  {(pair (unit boot) (list ovum)) _sys}
    :-  [but.gut (flop out.gut)]
    sys(out.gut ~, but.gut ~)
  ::                                                    ::  ++emit:le
  ++  emit                                              ::  emit move
    |=  mov/move:live
    +>(run.gut [mov run.gut]) 
  ::                                                    ::  ++pike:le
  ++  pike                                              ::  event to %pass
    |=  $:  ::  way: event route
            ::  now: date
            ::  ovo: input ovum
            ::
            way/@tas
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
    =*  mov  `move:live`[hen %pass tea way %& vax]
    ::
    ::  push move on stack, and work.
    ::
    work:(emit mov)
  ::                                                    ::  ++open:le
  ++  open                                              ::  input card to move
    |=  fav/card
    ^-  {vase _+>}
    ?<  off.mal
    ::
    ::  gat: mold for correct unix task
    ::  vax: molded card
    ::
    =^  gat  sac.hax  (~(slap wa sac.hax) zus.mal [%limb %unix-task])
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
    ^+  +>
    ~|  [%poke -.ovo]
    ?+    -.q.ovo  !!
    ::
    ::  unix input, send to vane
    ::
        $belt  (pike %dill now ovo)
        $blew  (pike %dill now ovo)
        $born  (pike %eyre now ovo)
        $hail  (pike %dill now ovo)
        $hear  (pike %ames now ovo)
        $hook  (pike %dill now ovo)
        $into  (pike %clay now ovo)
        $they  (pike %eyre now ovo)
        $this  (pike %eyre now ovo)
        $thus  (pike %eyre now ovo)
    ::
        ?($what $whom)
      =/  wap  ((hard wasp) ovo)
      =*  tea  `wire`[%$ %init ~]
      =*  hen  `duct`[tea [p.ovo ~]]
      =*  mov  `move:live`[hen %give %& !>(wap)]
      (emit mov)
    ==
  ::                                                  ::  ++va:le
  ++  va                                              ::  vane engine
    |_  $:  ::  way: vane name, eg `%ames`
            ::  vax: vane, or vane builder if `off.mal`
            ::  
            way/term
            vax/vase
        ==
    ::                                                ::  ++va-abet:va:le
    ++  va-abet                                       ::  resolve
      ^+  ..va
      ..va(van.mal (~(put by van.mal) way vax))
    ::                                                ::  ++va-amid:va:le
    ++  va-amid                                       ::  load existing
      |=  way/term
      ^+  +>
      ?<  off.mal
      +>(way way, vax (~(got by van.mal) way))
    ::                                                ::  ++va-abut:va:le
    ++  va-apex                                       ::  boot / reboot
      |=  $:  way/term
              src/hoof
          ==
      ^+  +>
      =.  ^way  way
      =/  bun  (~(get by van.mal) way)
      ?~  bun
        (va-create src)
      (va-update(vax u.bun) src)
    ::                                                ::  ++va-active:va:le
    ++  va-plow                                       ::  activated vane
      |=  bait
      ::
      ::  wok: working vase
      ::
      =/  wok  ^-  vase
        %+  slap
          (slop (slap vax `twig`[%limb %plow]) !>(+<))
        ^-  twig
        :+  %keep
          [[%& 2] ~]
        :~  [[[%& 6] ~] [%$ 3]]
        ==
      |%  
      ::                                              ::  ++doze:va-work:va:le
      ++  doze                                        ::  request wakeup at
        ^-  (unit @da)
        !!
      ::                                              ::  ++scry:va-work:va:le
      ++  scry                                        ::  internal peek
        |=  $:  ::  lyc: set of outputs 
                ::
                lyc/(unit (set ship))
                car/term
                bem/beam
            ==
        ^-  (unit (unit cage))
        !!
      ::                                              ::  ++spin:va-work:va:le
      ++  spin                                        ::  causal action
        |=  hen/duct
        ::
        ::  fox: running vase
        ::
        =/  fox  ^-  vase
          %+  slap
            (slop (slap wok `twig`[%limb %spin]) !>(+<))
          ^-  twig
          :+  %keep
            [[%& 2] ~]
          :~  [[[%& 6] ~] [%$ 3]]
          ==
        |%
        ::                                            ::  ++abet:spin:va-work:
        ++  abet                                      ::  integrate
          ^+  ..va
          !!
        ::                                            ::  ++call:spin:va-work:
        ++  call                                      ::  
          |=  hil/mill
          ^+  +>
          =^  
        ::                                            ::  ++take:spin:va-work:
        ++  take                                      ::
          |=  {tea/wire hil/mill}
          ^+  +>
          !!
        --
      --
    ::                                                ::  ++va-create:va:le 
    ++  va-create                                     ::  compile new vase
      |=  src/hoof
      ^+  +>
      ::  no existing vase; compile new vase
      ::
      ~&  [%vase-compile way `@p`(mug src)]
      =.  vax  (slap zus.mal (ream src))
      ?:  off.mal
        +>
      ::  initialize vane
      ::
      va-settle
    ::                                                ::  ++va-settle:va:le
    ++  va-settle                                     ::  initialize with ship
      ^+  .
      .(vax (slam vax !>(orb.rep)))
    ::                                                ::  ++va-update
    ++  va-update                                     ::  replace existing
      |=  src/hoof
      ^+  +>
      ?:  off.mal
        ::  replacing unbooted, weird but ok
        ::
        (va-create src)
      ::
      ::  out: saved state from old vane
      ::
      =+  out=(slap vax [%limb %stay])
      ::
      ::  replace `vax` with new empty vane
      ::
      =.  +>.$  (va-create src)
      ::
      ::  initialize new vane with old state
      ::
      +>.$(vax (slam (slap vax [%limb %come]) out))
    --
  ::                                                  ::  ++warp:le
  ++  warp                                            ::  arvo effect
    |=  {hen/duct wap/wasp}
    ^+  +>
    ?+  -.wap  !!
      $what  (what hen p.wap)
      $whom  (whom hen p.wap q.wap r.wap)
    ==
  ::                                                  ::  ++whom:le
  ++  whom                                            ::  initialize identity
    |=  {hen/duct our/@p nym/arms sec/(map @ud ruby)}
    ^+  +>
    ::  XX don't forget to keep working
    !!
  ::                                                  ::  ++wile:le
  ++  wile                                            ::  mill as card
    |=  hil/mill
    ^-  card
    ::
    ::  XX actually check card nature
    ::
    ?-  -.hil
      $|  ((hard card) q.p.hil)
      $&  ((hard card) q.p.hil)
    ==
  ::                                                  ::  ++wilt:le
  ++  wilt                                            ::  deep file as source
    |=  pet/plum
    ^-  hoof
    ?>(?=({$hoon @tas} pet) +.pet)
  ::                                                  ::  ++wise:le
  ++  wise                                            ::  load/reload vane
    |=  {way/term src/hoof}
    ^+  +>
    va-abet:(va-apex:va way src)
  ::                                                  ::  ++what:le
  ++  what                                            ::  write deep storage
    |=  {hen/duct fal/(list (pair path plum))}
    ^+  +>
    ::  dev: collated `fal`
    ::
    =/  dev
      =|  $=  dev
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
      =>  .(fal t.fal)
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
        $(use.dev (~(put by use.dev) pax pet))
      ?~  old
        $(new.dev (~(put by new.dev) pax pet))
      $(rez.dev (~(put by rez.dev) pax pet))
    ::
    ::  just adopt user changes, which have no systems impact
    ::
    =.  fat.rep  (~(uni by fat.rep) use.dev)
    ::
    ::  but: kernel reboot operation, if any
    ::
    =/  but
      ^-  (unit boot:live)
      =/  hun  (~(get by rez.dev) /sys/hoon)
      =/  arv  (~(get by rez.dev) /sys/arvo)
      ?~  hun
        ?~  arv  ~
        ::
        ::  light reboot, arvo only
        ::
        `[~ (wilt u.arv)]
      ::
      ::  heavy reboot, hoon and arvo
      ::
      `[`(wilt u.hun) (wilt ?^(arv u.arv (~(got by fat.rep) /sys/arvo)))]
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
        run.gut  ::  syt: all systems changes
                 ::
                 =*  syt  (~(tap by (~(uni by rez.dev) new.dev)))
                 :_  run.gut
                 `move:live`[hen %give %& !>([%what syt])]
        ::
        ::  delete reboot source files from deep
        ::  storage, so install causes vane upgrade,
        ::  and *does not* cause repeat kernel upgrade.
        ::
        fat.rep  ?~  p.u.but
                   fat.rep
                 (~(del by fat.rep) /sys/hoon)
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
              vat/(list (pair term hoof))
          ==
      =<  [yor zus (~(tap by van))]
      ::  yor: reload shared structures
      ::  zus: reload shared library
      ::  vat: replacement map
      ::
      =/  yor  (bind (~(get by rez.dev) /sys/york) wilt)
      =/  zus  (bind (~(get by rez.dev) /sys/zuse) wilt)
      ::
      ::  %york is the subject of %zuse
      ::
      =.  zus  ?^(zus zus ?~(yor ~ `(wilt (~(got by fat.rep) /sys/zuse))))
      ::
      ::  vat: all vane upgrades, as [initial name source]
      ::
      =/  van
        ::  zyr: all system file replacements
        ::  van: accumulated upgrades
        ::
        =/  zyr  (~(tap by rez.dev))
        =|  van/(map @tas hoof)
        |-  ^+  van
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
          (~(put in mor) nam (wilt q.i.zyr))
        ::
        ::  reload current vanes if needed
        ::
        ?.  |((~(has by new.dev) /sys/hoon) ?=(^ zus))
          ::
          ::  we didn't replace compiler, %york or %zuse
          van
        ::
        ::  also reboot any vanes not already rebooted
        ::
        %-  ~(gas by van)
        %+  skip  
          ^-  (list (pair term hoof))
          %+  turn  (~(tap by van.mal))
          |=  {way/term vax/vase}
          [way (wilt (~(got by fat.rep) [%sys %van way ~]))]
        |=  {way/term src/hoof}
        (~(has in van) way)
      .
    ::
    ::  upgrade %york, vane shared structures
    ::
    =>  ?~  yor.job  .
        %=    .
           yor.mal  ~&  [%york-boot `@p`(mug u.yor.job)]
                     (slap !>(..arms) (ream u.yor.job))
        ==
    ::
    ::  upgrade %zuse, vane shared libraries
    ::
    =>  ?~  zus.job  .
        %=    .
           zus.mal  ~&  [%zuse-boot `@p`(mug u.zus.job)]
                     (slap yor.mal (ream u.zus.job))
        ==
    ::
    ::  upgrade all indicated vanes
    ::
    |-  ^+  +>.^$
    ?~  vat.job  +>.^$
    ~&  [%vane-boot p.i.vat.job `@p`(mug q.i.vat.job)]
    $(vat.job t.vat.job, +>.^$ (wise i.vat.job))
  ::                                                    ::  ++unix:le
  ++  unix                                              ::  return to unix
    |=  {hen/duct fav/card}
    ^+  +>
    ?>  ?=({* $~} hen)
    work(out.gut [[i.hen fav] out.gut])
  ::                                                    ::  ++call:le
  ++  call                                              ::  forward to vane
    |=  {hen/duct way/term hil/mill}
    ^+  +>
    =>  (call:(spin:(va-plow:(va-amid:va way) now eny.mal peek) hen) hil)
    abet
  ::                                                    ::  ++grow:le
  ++  grow                                              ::  hardcoded prefixes
    |=  lay/term
    ^-  term
    ?+  lay  !!
      $a  %ames
      $b  %behn
      $c  %clay
      $d  %dill
      $e  %eyre
      $f  %ford
      $g  %gall
      $j  %jael
    ==
  ::                                                    ::  ++peek:le
  ++  peek                                              ::  namespace
    |=  $:  ::  lyc: other ships result may leak to
            ::  cyr: general perspective, eg %cx
            ::  bem: name
            ::
            lyc/(unit (set ship))                       ::  leakset
            cyr/term                                    ::  full perspective
            bem/beam                                    ::  path
        ==
    ^-  (unit (unit cage))
    ::
    ::  way: vane to look in
    ::  car: perspective within vane
    ::
    =+  way=(grow (end 3 1 cyr))
    =+  car=(rsh 3 1 cyr)
    (scry:(va-plow:(va-amid:va way) now `@`0 peek) lyc car bem)
  ::
  ++  take
    |=  {hen/duct way/term tea/wire hil/mill}
    ^+  +>
    =>  (take:(spin:(va-plow:(va-amid:va way) now eny.mal peek) hen) tea hil)
    abet
  ::                                                    ::  ++work:le
  ++  work                                              ::  main loop
    =*  ken  .
    ^+  ken
    ::
    ::  no-op if stack is empty
    ::
    ?~  run.gut  ken
    ::
    ::  mov: top move on stack
    ::  hen: cause of move
    ::  act: action in move
    ::
    =/  mov  `move:live`i.run.gut
    ::
    ::  pop top move off stack
    ::
    =>  .(run.gut t.run.gut)
    ::
    ::  interpret top move
    ::
    ?-    -.q.mov
    ::
    ::  %give: return move
    ::
        $give
      ::
      ::  the duct can't be empty
      ::
      ?>  ?=(^ p.mov)
      ::
      ::  tea: top wire on duct
      ::  nex: rest of duct
      ::
      =/  tea  i.p.mov
      =*  nex  t.p.mov
      ::
      ::  route gift by wire
      ::
      ?:  ?=({$$ *} tea)
        ::
        ::  gift returned on arvo wire
        ::
        ?:  ?=({$unix $~} t.tea)
          ::
          ::  gift returned to unix i/o
          ::
          (unix nex (wile p.q.mov))
        ?>  ?=({$arvo $~} t.tea)
        ::
        ::  gift returned to arvo control
        ::
        (warp nex ((hard wasp) (wile p.q.mov)))
      ::
      ::  gift returned to calling vane
      ::
      ?>  ?=({@tas *} tea)
      (take nex i.tea t.tea p.q.mov)
    ::
    ::  %pass: forward move
    ::
        $pass
      (call [p.q.mov p.mov] p.q.q.mov q.q.q.mov)
    ==
  --
--
