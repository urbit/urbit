!:                                                      ::  /sys/arvo
::                                                      ::  %reference/2
::  %arvo: arvo microkernel.
::
=<  ::  this lifecycle wrapper makes the arvo door
    ::  (multi-armed core) look like a gate (function
    ::  or single-armed core), to fit urbit's formal
    ::  lifecycle function.  a practical interpreter
    ::  can ignore it.
    ::
    |=  {now/@da ovo/ovum}
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
++  beam  {{p/ship q/desk r/case} s/path}               ::  global name
++  beak  {p/ship q/desk r/case}                        ::  garnish with beak
++  card  {p/@tas q/*}                                  ::  tagged event
++  case                                                ::  version
  $%  {$da p/@da}                                       ::  date
      {$tas p/@tas}                                     ::  label
      {$ud p/@ud}                                       ::  sequence
  ==                                                    ::
++  cask  |*(a/mold (pair mark a))                      ::  global data
++  cave  (cask maze)                                   ::  marked untyped vase
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
++  mark  @tas                                          ::  content type
++  maze  {p/* q/*}                                     ::  untyped vase
++  mill  (each vase milt)                              ::  vase+metavase
++  milt  {p/* q/*}                                     ::  metavase
++  ovum  (pair wire card)                              ::  input or output
++  plum  (pair term noun)                              ::  deep file
++  ruby  @pG                                           ::  64-bit passcode
++  roof  (room vase)                                   ::  namespace
++  rook  (room maze)                                   ::  meta-namespace
++  room                                                ::  either namespace
  |*  vase/mold                                         ::  vase or maze
  $-  $:  lyc/(unit (set ship))                         ::  leakset
          car/term                                      ::  perspective
          bem/beam                                      ::  path
      ==                                                ::
  %-  unit                                              ::  ~: unknown
  %-  unit                                              ::  ~ ~: invalid
  (cask vase)                                           ::  marked cargo
::                                                      ::
++  ship  @p                                            ::  network identity
++  vane                                                ::  kernel module
  |*  $:  task/mold                                     ::  ->$ in request
          gift/mold                                     ::  <-$ out result
          sign/mold                                     ::  $<- in result
          note/mold                                     ::  $-> out request
          soul/mold                                     ::  current state
          seed/mold                                     ::  prior state
      ==                                                ::
  =*  move                                              ::
    $%  {$give p/gift}                                  ::  return
        {$pass p/path q/note}                           ::  invoke
    ==                                                  ::
  $_  =|  soul                                          ::  active state
  ^?  |%                                                ::
  ++  load  |~(seed +>)                                 ::  restore
  ++  stay  *soul                                       ::  preserve
  ++  plow                                              ::  work in time
    |_  $:  now/@da                                     ::  date
            eny/@e                                      ::  entropy
            sky/roof                                    ::  namespace
        ==                                              ::
    ++  doze  *(unit @da)                               ::  awake when
    ++  peek  roof                                      ::  local namespace
    ++  spin                                            ::  work on state
      |_  $:  hen/duct                                  ::  cause stack
              moz/(list move)                           ::  moves, inverted
          ==                                            ::
      ++  call  |~(task +>)                             ::  forward effect
      ++  take  |~({wire sign} +>)                      ::  backward effect
      --                                                ::
    --                                                  ::
  --                                                    ::
++  wire  path                                          ::  cause
--  =>
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (2) state molds
::                                                      ::  ::
|%
++  evil                                                ::  evolvable state
  |*  {type/_type hoon/_hoon vase/_vase}                ::  injected molds
  |%                                                    ::
  ++  ball                                              ::  arvo vane move
    $%  {$give p/mill}                                  ::  vane "return"
        {$pass p/wire q/(pair term mill)}               ::  vane "call"
    ==                                                  ::
  ++  mall                                              ::  any arvo version
    $?  {$293 mast}                                     ::  kelvin 293, current
    ==                                                  ::
  ++  mast                                              ::  system state
    $:  $=  gut                                         ::  abdomen
        $:  run/(list move)                             ::  worklist
            out/(list ovum)                             ::  unix output
            but/(unit seed)                             ::  reboot
        ==                                              ::
        $=  hax                                         ::  thorax
        $:  sac/worm                                    ::  compiler cache
        ==                                              ::
        $=  bug                                         ::  insect brain
        $:  noc/@ta                                     ::  process nonce
            ver/(qual @tas @ud @ud @ud)                 ::  vendor/version
        ==                                              ::
        $=  mal                                         ::  mammal brain
        $:  off/?                                       ::  not yet booted
            lac/_|                                      ::  not verbose
            eny/@uvJ                                    ::  512-bit entropy
            lul/vase                                    ::  %lull, vane models
            zus/vase                                    ::  %zuse, user lib
            van/(map term vase)                         ::  vanes
        ==                                              ::
        $=  rep                                         ::  reptile brain
        $:  orb/@p                                      ::  ship
            nym/arms                                    ::  name information
            roy/(map @ud ruby)                          ::  start secrets
            fat/(map path (pair term noun))             ::  boot filesystem
    ==  ==                                              ::
  ++  mill  (each vase maze)                            ::  vase or metavase
  ++  move  (pair duct ball)                            ::  vane move
  ++  worm                                              ::  compiler cache
    $:  nes/(set ^)                                     ::  ++nest
        pay/(map (pair type hoon) type)                 ::  ++play
        mit/(map (pair type hoon) (pair type nock))     ::  ++mint
    ==                                                  ::
  --                                                    ::
++  hoof  @t                                            ::  hoon source file
++  live  (evil)                                        ::  modern molds
++  seed  (pair (unit hoof) hoof)                       ::  hoon/arvo boot src
++  vile  (evil typo hoon vise)                         ::  old molds
++  wasp                                                ::  arvo effect
  $%  {$wack p/@uvJ}                                    ::  add entropy
      {$what p/(list (pair path (pair term noun)))}     ::  reset reptile files
      {$whim p/arms}                                    ::  reset arms
      {$wise p/(map @ud ruby)}                          ::  reset secrets
      {$whom p/@p}                                      ::  set identity; boot
  ==                                                    ::
--  =>
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (3) engines
::                                                      ::  ::
|%
::                                                      ::  ++le
++  le                                                  ::  deep engine
  =+  [now=*@da *mall:live]
  =*  ::
      ::  sys: system state
      ::
      sys  ->
  |%
  ::                                                    ::  ++abet:le
  ++  abet                                              ::  complete cycle
    ^-  {(each (list ovum) seed) _sys}
    ?^  but.gut
      [[%| u.but.gut] sys]
    [[%& (flop out.gut)] sys(out.gut ~)]
  ::                                                    ::  ++boot:le
  ++  boot                                              ::  reboot
    |=  $:  ::  hyn: optional hoon.hoon source
            ::  ars: arvo.hoon source
            ::
            hyn/(unit @t)
            ars/@t
        ==
    ^-  {* *}
    ?~  hyn
      ::  hon: hoon kernel as a vase
      ::  are: arvo kernel as a vase
      ::  arc: arvo core as a vase
      ::  lod: load gate on arvo core
      ::
      =/  hon  !>(..ride)
      ~&  [%compile-arvo `@p`(mug p.hon) `@p`(mug ars)]
      =*  are  (slap hon (ream ars))
      =*  arc  (slot 7 are)
      =*  lod  (slap arc [%limb %load])
      (^ q:(slam lod !>([now sys])))
    ::
    ::  compile the hoon.hoon source with the current compiler
    ::
    ~&  [%hoon-compile `@p`(mug u.hyn)]
    =+  raw=(ride %noun u.hyn)
    ::
    ::  activate the new compiler gate
    ::
    =+  cop=.*(0 +.raw)
    ::
    ::  find the hoon version number of the new kernel
    ::
    =+  nex=(@ .*(cop q:(~(mint ut p.raw) %noun [%limb %hoon])))
    ?>  |(=(nex hoon) =(+(nex) hoon))
    ::
    ::  if we're upgrading language versions, recompile the compiler
    ::
    =>  ?:  =(nex hoon)
          [hot=`*`raw .]
        ~&  [%hoon-compile-upgrade nex]
        =+  hot=.*(cop(+< [%noun u.hyn]) -.cop)
        .(cop .*(0 +.hot))
    ::
    ::  extract the hoon core from the outer gate
    ::
    =+  hoc=.*(cop [0 7])
    ::
    ::  compute the span of the hoon.hoon core
    ::
    =+  hyp=-:.*(cop(+< [-.hot '+>']) -.cop)
    ::
    ::  compile arvo
    ::
    ~&  [%compile-arvo `@p`(mug hyp) `@p`(mug ars)]
    =+  rav=.*(cop(+< [hyp ars]) -.cop)
    ::
    ::  create the arvo kernel
    ::
    =+  arv=.*(hoc +.rav)
    ::
    ::  extract the arvo core from the outer gate
    ::
    =+  voc=.*(arv [0 7])
    ::
    ::  compute the span of the arvo.hoon core
    ::
    =+  vip=-:.*(cop(+< [-.rav '+>']) -.cop)
    ::
    ::  entry gate: ++load for the normal case, ++come for upgrade
    ::
    =+  gat=.*(voc +:.*(cop(+< [vip ?:(=(nex hoon) 'load' 'come')]) -.cop))
    ::
    ::  sample: [date system-state]
    ::
    =+  sam=[now sys]
    ::
    ::  call into the new kernel
    ::
    (^ .*(gat(+< sam) -.gat))
  ::                                                    ::  ++call:le
  ++  call                                              ::  forward to vane
    |=  {hen/duct way/term hil/mill}
    ^+  +>
    (call:(spin way hen) hil)
  ::                                                    ::  ++doze:le
  ++  doze                                              ::  next wakeup by vane
    |=  way/term
    ^-  (unit @da)
    doze:(plow way)
  ::                                                    ::  ++emit:le
  ++  emit                                              ::  emit move
    |=  mov/move:live
    +>(run.gut [mov run.gut])
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
  ::                                                    ::  ++loop:le
  ++  loop                                              ::  main loop
    ^+  .
    ::  done if stack is empty
    ::
    ?~  run.gut  .
    ::
    ::  mov: top move on stack
    ::
    =/  mov  `move:live`i.run.gut
    ::
    ::  pop top move off stack
    ::
    =>  .(run.gut t.run.gut)
    ::
    ::  interpret top move
    ::
    ~&  [%brhp -.q.mov]
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
      ?.  ?=({$$ *} tea)
        ::
        ::  the caller was another vane
        ::
        ?>  ?=({@tas *} tea)
        (take nex i.tea t.tea p.q.mov)
      ::
      ::  the caller was arvo itself
      ::
      ?:  ?=({$unix $~} t.tea)
        ::
        ::  the caller was unix i/o
        ::
        (unix nex (wile p.q.mov))
      ?>  ?=({$arvo $~} t.tea)
      ::
      ::  the caller was boot logic
      ::
      (warp nex ;;(wasp (wile p.q.mov)))
    ::
    ::  %pass: forward move
    ::
        $pass
      ::  tea: proximate cause of action
      ::  hen: ultimate cause of action
      ::  way: target
      ::  hil: event data
      ::
      =*  tea  p.q.mov
      =*  hen  p.mov
      =*  way  p.q.q.mov
      =*  hil  q.q.q.mov
      (call [tea hen] way hil)
    ==
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
    loop:(emit mov)
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
  ::                                                    ::  ++peek:le
  ++  peek                                              ::  namespace
    |=  $:  ::  lyc: other ships result may leak to
            ::  cyr: general perspective, eg %cx
            ::  bem: name
            ::
            lyc/(unit (set ship))
            cyr/term
            bem/beam
        ==
    ^-  (unit (unit cave))
    ::
    ::  way: vane to look in
    ::  car: perspective within vane
    ::
    =*  way  (grow (end 3 1 cyr))
    =*  car  (rsh 3 1 cyr)
    (peek:(plow(eny.mal `@`0) way) lyc car bem)
  ::                                                    ::  ++plow:le
  ++  plow                                              ::  plowing vane
    |=  way/term
    (va-plow:(va-amid:va way) now eny.mal peek)
  ::                                                    ::  ++poke:le
  ++  poke                                              ::  event from unix
    |=  $:  ::  ovo: event
            ::
            ovo/ovum
        ==
    ^+  +>
    ~&  [%poke -.ovo]
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
        $wake  (pike %behn now ovo)
    ::
        ?($wack $what $whom $whim $wise)
      =/  wap  ;;(wasp q.ovo)
      =*  tea  `wire`[%$ %arvo ~]
      =*  hen  `duct`[tea [p.ovo ~]]
      =*  mov  `move:live`[hen %give %& !>(wap)]
      loop:(emit mov)
    ==
  ::                                                    ::  ++spin:le
  ++  spin                                              ::  spinning vane
    |=  {way/term hen/duct}
    (spin:(plow way) hen)
  ::
  ++  take
    |=  {hen/duct way/term tea/wire hil/mill}
    ^+  +>
    =<  loop
    (take:(spin way hen) tea hil)
  ::                                                    ::  ++unix:le
  ++  unix                                              ::  return to unix
    |=  {hen/duct fav/card}
    ^+  +>
    ?>  ?=({* $~} hen)
    loop(out.gut [[i.hen fav] out.gut])
  ::                                                    ::  ++va:le
  ++  va                                                ::  vane engine
    |_  $:  ::  way: vane name, eg `%ames`
            ::  vax: vane, or vane builder if `off.mal`
            ::
            way/term
            vax/vase
        ==
    ::                                                  ::  ++va-abet:va:le
    ++  va-abet                                         ::  resolve
      ^+  ..va
      ..va(van.mal (~(put by van.mal) way vax))
    ::                                                  ::  ++va-amid:va:le
    ++  va-amid                                         ::  load existing
      |=  way/term
      ^+  +>
      ?<  off.mal
      +>(way way, vax (~(got by van.mal) way))
    ::                                                  ::  ++va-abut:va:le
    ++  va-apex                                         ::  boot / reboot
      |=  $:  way/term
              src/hoof
          ==
      ^+  +>
      =.  ^way  way
      =/  bun  (~(get by van.mal) way)
      ?~  bun
        (va-create src)
      (va-update(vax u.bun) src)
    ::                                                  ::  ++va-plow:va:le
    ++  va-plow                                         ::  context awareness
      |=  $:  ::  now: date
              ::  eny: 512-bit entropy
              ::  sky: meta-typed namespace
              ::
              now/@da
              eny/@uvJ
              sky/rook
          ==
      ::  kys: user-typed namespace vase
      ::  sam: core sample
      ::  wok: plowing vase
      ::
      =*  kys  `vase`[-:!>(*roof) sky]
      =*  sam  (slop !>(now) (slop !>(eny) kys))
      =^  wok  sac.hax  (~(open wa sac.hax) vax %plow %& sam)
      |%
      ::                                                ::  ++doze:va-plow:va:le
      ++  doze                                          ::  next wakeup
        ^-  (unit @da)
        =^  pro  sac.hax  (~(slap wa sac.hax) wok [%limb %doze])
        =.  sac.hax  (~(neat wa sac.hax) -:!>(*(unit @da)) %& pro)
        ((unit @da) q.pro)
      ::                                                ::  ++peek:va-plow:va:le
      ++  peek                                          ::  internal peek
        |=  $:  ::  lyc: set of output ships
                ::  car: local perspective
                ::  bem: true path
                ::
                lyc/(unit (set ship))
                car/term
                bem/beam
            ==
        ^-  (unit (unit cave))
        ::
        ::  yeb: namespace input
        ::  pro: namespace output
        ::
        =/  yeb  !>([lyc car bem])
        =^  pro  sac.hax  (~(call wa sac.hax) wok %peek %& yeb)
        =.  sac.hax  (~(neat wa sac.hax) -:!>([*mark *vase]) %& pro)
        ::
        ::  detect unit cases
        ::
        ?~  q.pro  ~
        ?~  +.q.pro  [~ ~]
        ::
        ::  dat: vase of [mark vase]
        ::
        =^  dat  sac.hax  (~(slot wa sac.hax) 7 pro)
        ``[(mark -.q.dat) (^ +.q.dat)]
      ::                                                ::  ++spin:va-plow:va:le
      ++  spin                                          ::  causal action
        |=  hen/duct
        ::
        ::  fox: spinning vase
        ::
        =*  sam  !>([hen *(list move:live)])
        =^  fox  sac.hax  (~(open wa sac.hax) vax %spin %& sam)
        |%
        ::                                              ::  ++abet:spin:va-plow:
        ++  abet                                        ::  integrate
          ^+  ..va
          ::
          ::  vom: vase of (list move)
          ::  moz: actual output list (inverted order)
          ::  zax: new vase core
          ::
          =^  vom  sac.hax  (~(slot wa sac.hax) 13 fox)
          =^  moz  sac.hax  (~(refine-moves me sac.hax) vom)
          =^  zax  sac.hax  (~(slot wa sac.hax) 31 fox)
          %=    va-abet
              vax  zax
              run.gut
            %+  weld
              %+  turn  (flop moz)
              |=  mov/move:live
              ?.  ?=($pass -.q.mov)  mov
              ::
              ::  append vane label to pass return address
              ::
              mov(p.q [way p.q.mov])
            run.gut
          ==
        ::                                              ::  ++call:spin:va-plow:
        ++  call                                        ::  pass forward
          |=  $:  ::  hil: logical argument
                  ::
                  hil/mill
              ==
          ^+  ..va
          =^  nex  sac.hax  (~(call wa sac.hax) fox %cnhp hil)
          abet(fox nex)
        ::                                              ::  ++take:spin:va-plow:
        ++  take                                        ::  pass backward
          |=  $:  ::  tea: return address
                  ::  hil: logical result
                  ::
                  tea/wire
                  hil/mill
              ==
          ^+  ..va
          ::  yet: return address as vase
          ::  sam: whole sample as mill
          ::
          =/  yet  !>(tea)
          =/  sam  ^-  mill
            ?-  -.hil
              %&  [%& (slop yet p.hil)]
              %|  [%| [[%cell p.yet p.p.hil] [q.yet q.p.hil]]]
            ==
          =^  nex  sac.hax  (~(call wa sac.hax) fox %take sam)
          abet(fox nex)
        --
      --
    ::                                                  ::  ++va-create:va:le
    ++  va-create                                       ::  compile new vase
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
    ::                                                  ::  ++va-settle:va:le
    ++  va-settle                                       ::  initialize with ship
      ^+  .
      .(vax (slam vax !>(orb.rep)))
    ::                                                  ::  ++va-update
    ++  va-update                                       ::  replace existing
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
  ::                                                    ::  ++warp:le
  ++  warp                                              ::  arvo effect
    |=  {hen/duct wap/wasp}
    ^+  +>
    ~&  [%warp -.wap]
    ?-  -.wap
      $wack  +>(eny.mal (mix (shaz (mix now eny.mal)) (shaz p.wap)))
      $what  (what hen p.wap)
      $whim  +>(nym.rep p.wap)
      $wise  +>(roy.rep p.wap)
      $whom  (whom hen p.wap)
    ==
  ::                                                    ::  ++whom:le
  ++  whom                                              ::  initialize ship
    |=  {hen/duct our/@p}
    ^+  +>
    ::  initialization only happens once
    ::
    ?>  =(& off.mal)
    ::
    ::  continue working after initialization
    ::
    =<  loop
    ::
    ::  set all flags
    ::
    =:  orb.rep  our
        off.mal  |
      ==
    ::
    ::  activate all vanes
    ::
    =.  van.mal
      %-  ~(run by van.mal)
      |=(vase (slam +< !>(our)))
    ::
    ::  send vanes `[%boot ~]` card, in alphabetical order
    ::
    =/  fal  (sort (turn ~(tap by van.mal) |=({term *} +<-)) aor)
    |-  ^+  +>.^$
    ?~  fal  +>.^$
    =.  +>.^$  $(fal t.fal)
    (emit [hen %pass [%$ %arvo ~] i.fal %& !>([%boot ~])])
  ::                                                    ::  ++wile:le
  ++  wile                                              ::  mill as card
    |=  hil/mill
    ^-  card
    =.  sac.hax  (~(neat wa sac.hax) -:!>(*card) hil)
    ?-  -.hil
      %|  ;;(card q.p.hil)
      %&  ;;(card q.p.hil)
    ==
  ::                                                    ::  ++wilt:le
  ++  wilt                                              ::  deep file as source
    |=  pet/plum
    ^-  hoof
    ?>(?=({$hoon @tas} pet) +.pet)
  ::                                                    ::  ++wise:le
  ++  wine                                              ::  load/reload vane
    |=  {way/term src/hoof}
    ^+  +>
    va-abet:(va-apex:va way src)
  ::                                                    ::  ++what:le
  ++  what                                              ::  write deep storage
    |=  {hen/duct fal/(list (pair path plum))}
    ^+  +>
    ::  dev: collated `fal`
    ::
    =/  dev
      =|  $=  dev
          $:  ::  use: non-system files
              ::  new: new set
              ::  del: installs + replacements
              ::
              use/(map path plum)
              new/(set path)
              del/(map path plum)
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
      ?.  ?=({$neo *} pax)
        $(use.dev (~(put by use.dev) pax pet))
      =?    new.dev
          =(~ old)
        (~(put in new.dev) pax)
      $(del.dev (~(put by del.dev) pax pet))
    ::
    ::  print new entries
    ::
    ~?  !=(~ use.dev)
      [%what-old (turn ~(tap by use.dev) |=({path *} +<-))]
    ~?  !=(~ new.dev)
      [%what-new ~(tap in new.dev)]
    ~?  !=(~ use.dev)
      [%what-del (turn ~(tap by del.dev) |=({path *} +<-))]
    ::
    ::  just adopt user changes, which have no systems impact
    ::
    =.  fat.rep  (~(uni by fat.rep) use.dev)
    ::
    ::  but: kernel reboot operation, if any
    ::
    =/  but
      ^-  (unit seed)
      ::
      ::  when we get new hoon and arvo system files,
      ::  we assume they match what's running now
      ::
      =/  hun  ?:  (~(has in new.dev) /neo/hoon)  ~
               (~(get by del.dev) /neo/hoon)
      =/  arv  ?:  (~(has in new.dev) /neo/arvo)  ~
               (~(get by del.dev) /neo/hoon)
      ?~  hun
        ?~  arv  ~
        ::
        ::  light reboot, arvo only
        ::
        ~&  %light-reboot
        `[~ (wilt u.arv)]
      ::
      ::  heavy reboot, hoon and arvo
      ::
      ~&  %heavy-reboot
      `[`(wilt u.hun) (wilt ?^(arv u.arv (~(got by fat.rep) /neo/arvo)))]
    ?^  but
      ::  stop working and set up reboot
      ::
      ~&  %reboot
      %=  +>.$
        ::  set boot hook for termination
        ::
        but.gut  ?>(=(~ but.gut) but)
        ::
        ::  finish write *after* reboot, not now, so that new code
        ::  can use the new kernel
        ::
        run.gut  :_  run.gut
                 `move:live`[hen %give %& !>([%what ~(tap by del.dev)])]
        ::
        ::  delete kernel source file from deep
        ::  storage, so that install causes vane upgrade,
        ::  and *does not* cause repeat kernel upgrade.
        ::
        fat.rep  ?~  p.u.but
                   fat.rep
                 (~(del by fat.rep) /neo/hoon)
      ==
    ::  keep working after vane upgrades
    ::
    =<  loop
    ::
    ::  job: plan for upgrading
    ::
    =/  job
      ^-  $:  lul/(unit hoof)
              zus/(unit hoof)
              vat/(list (pair term hoof))
          ==
      =<  [lul zus ~(tap by van)]
      ::
      ::  lul: shared structures
      ::  zus: shared library
      ::
      =/  lul  (bind (~(get by del.dev) /neo/lull) wilt)
      =/  zus  (bind (~(get by del.dev) /neo/zuse) wilt)
      ::
      ::  %lull is the subject of %zuse; if we have a new %lull,
      ::  but no new %zuse, get the running %
      ::
      =.  zus  ?^(zus zus ?~(lul ~ `(wilt (~(got by fat.rep) /neo/zuse))))
      ::
      ::  van: all vane upgrades, as [initial name source]
      ::
      =/  van
        ::  zyr: all system file replacements
        ::  van: accumulated upgrades
        ::
        =/  zyr  ~(tap by del.dev)
        =|  van/(map @tas hoof)
        |-  ^+  van
        ?^  zyr
          ::  mor: process rest of `zyr`
          ::
          =/  mor  $(zyr t.zyr)
          ?.  ?=({$neo $van @tas $~} p.i.zyr)
            ::
            ::  ignore anything that isn't a vane
            ::
            mor
          ::  replaced vane in `/neo/vane/*/[nam]`
          ::
          =*  nam  `term`i.t.t.p.i.zyr
          ~&  [%new-vane nam `path`p.i.zyr `@p`(mug q.i.zyr)]
          (~(put in mor) nam (wilt q.i.zyr))
        ::
        ::  if this is a new install after a heavy reboot,
        ::  or if we've adjusted %zuse or %lull, reboot all
        ::  running vanes
        ::
        ?.  |((~(has in new.dev) /neo/hoon) ?=(^ zus))  van
        ::
        ::  all running vanes
        ::
        %-  ~(gas by van)
        %+  skip
          ^-  (list (pair term hoof))
          %+  turn  ~(tap by van.mal)
          |=  {way/term vax/vase}
          [way (wilt (~(got by fat.rep) [%neo %van way ~]))]
        |=  {way/term src/hoof}
        (~(has by van) way)
      .
    ::  upgrade %lull, vane shared structures
    ::
    =>  ?~  lul.job  .
        %=    .
           lul.mal  ~&  [%lull-boot `@p`(mug u.lul.job)]
                     (slap !>(..arms) (ream u.lul.job))
        ==
    ::  upgrade %zuse, vane shared libraries
    ::
    =>  ?~  zus.job  .
        %=    .
           zus.mal  ~&  [%zuse-boot `@p`(mug u.zus.job)]
                     (slap lul.mal (ream u.zus.job))
        ==
    ::  upgrade all indicated vanes
    ::
    |-  ^+  +>.^$
    ?~  vat.job  +>.^$
    ~&  [%vane-boot p.i.vat.job `@p`(mug q.i.vat.job)]
    $(vat.job t.vat.job, +>.^$ (wine i.vat.job))
  --
::                                                      ::  ++me
++  me                                                  ::  dynamic analysis
  ::  sac: compiler cache
  ::
  |_  sac/worm
  ::                                                    ::  ++refine-moves:me
  ++  refine-moves                                      ::  move list from vase
    |=  vax/vase
    ^-  {(list move:live) worm}
    ?:  =(~ q.vax)  [~ sac]
    =^  hed  sac  (~(slot wa sac) 2 vax)
    =^  tal  sac  (~(slot wa sac) 3 vax)
    =^  mov  sac  (refine-move hed)
    =^  moz  sac  $(vax tal)
    [[mov moz] sac]
  ::                                                    ::  ++refine-move:me
  ++  refine-move                                       ::  move from vase
    |=  vax/vase
    ^-  {move:live worm}
    ::
    ::  den: ++duct vase
    ::  yat: card vase
    ::
    =^  hip  sac  (~(nell wa sac) p.vax)
    ?>  hip
    =^  den  sac  (~(slot wa sac) 2 vax)
    =^  yat  sac  (~(slot wa sac) 3 vax)
    =.  sac  (~(neat wa sac) -:!>(*duct) %& den)
    ?>  hip
    =^  del  sac  (refine-ball yat)
    [[(duct q.den) del] sac]
  ::                                                    ::  ++refine-ball:me
  ++  refine-ball                                       ::  ball from vase
    |=  vax/vase
    ^-  {ball:live worm}
    ::
    ::  specialize span to actual card stem
    ::
    =^  hex  sac  (~(sped wa sac) vax)
    ?+    -.q.hex  ~|(%bad-move !!)
        $give
      =.  sac  (~(neat wa sac) -:!>([%give *card]) %& hex)
      ::
      ::  yed: vase containing card
      ::  hil: card as mill
      ::
      =^  yed  sac  (~(slot wa sac) 3 hex)
      =^  hil  sac  (refine-card yed)
      [[%give hil] sac]
    ::
        $pass
      =.  sac  (~(neat wa sac) -:!>([%pass *path *term *card]) %& hex)
      ::
      ::  yed: vase containing card
      ::  hil: card as mill
      ::
      =^  yed  sac  (~(slot wa sac) 15 hex)
      =^  hil  sac  (refine-card yed)
      [[%pass (path +6:p.hex) (term +14:p.hex) hil] sac]
    ==
  ::                                                    ::  ++refine-card:me
  ++  refine-card                                       ::  card from vase
    |=  vax/vase
    ^-  (pair mill worm)
    ::
    ::  specialize span to actual card data
    ::
    =^  hex  sac  (~(sped wa sac) vax)
    =^  hip  sac  (~(nell wa sac) p.hex)
    ?>  hip
    ?.  ?=($meta -.q.hex)
      ::
      ::  for an non-meta card, the mill is the vase
      ::
      [[%& hex] sac]
    ::
    ::  tiv: vase of vase of card
    ::  typ: vase of span
    ::
    =^  tiv  sac  (~(slot wa sac) 3 hex)
    =^  hip  sac  (~(nell wa sac) p.tiv)
    ?>  hip
    =^  typ  sac  (~(slot wa sac) 2 tiv)
    =.  sac  (~(neat wa sac) -:!>(*type) %& hex)
    ::
    ::  support for meta-meta-cards has been removed
    ::
    [[%| (^ q.tiv)] sac]
  -- :: me
--
::                                                      ::  ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::  (4) interface
::                                                      ::  ::
=|  sys/mall:live
|%
::                                                      ::  ++come
++  come                                                ::  load old-hoon, +11
  |=  {now/@da old/mall:vile}
  ::
  ::  if typed, would produce `{(list ovum) _+>}`
  ::
  ^-  {* *}
  ::  trivial when arvo models don't change
  ::
  (load now old)
::                                                      ::  ++keep
++  keep                                                ::  timeout, +4
  |=  {now/@da pax/path}
  ^-  (unit @da)
  ::
  ::  XX: change interface to specify vane, not path
  ::  XX: rename "keep" to "doze"
  ::  way: vane of timeout
  ::
  ?>  ?=({$$ @tas $~} pax)
  =*  way  i.t.pax
  (~(doze le now sys) way)
::                                                      ::  ++load
++  load                                                ::  load current, +86
  |=  {now/@da new/mall:live}
  ::
  ::  if typed, would produce `{(list ovum) _+>}`
  ::
  ^-  {* *}
  (poke(sys new) now *ovum)
::                                                      ::  ++peek
++  peek                                                ::  inspect, 87
  |=  {now/@da pax/path}
  ^-  (unit *)
  ::
  ::  XX: adapt external users to modern (unit (unit cage))
  ::
  ?.  ?=({@ta @ta @ta @ta *} pax)  ~
  ::
  ::  lyc: access control, `[~ ~]` gets anything
  ::  cyr: perspective
  ::  bec: path head, `[ship desk case]`
  ::  tyl: path tail
  ::  nut: peek result
  ::
  =/  lyc  `(unit (set ship))`[~ ~]
  =/  cyr  ?>(((sane %tas) i.pax) `@tas`i.pax)
  =/  bec  ^-  beak
    :+  (slav %p i.t.pax)
      (slav %tas i.t.t.pax)
    ;;(case (slay i.t.t.t.pax))
  =*  tyl  t.t.t.t.pax
  =/  nut  (~(peek le now sys) lyc cyr bec tyl)
  ?~  nut  ~
  ?~  u.nut  ~
  [~ +.q.u.u.nut]
::                                                      ::  ++poke
++  poke                                                ::  apply, 42
  |=  {now/@da ovo/ovum}
  ::
  ::  if typed, would produce `{(list ovum) _+>}`
  ::
  ^-  {* *}
  ::
  ::  iterate entropy, it can't hurt
  ::
  =.  eny.mal.sys  (mix (shaz now) eny.mal.sys)
  ::
  ::  produce a new state, and either output or a reboot
  ::
  =^  new  sys
    =<  abet
    ::
    ::  as a hack for reboots, an empty ovum is a no-op
    ::
    ?:  =(*ovum ovo)
      ~(loop le now sys)
    (~(poke le now sys) ovo)
  ?-  -.new
    ::
    ::  no reboot; produce output and current core
    ::
    %&  [`(list ovum)`p.new +>.$]
    ::
    ::  reboot; produce loop result from new kernel
    ::
    %|  ~(boot le now sys)
  ==
::                                                      ::  ++wish
++  wish                                                ::  compute, 20
  |=  src/hoof
  q:(slap ?:(off.mal.sys !>(+>) zus.mal.sys) (ream src))
--
