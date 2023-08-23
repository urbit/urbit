=>  ..ride  =>
!:
|%
+|  %global
::
++  arvo  %237
::
::  $arch: node identity
::  $axal: fundamental node, recursive (trie)
::  $axil: fundamental node
::  $beak: global context
::  $beam: global name
::  $bone: opaque duct handle
::  $case: global version
::  $cage: marked vase
::  +cask: marked data builder
::  $desk: local workspace
::  $dock: message target
::  $gang: infinite set of peers
::  $mark: symbolic content type
::  $mien: orientation
::  $page: untyped cage
::  $omen: fully-qualified namespace path
::  $ship: network identity
::  $sink: subscription
::
+$  arch  (axil @uvI)
++  axal
  |$  [item]
  [fil=(unit item) dir=(map @ta $)]
++  axil
  |$  [item]
  [fil=(unit item) dir=(map @ta ~)]
::
+$  beak  (trel ship desk case)
+$  beam  [beak s=path]
+$  bone  @ud
+$  case
  $%  ::  %da:  date
      ::  %tas: label
      ::  %ud:  sequence
      ::  %uv:  hash
      ::
      [%da p=@da]
      [%tas p=@tas]
      [%ud p=@ud]
      [%uv p=@uv]
  ==
+$  cage  (cask vase)
++  cask  |$  [a]  (pair mark a)
+$  desk  @tas
+$  dock  (pair @p term)
+$  gang  (unit (set ship))
+$  mark  @tas
+$  mien  [our=ship now=@da eny=@uvJ]
+$  page  (cask)
+$  omen  [vis=view bem=beam]
+$  ship  @p
+$  sink  (trel bone ship path)
::
+|  %meta
::
::  +hypo: type-associated builder
::  $meta: meta-vase
::  $maze: vase, or meta-vase
::
++  hypo
  |$  [a]
  (pair type a)
+$  meta  (pair)
+$  maze  (each vase meta)
::
+|  %interface
::
::  $ball: dynamic kernel action
::  $card: tagged, untyped event
::  $duct: causal history
::  +hobo: %soft task builder
::  $goof: crash label and trace XX fail/ruin/crud/flaw/lack/miss
::  $mass: memory usage
::  $move: cause and action
::  $ovum: card with cause
::  $roof: namespace
::  $rook: meta-namespace (super advanced)
::  +room: generic namespace
::  +roon: partial namespace
::  $root: raw namespace
::  $view: namespace perspective
::  +wind: kernel action builder
::  $wire: event pretext
::  +wite: kernel action/error builder
::
+$  ball  (wite [vane=term task=maze] maze)
+$  card  (cask)
+$  duct  (list wire)
++  hobo
  |$  [a]
  $?  $%  [%soft p=*]
      ==
      a
  ==
+$  goof  [mote=term =tang]
+$  mass  $~  $+|+~
          (pair cord (each * (list mass)))
+$  move  [=duct =ball]
+$  ovum  [=wire =card]
::
+$  roof  (room vase)                                   ::  namespace
+$  rook  (room meta)                                   ::  meta-namespace
++  room                                                ::  either namespace
  |$  [a]
  $~  =>(~ |~(* ~))
  $-  $:  lyc=gang                                      ::  leakset
          pov=path                                      ::  provenance
          omen                                          ::  perspective, path
      ==                                                ::
  %-  unit                                              ::  ~: unknown
  %-  unit                                              ::  ~ ~: invalid
  (cask a)                                              ::
+$  roon                                                ::  partial namespace
  $~  =>(~ |~(* ~))
  $-  [lyc=gang pov=path car=term bem=beam]
  (unit (unit cage))
+$  root  $-(^ (unit (unit)))
+$  view  $@(term [way=term car=term])                  ::  perspective
::
++  wind
  |$  ::  a: forward
      ::  b: reverse
      ::
      [a b]
  $%  ::  %pass: advance
      ::  %slip: lateral
      ::  %give: retreat
      ::
      [%pass p=path q=a]
      [%slip p=a]
      [%give p=b]
  ==
+$  wire  path
++  wite
  |$  ::  note: a routed $task
      ::  gift: a reverse action
      ::
      ::    NB:  task: a forward action
      ::         sign: a sourced $gift
      ::
      [note gift]
  $%  ::  %hurl: action failed
      ::  %pass: advance
      ::  %slip: lateral
      ::  %give: retreat
      ::
      [%hurl =goof wite=$;($>(?(%pass %give) $))]
      [%pass =wire =note]
      [%slip =note]
      [%give =gift]
  ==
::
+|  %implementation
::
::  $debt: ephemeral state
::  $grub: persistent state, larval stage
::  $germ: worklist source and bar stack
::  $heir: upgradeable state
::  $plan: worklist
::  $soul: persistent state
::  $vane: kernel module
::  $vere: runtime version
::  $vile: reflexive constants
::  $waif: arvo task, from anywhere
::  $wasp: arvo task, from Outside
::  $weft: kelvin version, tag and number
::  $worm: compiler cache
::  $wisp: arvo task, larval stage
::  $wynn: kelvin stack
::
+$  debt
  $:  ::  run: list of worklists
      ::  out: pending output
      ::  kel: kernel files
      ::  fil: pending files
      ::
      run=(list plan)
      out=(list ovum)
      kel=(list (pair path (cask)))
      fil=(list (pair path (cask)))
  ==
+$  germ  [vane=term bars=(list duct)]
+$  grub
  $:  ::  who: identity once we know it
      ::  eny: entropy once we learn it
      ::  lac: laconicity as we want it
      ::  ver: the Outside as we see it
      ::  fat: source when we attain it
      ::  lul: %lull when we acquire it
      ::  zus: %zuse once we receive it
      ::  van: vanes while we desire it
      ::
      who=(unit ship)
      eny=(unit @)
      lac=?
      ver=(unit vere)
      fat=(unit (axal (cask)))
      lul=(unit (trap vase))
      zus=(unit (trap vase))
      van=(map term (trap vase))
  ==
+$  heir
  $%  $:  %grub
          $%  [?(%240 %239 %238 %237) =grub]
      ==  ==
      [?(%240 %239 %238 %237) =debt =soul]
  ==
+$  plan  (pair germ (list move))
+$  soul
  $:  ::  identity, time, entropy
      ::  fad: configuration
      ::  zen: Outside knowledge
      ::  mod: internal modules
      ::
      mien
      $=  fad
      $:  ::  lac: not verbose
          ::
          lac=?
      ==
      $=  zen
      $:  ::  ver: runtime version
          ::  lag: upgrade blocked
          ::
          ver=vere
          lag=_|
      ==
      $=  mod
      $:  ::  fat: filesystem
          ::  lul: %lull
          ::  zus: %zuse
          ::  van: vanes
          ::
          fat=(axal (cask))
          lul=vase
          zus=vase
          van=(map term vane)
      ==
  ==
+$  vane  [=vase =worm]
+$  vere  [[non=@ta rev=path] kel=wynn]
+$  vile
  $:  typ=type    ::  -:!>(*type)
      duc=type    ::  -:!>(*duct)
      wir=type    ::  -:!>(*wire)
      dud=type    ::  -:!>(*(unit goof))
  ==
::
+$  waif
  ::  %trim: trim state, spam to all
  ::  %what: update from files
  ::  %whey: produce $mass                    :: XX remove, scry
  ::  %verb: toggle laconicity
  ::  %whiz: prime vane caches
  ::
  $%  [%trim p=@ud]
      [%what p=(list (pair path (cask)))]
      [%whey ~]
      [%verb p=(unit ?)]
      [%whiz ~]
  ==
+$  wasp
  ::  %crud: reroute $ovum with $goof
  ::  %wack: iterate entropy
  ::  %wyrd: check/record runtime kelvin stack
  ::
  $%  [%crud =goof =ovum]
      [%wack p=@uvJ]
      [%wyrd p=vere]
  ==
+$  weft  [lal=@tas num=@ud]
+$  worm
  $:  ::  +nest, +play, and +mint
      ::
      nes=(set ^)
      pay=(map (pair type hoon) type)
      mit=(map (pair type hoon) (pair type nock))
  ==
+$  wisp
  $%  $>(?(%verb %what) waif)
      $>(?(%wack %wyrd) wasp)
      [%whom p=ship]
  ==
+$  wynn  (list weft)
--  =>
::
~%  %hex  ..ut  ~
|%
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bE, Arvo core                ::
::
++  en-beam
  |=(b=beam =*(s scot `path`[(s %p p.b) q.b (s r.b) s.b]))
::
++  de-beam
  ~/  %de-beam
  |=  p=path
  ^-  (unit beam)
  ?.  ?=([@ @ @ *] p)  ~
  ?~  who=(slaw %p i.p)  ~
  ?~  des=?~(i.t.p (some %$) (slaw %tas i.t.p))  ~  :: XX +sym ;~(pose low (easy %$))
  ?~  ved=(de-case i.t.t.p)  ~
  `[[`ship`u.who `desk`u.des u.ved] t.t.t.p]
::
++  de-case
  ~/  %de-case
  |=  =knot
  ^-  (unit case)
  ?^  num=(slaw %ud knot)  `[%ud u.num]
  ?^  wen=(slaw %da knot)  `[%da u.wen]
  ?^  hax=(slaw %uv knot)  `[%uv u.hax]
  ?~  lab=(slaw %tas knot)  ~
  `[%tas u.lab]
::
++  en-omen
  |=  omen
  ^-  path
  :_  (en-beam bem)
  ?@  vis  vis
  ~(rent co [%many $/tas/way.vis $/tas/car.vis ~])
::
++  de-omen
  ~/  %de-omen
  |=  pax=path
  ^-  (unit omen)
  ?~  pax  ~
  ?~  bem=(de-beam t.pax)  ~
  ?:  ((sane %tas) i.pax)
    `[i.pax u.bem]
  =/  lot=(unit coin)  (rush i.pax ;~(pfix dot perd:so))
  ?.  ?&  ?=(^ lot)
          ?=([%many [%$ %tas @] [%$ %tas @] ~] u.lot)
      ==
    ~
  `[[q.p.i q.p.i.t]:p.u.lot u.bem]
::
++  look
  ~/  %look
  |=  [rof=roof lyc=gang pov=path]
  ^-  root
  ~/  %in
  |=  [ref=* raw=*]
  ?~  pax=((soft path) raw)  ~
  ?~  mon=(de-omen u.pax)  ~
  ?~  dat=(rof lyc pov u.mon)  ~
  ?~  u.dat  [~ ~]
  =*  vax  q.u.u.dat
  ?.  ?&  ?=(^ ref)
          =(hoon-version -.ref)
          -:(~(nets wa *worm) +.ref p.vax)
      ==
    ~>(%slog.[0 leaf+"arvo: scry-lost"] ~)
  [~ ~ q.vax]
::  |wyrd: kelvin negotiation
::
::    specified but unimplemented:
::    arvo should produce a [wend/wynn] effect
::    to signal downgrade
::
++  wyrd
  |%
  ::  +sane: kelvin stack for validity
  ::
  ++  sane
    |=  kel=wynn
    ^-  ?
    ?:  =(~ kel)  &
    =^  las=weft  kel  kel
    |-  ^-  ?
    ?~  kel  &
    ?&  (gte num.las num.i.kel)
        $(las i.kel, kel t.kel)
    ==
  ::  +need: require kelvins
  ::
  ++  need
    |=  [run=wynn hav=wynn]
    ::  wyr: ~: runtime supports all required kelvins
    ::       ^: runtime support is missing or lagging
    ::
    =;  wyr  !.
      ?~  wyr
        same
      ~&  wyrd=wyr
      ~_  :+  %rose
            [" " ~ ~]
          :~  =+  p.u.wyr
              leaf/"%{(trip lal)} %{(scow %ud num)} required;"
              ?~  q.u.wyr
                leaf/"runtime missing support"
              leaf/"runtime only supports %{(scow %ud u.q.u.wyr)}"
          ==
      ~>  %mean.'arvo: upgrade blocked'
      ~>  %mean.'wyrd'
      !!
    ::
    |-  ^-  (unit (pair weft (unit @ud)))
    ?~  hav  ~
    ::
    ::  fel: %&: runtime kelvin for [i.hav]
    ::       %|: no specified runtime support
    ::
    =/  fel
      |-  ^-  (each @ud weft)
      ?~  run  |/i.hav
      ?:(=(lal.i.hav lal.i.run) &/num.i.run $(run t.run))
    ::
    ?-  -.fel
      %|  `[p.fel ~]
      %&  ?.((lte p.fel num.i.hav) `[i.hav `p.fel] $(hav t.hav))
    ==
  --
::
::  |of: axal engine
::
++  of
  =|  fat=(axal)
  |@
  ++  del
    |=  pax=path
    ^+  fat
    ?~  pax  [~ dir.fat]
    =/  kid  (~(get by dir.fat) i.pax)
    ?~  kid  fat
    fat(dir (~(put by dir.fat) i.pax $(fat u.kid, pax t.pax)))
  ::  Descend to the axal at this path
  ::
  ++  dip
    |=  pax=path
    ^+  fat
    ?~  pax  fat
    =/  kid  (~(get by dir.fat) i.pax)
    ?~  kid  [~ ~]
    $(fat u.kid, pax t.pax)
  ::
  ++  gas
    |=  lit=(list (pair path _?>(?=(^ fil.fat) u.fil.fat)))
    ^+  fat
    ?~  lit  fat
    $(fat (put p.i.lit q.i.lit), lit t.lit)
  ::
  ++  get
    |=  pax=path
    fil:(dip pax)
  ::  Fetch file at longest existing prefix of the path
  ::
  ++  fit
    |=  pax=path
    ^+  [pax fil.fat]
    ?~  pax  [~ fil.fat]
    =/  kid  (~(get by dir.fat) i.pax)
    ?~  kid  [pax fil.fat]
    =/  low  $(fat u.kid, pax t.pax)
    ?~  +.low
      [pax fil.fat]
    low
  ::
  ++  has
    |=  pax=path
    !=(~ (get pax))
  ::  Delete subtree
  ::
  ++  lop
    |=  pax=path
    ^+  fat
    ?~  pax  fat
    |-
    ?~  t.pax  fat(dir (~(del by dir.fat) i.pax))
    =/  kid  (~(get by dir.fat) i.pax)
    ?~  kid  fat
    fat(dir (~(put by dir.fat) i.pax $(fat u.kid, pax t.pax)))
  ::
  ++  put
    |*  [pax=path dat=*]
    =>  .(dat `_?>(?=(^ fil.fat) u.fil.fat)`dat, pax `path`pax)
    |-  ^+  fat
    ?~  pax  fat(fil `dat)
    =/  kid  (~(gut by dir.fat) i.pax ^+(fat [~ ~]))
    fat(dir (~(put by dir.fat) i.pax $(fat kid, pax t.pax)))
  ::
  ++  tap
    =|  pax=path
    =|  out=(list (pair path _?>(?=(^ fil.fat) u.fil.fat)))
    |-  ^+   out
    =?  out  ?=(^ fil.fat)  :_(out [pax u.fil.fat])
    =/  dir  ~(tap by dir.fat)
    |-  ^+   out
    ?~  dir  out
    %=  $
      dir  t.dir
      out  ^$(pax (weld pax /[p.i.dir]), fat q.i.dir)
    ==
  ::  Serialize to map
  ::
  ++  tar
    (~(gas by *(map path _?>(?=(^ fil.fat) u.fil.fat))) tap)
  --
::
++  wa                                                  ::  cached compile
  |_  worm
  ++  nell  |=(ref=type (nest [%cell %noun %noun] ref)) ::  nest in cell
  ++  nest                                              ::  nest:ut, cached
    |=  [sut=type ref=type]
    ^-  [? worm]
    ?:  (~(has in nes) [sut ref])  [& +>+<]
    ?.  (~(nest ut sut) | ref)
      ~&  %nest-failed
      =+  foo=(skol ref)
      =+  bar=(skol sut)
      ~&  %nest-need
      ~>  %slog.[0 bar]
      ~&  %nest-have
      ~>  %slog.[0 foo]
      [| +>+<.$]
    [& +>+<(nes (~(put in nes) [sut ref]))]
  ::
  ++  call                                              ::  call gate
    |=  [vax=vase nam=term som=(each vase ^)]
    ^-  [vase worm]
    =^  duf  +>+<.$  (open vax nam som)
    (slap duf [%limb %$])
  ::
  ++  open                                              ::  assemble door
    |=  [vax=vase nam=term som=(each vase ^)]
    ^-  [vase worm]
    =*  key  [%cncb [[%& 2] ~] [[[%& 6] ~] [%$ 3]] ~]
    =^  dor  +>+<.$  (slap vax [%limb nam])
    =^  mes  +>+<.$  (slot 6 dor)
    =^  hip  +>+<.$
      ?-  -.som
         %&  (nest p.mes p.p.som)
         %|  (nets p.mes -.p.som)
      ==
    ?>  hip
    [[p.dor q.dor(+6 +7.som)] +>+<.$]
  ::
  ++  neat                                              ::  type compliance
    |=  [typ=type som=(each vase ^)]
    ^-  worm
    =^  hip  +>+<.$
      ?-  -.som
        %&  (nest typ p.p.som)
        %|  (nets typ -.p.som)
      ==
    ?>  hip
    +>+<.$
  ::
  ++  nets                                              ::  typeless nest
    |=  [sut=* ref=*]
    ^-  [? worm]
    ?:  (~(has in nes) [sut ref])  [& +>+<]
    =+  gat=|=([a=type b=type] (~(nest ut a) | b))
    ?.  (? (slum gat [sut ref]))
      ~&  %nets-failed
      =+  tag=`*`skol
      =+  foo=(tank (slum tag ref))
      =+  bar=(tank (slum tag sut))
      ~&  %nets-need
      ~>  %slog.[0 bar]
      ~&  %nets-have
      ~>  %slog.[0 foo]
      [| +>+<.$]
    [& +>+<.$(nes (~(put in nes) [sut ref]))]
  ::  +play: +play:ut, cached
  ::
  ++  play
    |=  [sut=type gen=hoon]
    ^-  [type worm]
    =+  old=(~(get by pay) [sut gen])
    ?^  old  [u.old +>+<.$]
    =+  new=(~(play ut sut) gen)
    [new +>+<.$(pay (~(put by pay) [sut gen] new))]
  ::  +mint: +mint:ut to noun, cached
  ::
  ++  mint
    |=  [sut=type gen=hoon]
    ^-  [(pair type nock) worm]
    =+  old=(~(get by mit) [sut gen])
    ?^  old  [u.old +>+<.$]
    =+  new=(~(mint ut sut) %noun gen)
    [new +>+<.$(mit (~(put by mit) [sut gen] new))]
  ::  +slam: +slam:ut, cached
  ::
  ++  slam
    |=  [gat=vase sam=vase]
    =/  sut=type  [%cell p.gat p.sam]
    =/  gen=hoon  [%cnsg [%$ ~] [%$ 2] [%$ 3] ~]
    =^  new=type  +>+<.$  (play sut gen)
    [[new (slum q.gat q.sam)] +>+<.$]
  ::  +slap: +slap:ut, cached
  ::
  ++  slap
    |=  [vax=vase gen=hoon]
    ^-  [vase worm]
    =^  gun  +>+<  (mint p.vax gen)
    [[p.gun .*(q.vax q.gun)] +>+<.$]
  ::  +slot: +slot:ut, cached
  ::
  ++  slot
    |=  [axe=@ vax=vase]
    ^-  [vase worm]
    =^  gun  +>+<  (mint p.vax [%$ axe])
    [[p.gun .*(q.vax [0 axe])] +>+<.$]
  ::
  ::  +slur: slam a vase with a maze
  ::
  ++  slur
    |=  [gat=vase sam=maze]
    ^-  [vase worm]
    =^  cur  +>+<.$  (slot 6 gat)
    =.  +>+<.$  (neat p.cur sam)
    (slym gat q.p.sam)
  ::  +slym: +slym:ut, cached
  ::
  ++  slym
    |=  [gat=vase sam=*]
    ^-  [vase worm]
    (slap gat(+<.q sam) [%limb %$])
  ::
  ++  sped                                              ::  specialize vase
    |=  vax=vase
    ^-  [vase worm]
    =+  ^=  gen  ^-  hoon
      ?@  q.vax    [%wtts [%base [%atom %$]] [%& 1]~]
      ?@  -.q.vax  [%wtts [%leaf %tas -.q.vax] [%& 2]~]
      [%wtts [%base %cell] [%& 1]~]
    =^  typ  +>+<.$  (play p.vax [%wtgr gen [%$ 1]])
    [[typ q.vax] +>+<.$]
  ::
  ++  spot                                              ::  slot then sped
    |=  [axe=@ vax=vase]
    ^-  [vase worm]
    =^  xav  +>+<  (slot axe vax)
    (sped xav)
  ::
  ++  stop                                              ::  sped then slot
    |=  [axe=@ vax=vase]
    ^-  [vase worm]
    =^  xav  +>+<  (sped vax)
    (slot axe xav)
  --
::
::  |part: arvo structures and engines
::
++  part
  =>  |%
      ::  $card: tagged, untyped event
      ::  $ovum: card with cause
      ::  $news: collated updates
      ::  $oped: module updates
      ::  $seed: next kernel source
      ::
      +$  news
          $:  ::  sys: installs + replacements
              ::  use: non-system files
              ::
              sys=(map path (cask))
              use=(map path (cask))
          ==
      +$  oped
        $:  lul=(unit cord)
            zus=(unit cord)
            van=(list (cask cord))
        ==
      +$  seed  [hun=(unit cord) arv=cord]
      --
  ::
  ~%  %part  ..part  ~
  |%
  ::
  +|  %engines
  ::
  ::  |eden: lifecycle and bootstrap formula generators
  ::
  ::    while unused by arvo itself, these nock formulas
  ::    bootstrap arvo and define its lifecycle.
  ::
  ::    we're creating an event series E whose lifecycle can be computed
  ::    with the urbit lifecycle formula L, `[2 [0 3] [0 2]]`.  that is:
  ::    if E is the list of events processed by a computer in its life,
  ::    its final state is S, where S is nock(E L).
  ::
  ::    in practice, the first five nouns in E are: two boot formulas,
  ::    a hoon compiler as a nock formula, the same compiler as source,
  ::    and the arvo kernel as source.
  ::
  ::    after the first five special events, we enter an iterative
  ::    sequence of regular events which continues for the rest of the
  ::    computer's life.  during this sequence, each state is a function
  ::    that, passed the next event, produces the next state.
  ::
  ::    a regular event is an $ovum, or `[date wire type data]` tuple, where
  ::    `date` is a 128-bit Urbit date; `wire` is an opaque path which
  ::    output can match to track causality; `type` is a symbol describing
  ::    the type of input; and `data` is input data specific to `type`.
  ::
  ::    in real life we don't actually run the lifecycle loop,
  ::    since real life is updated incrementally and also cares
  ::    about things like output.  we couple to the internal
  ::    structure of the state machine and work directly with
  ::    the underlying arvo engine.
  ::
  ::    this arvo core, which is at `+7` (Lisp `cddr`) of the state
  ::    function (see its public interface in `sys/arvo`), gives us
  ::    extra features, like output, which are relevant to running
  ::    a real-life urbit vm, but don't affect the formal definition.
  ::
  ::    so a real-life urbit interpreter is coupled to the shape of
  ::    the arvo core.  it becomes very hard to change this shape.
  ::    fortunately, it is not a very complex interface.
  ::
  ++  eden
    |%
    ::  +aeon: arvo lifecycle loop
    ::
    ::    the first event in a ship's log,
    ::    computing the final state from the rest of log
    ::    when invoked via the lifecycle formula: [%2 [%0 3] %0 2]
    ::
    ::    the formal urbit state is always just a gate (function)
    ::    which, passed the next event, produces the next state.
    ::
    ++  aeon
      ^-  *
      =>  ::  boot: kernel bootstrap, event 2
          ::  tale: events 3-n
          ::
          *log=[boot=* tale=*]
      !=  ::  arvo: bootstrapped kernel
          ::  epic: remainder of the log
          ::
      =+  [arvo epic]=.*(tale.log boot.log)
      |-  ^-  *
      ?@  epic  arvo
      %=  $
        epic  +.epic
        arvo  .*([arvo -.epic] [%9 2 %10 [6 %0 3] %0 2])
      ==
    ::
    ::  +boot: event 2: bootstrap a kernel from source
    ::
    ++  boot
      ^-  *
      ::
      ::  event 2 is the startup formula, which verifies the compiler
      ::  and starts the main lifecycle.
      ::
      =>  ::  fate: event 3: a nock formula producing the hoon bootstrap compiler
          ::  hoon: event 4: compiler source
          ::  arvo: event 5: kernel source
          ::  epic: event 6-n
          ::
          *log=[fate=* hoon=@ arvo=@ epic=*]
      !=
      ::
      ::  activate the compiler gate.  the product of this formula
      ::  is smaller than the formula.  so you might think we should
      ::  save the gate itself rather than the formula producing it.
      ::  but we have to run the formula at runtime, to register jets.
      ::
      ::  as always, we have to use raw nock as we have no type.
      ::  the gate is in fact ++ride.
      ::
      ~>  %slog.[0 leaf+"1-b"]
      =/  compiler-gate  .*(0 fate.log)
      ::
      ::  compile the compiler source, producing (pair span nock).
      ::  the compiler ignores its input so we use a trivial span.
      ::
      ~>  %slog.[0 leaf+"1-c (compiling compiler, wait a few minutes)"]
      =/  compiler-tool
        ~>  %bout
        .*([compiler-gate noun/hoon.log] [%9 2 %10 [6 %0 3] %0 2])
      ::
      ::  switch to the second-generation compiler.  we want to be
      ::  able to generate matching reflection nouns even if the
      ::  language changes -- the first-generation formula will
      ::  generate last-generation spans for `!>`, etc.
      ::
      ~>  %slog.[0 leaf+"1-d"]
      =.  compiler-gate  ~>(%bout .*(0 +.compiler-tool))
      ::
      ::  get the span (type) of the kernel core, which is the context
      ::  of the compiler gate.  we just compiled the compiler,
      ::  so we know the span (type) of the compiler gate.  its
      ::  context is at tree address `+>` (ie, `+7` or Lisp `cddr`).
      ::  we use the compiler again to infer this trivial program.
      ::
      ~>  %slog.[0 leaf+"1-e"]
      =/  kernel-span
        ~>  %bout
        -:.*([compiler-gate -.compiler-tool '+>'] [%9 2 %10 [6 %0 3] %0 2])
      ::
      ::  compile the arvo source against the kernel core.
      ::
      ~>  %slog.[0 leaf+"1-f"]
      =/  kernel-tool
        ~>  %bout
        .*([compiler-gate kernel-span arvo.log] [%9 2 %10 [6 %0 3] %0 2])
      ::
      ::  create the arvo kernel, whose subject is the kernel core.
      ::
      ~>  %slog.[0 leaf+"1-g"]
      ~>  %bout
      [.*(+>.compiler-gate +.kernel-tool) epic.log]
    --
  ::
  ::  |adapt
  ::
  ++  adapt
    =>  |%
        ::  deep file as source
        ::
        ++  sole  |=(a=(cask) `cord`?>(?=([%hoon @t] a) q.a))
        --
    |_  fat=(axal (cask))
    ::
    ::  +group: collate changes
    ::
    ++  group
      |=  fal=(list (pair path (cask)))
      =|  del=news
      |-  ^+   del
      ?~  fal  del
      ::  classify files, ignoring unchanged
      ::
      =*  pax  p.i.fal
      =*  dat  q.i.fal
      =/  hav  (~(get of fat) pax)
      =?  del  |(?=(~ hav) !=(u.hav dat))
         ?:  ?=([%sys *] pax)
           del(sys (~(put by sys.del) pax dat))
         del(use (~(put by use.del) pax dat))
      $(fal t.fal)
    ::  +usurp: consider self-replacement
    ::
    ++  usurp
      |=  del=news
      ^-  (unit (pair seed (list (pair path (cask)))))
      =/  hun  (~(get by sys.del) /sys/hoon)
      =/  arv  (~(get by sys.del) /sys/arvo)
      ?~  hun
        ?~  arv  ~
        `[`(sole u.arv) [/sys/arvo u.arv] ~]
      =/  rav
        ~|  %usurp-hoon-no-arvo
        ((bond |.((need (~(get of fat) /sys/arvo)))) arv)
      ~!  rav
      :+  ~
        [`(sole u.hun) (sole rav)]
      [[/sys/arvo rav] [/sys/hoon u.hun] ~]
    ::  +adorn: augment capabilities
    ::
    ++  adorn
      |=  [del=news all=?]
      ^-  (pair oped _fat)
      ::  lull: shared structures
      ::
      =^  lul  fat
        ?^  hav=(~(get by sys.del) /sys/lull)
          :-  `(sole u.hav)
          (~(put of fat) /sys/lull u.hav)
        :_  fat
        ~|  %adorn-no-lull
        ?.(all ~ `(sole (need (~(get of fat) /sys/lull))))
      ::  zuse: shared library
      ::
      ::    %lull is the subject of %zuse; force all if we have a new %lull
      ::
      =.  all  |(all ?=(^ lul))
      =^  zus  fat
        ?^  hav=(~(get by sys.del) /sys/zuse)
          :-  `(sole u.hav)
          (~(put of fat) /sys/zuse u.hav)
        :_  fat
        ~|  %adorn-no-zuse
        ?.(all ~ `(sole (need (~(get of fat) /sys/zuse))))
      ::  kernel modules
      ::
      ::    %zuse is the subject of the vanes; force all if we have a new %zuse
      ::
      =.  all  |(all ?=(^ zus))
      =|  nav=(map term cord)
      =?  nav  all
        %-  ~(gas by nav)
        %+  turn
          ~(tap by dir:(~(dip of fat) /sys/vane))
        |=([name=@ta _fat] [`@tas`name (sole (need fil))])
      ::
      =^  new  fat
        %^    spin
            %+  skim  ~(tap by sys.del)
            |=([p=path *] ?=([%sys %vane @tas ~] p))
          fat
        |=  [[p=path q=(cask)] taf=_fat]
        ^-  (pair (cask cord) _fat)
        ?>  ?=([%sys %vane @tas ~] p)
        =*  nam  i.t.t.p
        ?>  ((sane %tas) nam)
        [[`@tas`nam (sole q)] (~(put of taf) p q)]
      ::
      =;  van
        [[lul zus van] fat]
      %+  sort  ~(tap by (~(gas by nav) new))
      |=([[a=@tas *] [b=@tas *]] (aor a b))
    -- :: adapt
  ::
  ::  |me: dynamic analysis
  ::
  ++  me
    ~/  %me
    |_  ::  sac: compiler cache
        ::  pyt: cached types
        ::
        [sac=worm vil=vile]
    ::  +refine-moves: move list from vase
    ::
    ++  refine-moves
      |=  vax=vase
      ^-  (pair (list move) worm)
      ?:  =(~ q.vax)  [~ sac]
      =^  hed  sac  (~(slot wa sac) 2 vax)
      =^  tal  sac  (~(slot wa sac) 3 vax)
      =^  mov  sac  (refine-move hed)
      =^  moz  sac  $(vax tal)
      [[mov moz] sac]
    ::  +refine-move: move from vase
    ::
    ++  refine-move
      |=  vax=vase
      ^-  (pair move worm)
      ~>  %mean.'bad-move'
      =^  hip  sac  (~(nell wa sac) p.vax)
      ?.  hip
        ~>(%mean.'not-cell' !!)
      =/  duc
        ~>  %mean.'bad-duct'
        ;;(duct -.q.vax)
      ::
      ::  yat: specialized ball vase
      ::
      =^  yat  sac  (~(spot wa sac) 3 vax)
      =^  del  sac  (refine-ball yat)
      [[duc del] sac]
    ::  +refine-ball: ball from vase
    ::
    ++  refine-ball
      |=  vax=vase
      ^-  (pair ball worm)
      ?+    q.vax
          ~>  %mean.'bad-ball'
          ~_  (sell vax)
          !!
      ::
          [%give card]
        ::  yed: vase containing card
        ::  hil: card as maze
        ::
        =^  yed  sac  (~(spot wa sac) 3 vax)
        =^  hil  sac  (refine-card yed)
        [[%give hil] sac]
      ::
          [%pass wire=* vane=term card]
        =/  =wire
          ~>  %mean.'bad-wire'
          ;;(wire wire.q.vax)
        =/  vane
          ~>  %mean.'bad-vane-label'
          ?>  ((sane %tas) vane.q.vax)
          vane.q.vax
        ::
        ::  yed: vase containing card
        ::  hil: card as maze
        ::
        =^  xav  sac  (~(spot wa sac) 7 vax)
        =^  yed  sac  (~(spot wa sac) 3 xav)
        =^  hil  sac  (refine-card yed)
        [[%pass wire vane hil] sac]
      ::
          [%slip vane=term card]
        =/  vane
          ~>  %mean.'bad-vane-label'
          ?>  ((sane %tas) vane.q.vax)
          vane.q.vax
        ::
        ::  yed: vase containing card
        ::  hil: card as maze
        ::
        =^  xav  sac  (~(spot wa sac) 3 vax)
        =^  yed  sac  (~(spot wa sac) 3 xav)
        =^  hil  sac  (refine-card yed)
        [[%slip vane hil] sac]
      ::
          [%hurl goof=^ ball=*]
        =/  =goof
          =/  mote  -.goof.q.vax
          ?>  ?&  ?=(@ mote)
                  ((sane %tas) mote)
              ==
          [mote ;;(tang +.goof.q.vax)]
        ::
        =^  bal  sac
          =^  lab  sac  (~(spot wa sac) 7 vax)
          $(vax lab)
        ::
        ?>  ?=(?(%pass %give) -.p.bal)
        [[%hurl goof p.bal] sac]
      ==
    ::  +refine-card: card from vase
    ::
    ++  refine-card
      |=  vax=vase
      ^-  (pair maze worm)
      ~>  %mean.'bad-card'
      =^  hip  sac  (~(nell wa sac) p.vax)
      ?>  hip
      ?.  ?=(%meta -.q.vax)
        ::
        ::  for a non-meta card, the maze is the vase
        ::
        [[%& vax] sac]
      ~>  %mean.'bad-meta'
      ::
      ::  tiv: vase of vase of card
      ::  typ: vase of span
      ::
      =^  tiv  sac  (~(slot wa sac) 3 vax)
      =^  hip  sac  (~(nell wa sac) p.tiv)
      ?>  hip
      =^  typ  sac  (~(slot wa sac) 2 tiv)
      =.  sac  (~(neat wa sac) typ.vil [%& typ])
      ::
      ::  support for meta-meta-cards has been removed
      ::
      ?>  ?=(meta q.tiv)
      [[%| q.tiv] sac]
    --
  ::
  ::  |va: vane engine
  ::
  ++  va
    =>  ~%  %va-ctx  ..va  ~
        |%
        +$  vane-sample  [now=@da eny=@uvJ rof=rook]
        ::
        ++  smit
          |=  [cap=tape sub=vase pax=path txt=@t]
          ^-  vase
          ~>  %slog.[0 leaf/"{cap}: {(scow uv+(mug txt))}"]
          ~>  %bout
          %-  road  |.
          ~_  leaf/"{cap}: build failed"
          (slap sub (rain pax txt))
        ::
        ++  create
          ~/  %create
          |=  [our=ship zus=vase lal=term pax=path txt=@t]
          ^-  vase
          =/  cap  "vane: %{(trip lal)}"
          (slym (smit cap zus pax txt) our)
        ::
        ++  settle
          ~/  %settle
          |=  van=vase
          ^-  (pair vase worm)
          =|  sac=worm
          =^  rig=vase  sac  (~(slym wa sac) van *vane-sample)
          =^  gat=vase  sac  (~(slap wa sac) rig [%limb %scry])
          =^  pro=vase  sac  (~(slap wa sac) gat [%limb %$])
          [van +:(~(mint wa sac) p.pro [%$ 7])]
        ::
        ::  XX pass identity to preserve behavior?
        ::
        ++  update
          ~/  %update
          |=  [las=vase nex=vase]
          ^-  vase
          =/  sam=vase  (slap (slym las *vane-sample) [%limb %stay])
          =/  gat=vase  (slap (slym nex *vane-sample) [%limb %load])
          (slam gat sam)
        --
    ::
    ~%  %va  ..va  ~
    |_  [vil=vile vax=vase sac=worm]
    ::
    ::  |plow:va: operate in time and space
    ::
    ++  plow
      ~/  %plow
      |=  [now=@da rok=rook]
      ~%  %plow-core  +  ~
      |%
      ::  +peek:plow:va: read from a local namespace
      ::
      ++  peek
        ~/  %peek
        ^-  rook
        |=  [lyc=gang pov=path omen]
        ^-  (unit (unit (cask meta)))
        ::  namespace reads receive no entropy
        ::
        =/  sam=vane-sample  [now *@uvJ rok]
        =^  rig  sac
          ~>  %mean.'peek: activation failed'
          (~(slym wa sac) vax sam)
        =^  gat  sac
          ~>  %mean.'peek: pull failed'
          (~(slap wa sac) rig [%limb %scry])
        ::
        =/  mas=[gang path view beam]  [lyc pov vis bem]
        ::
        =^  pro  sac
          ~>  %mean.'peek: call failed'
          (~(slym wa sac) gat mas)
        ?~  q.pro  ~
        ?~  +.q.pro  [~ ~]
        =^  dat  sac  (~(slot wa sac) 7 pro)
        ``[(,mark -.q.dat) (,^ +.q.dat)]
      ::
      ::  |spin:plow:va: move statefully
      ::
      ++  spin
        |=  [hen=duct eny=@uvJ dud=(unit goof)]
        =*  duc  [duc.vil hen]
        =*  err  [dud.vil dud]
        =/  sam=vane-sample  [now eny rok]
        =^  rig  sac
          ~>  %mean.'spin: activation failed'
          (~(slym wa sac) vax sam)
        ::
        =>  |%
            ::  +slid: cons a vase onto a maze
            ::
            ++  slid
              |=  [hed=vase tal=maze]
              ^-  maze
              ?-  -.tal
                %&  [%& (slop hed p.tal)]
                %|  [%| [%cell p.hed p.p.tal] [q.hed q.p.tal]]
              ==
            --
        |%
        ::  +peel:spin:plow:va: extract products, finalize vane
        ::
        ++  peel
          |=  pro=vase
          ^-  (pair [vase vase] worm)
          =^  moz  sac  (~(slot wa sac) 2 pro)
          =^  vem  sac  (~(slot wa sac) 3 pro)
          ::  replace vane sample with default to plug leak
          ::
          =.  +<.q.vem  *vane-sample
          [[moz vem] sac]
        ::  +call:spin:plow:va: advance statefully
        ::
        ++  call
          |=  task=maze
          ^-  (pair [vase vase] worm)
          ~>  %mean.'call: failed'
          =^  gat  sac
            (~(slap wa sac) rig [%limb %call])
          ::
          ::  sample is [duct (unit goof) (hobo task)]
          ::
          =/  sam=maze
            (slid duc (slid err task))
          =^  pro  sac  (~(slur wa sac) gat sam)
          (peel pro)
        ::  +take:spin:plow:va: retreat statefully
        ::
        ++  take
          |=  [=wire from=term gift=maze]
          ^-  (pair [vase vase] worm)
          ~>  %mean.'take: failed'
          =^  gat  sac
            (~(slap wa sac) rig [%limb %take])
          =/  src=vase
            [[%atom %tas `from] from]
          ::
          ::  sample is [wire duct (unit goof) sign=[term gift]]
          ::
          =/  sam=maze
            =*  tea  [wir.vil wire]
            (slid tea (slid duc (slid err (slid src gift))))
          =^  pro  sac  (~(slur wa sac) gat sam)
          (peel pro)
        --
      --
    --
  ::
  ::  |le: arvo event-loop engine
  ::
  ++  le
    ~%  %le  ..le  ~
    =|  $:  ::  run: list of worklists
            ::  out: pending output
            ::  gem: worklist metadata
            ::  dud: propagate error
            ::  but: reboot signal
            ::
            ::
            run=(list plan)
            out=(list ovum)
            gem=germ
            dud=(unit goof)
            $=  but  %-  unit
                     $:  gat=$-(heir (trap ^))
                         kel=(list (pair path (cask)))
                         fil=(list (pair path (cask)))
                     ==
        ==
    ::
    |_  [[pit=vase vil=vile] soul]
    +*  this  .
        sol   +<+
    ::
    ::  +abet: finalize loop
    ::
    ++  abet
      ^-  (each (pair (list ovum) soul) (trap ^))
      ?~  but
        ^-   [%& (pair (list ovum) soul)]
        &/[(flop out) sol]
      |/(gat.u.but [arvo [run out [kel fil]:u.but] sol])
    ::  +poke: prepare a worklist-of-one from outside
    ::
    ++  poke
      |=  =ovum
      ^+  this
      ~>  %mean.'arvo: poke crashed'
      ~?  !lac.fad  ["" %unix p.card.ovum wire.ovum now]
      (poke:pith ovum)
    ::
    ++  jump
      |=  =debt
      ^+  this
      =:  run  run.debt
          out  out.debt
        ==
      ::  apply remaining update
      ::
      =.  ..this  (~(lod what:pith fil.debt) kel.debt)
      ::  send upgrade notifications
      ::
      =+  [wir car]=[/arvo vega/~]
      =.  ..this  (xeno:pith $/wir car)
      (emit $/~ (spam:pith wir !>(car)))
    ::  +emit: enqueue a worklist with source
    ::
    ++  emit
      |=  pan=plan
      this(run [pan run])
    ::  +loop: until done
    ::
    ++  loop
      ^+  abet
      ?:  ?|  ?=(~ run)
              ?=(^ but)
          ==
        abet
      ?:  =(~ q.i.run)    :: XX TMI
        loop(run t.run)
      =.  dud  ~
      =.  gem  p.i.run
      =^  mov=move  q.i.run  q.i.run
      loop:(step mov)
    ::  +step: advance the loop one step by routing a move
    ::
    ++  step
      |=  =move
      ^+  this
      ::
      ~?  &(!lac.fad ?=(^ dud))  %goof
      ::
      ?-  -.ball.move
      ::
      ::  %pass: forward move
      ::
          %pass
        =*  wire  wire.ball.move
        =*  duct  duct.move
        =*  vane  vane.note.ball.move
        =*  task  task.note.ball.move
        ::
        ~?  &(!lac.fad !=(%$ vane.gem))
          :-  (runt [(lent bars.gem) '|'] "")
          :^  %pass  [vane.gem vane]
            ?:  ?=(?(%deal %deal-gall) +>-.task)
              :-  :-  +>-.task
                  ;;([[ship ship path] term term] [+>+< +>+>- +>+>+<]:task)
              wire
            [(symp +>-.task) wire]
          duct
        ::
        ::  cons source onto wire, and wire onto duct
        ::
        (call [[vane.gem wire] duct] vane task)
      ::
      ::  %slip: lateral move
      ::
          %slip
        =*  duct  duct.move
        =*  vane  vane.note.ball.move
        =*  task  task.note.ball.move
        ::
        ~?  !lac.fad
          :-  (runt [(lent bars.gem) '|'] "")
          [%slip [vane.gem vane] (symp +>-.task) duct]
        ::
        (call duct vane task)
      ::
      ::  %give: return move
      ::
          %give
        ?.  ?=(^ duct.move)
          ~>(%mean.'give-no-duct' !!)
        ::
        =/  wire  i.duct.move
        =/  duct  t.duct.move
        =*  gift  gift.ball.move
        ::
        =^  way=term  wire
          ~|  [%give duct.move (symp -.q.p.gift)]
          ?>(?=(^ wire) wire)
        ::
        ~?  &(!lac.fad !=(%$ way) |(!=(%blit +>-.gift) !=(%d vane.gem)))
          :-  (runt [(lent bars.gem) '|'] "")
          :^  %give  vane.gem
            ?:  ?=(%unto +>-.gift)
              [+>-.gift (symp +>+<.gift)]
            (symp +>-.gift)
          duct.move
        ::
        (take duct wire way gift)
      ::
      ::  %hurl: action with error
      ::
          %hurl
        %=  $
          dud       `goof.ball.move
          ball.move  wite.ball.move
        ==
      ==
    ::  +whey: measure memory usage
    ::
    ++  whey
      ^-  mass
      =;  sam=(list mass)
        :+  %arvo  %|
        :~  :+  %hoon  %|
            :~  one+&+..bloq
                two+&+..turn
                tri+&+..year
                qua+&+..sane
                pen+&+..ride
            ==
            hex+&+..part
            pit+&+pit
            lull+|+[dot+&+q typ+&+p ~]:lul.mod
            zuse+|+[dot+&+q typ+&+p ~]:zus.mod
            vane+|+sam
        ==
      ::
      %+  turn
        (sort ~(tap by van.mod) |=([[a=@tas *] [b=@tas *]] (aor a b)))
      =/  bem=beam  [[our %$ da+now] //whey]
      |=  [nam=term =vane]
      =;  mas=(list mass)
        nam^|+(welp mas [dot+&+q.vase typ+&+p.vase sac+&+worm ~]:vane)
      ?~  met=(peek [~ ~] / [nam %x] bem)  ~
      ?~  u.met  ~
      ~|  mass+nam
      ;;((list mass) q.q.u.u.met)
    ::  +peek: read from the entire namespace
    ::
    ++  peek
      ^-  rook
      |=  [lyc=gang pov=path omen]
      ^-  (unit (unit (cask meta)))
      ::  vane and care may be concatenated
      ::
      =/  [way=term car=term]
        ?^  vis  vis
        ?.  =(2 (met 3 vis))
          [vis %$]
        [(end 3 vis) (rsh 3 vis)]
      ::
      ?:  ?=(%$ way)
        (peek:pith lyc pov car bem)
      ::
      =.  way  (grow way)
      ?~  van=(~(get by van.mod) way)
        ~
      %.  [lyc pov car bem]
      peek:spin:(~(plow va [vil u.van]) now peek)
    ::  +call: advance to target
    ::
    ++  call
      |=  [=duct way=term task=maze]
      ^+  this
      ?:  ?=(%$ way)
        ~>  %mean.'arvo: call:pith failed'
        %-  call:pith
        ~>  %mean.'call: bad waif'
        ;;(waif q.p.task)
      ::
      =.  way  (grow way)
      %+  push  [way duct bars.gem]
      ~|  bar-stack=`(list ^duct)`[duct bars.gem]
      %.  task
      call:(spin:(plow way) duct eny dud)
    ::  +take: retreat along call-stack
    ::
    ++  take
      |=  [=duct =wire way=term gift=maze]
      ^+  this
      ?:  ?=(%$ way)
        ::
        ::  the caller was Outside
        ::
        ?>  ?=(~ duct)
        (xeno:pith wire ;;(card q.p.gift))
      ::  the caller was a vane
      ::
      =.  way  (grow way)
      %+  push  [way duct bars.gem]
      ::
      ::  cons source onto .gift to make a $sign
      ::
      ~|  wire=wire
      ~|  bar-stack=`(list ^duct)`[duct bars.gem]
      %.  [wire [vane.gem gift]]
      take:(spin:(plow way) duct eny dud)
    ::  +push: finalize an individual step
    ::
    ++  push
      |=  [gum=germ [zom=vase vax=vase] sac=worm]
      ^+  this
      =^  moz  sac
        (~(refine-moves me sac vil) zom)
      =.  van.mod  (~(put by van.mod) vane.gum [vax sac])
      (emit `plan`[`germ`gum `(list move)`moz])
    ::  +plow: operate on a vane, in time and space
    ::
    ++  plow
      |=  way=term
      ~|  [%plow-failed way]
      =/  =vane
        ~|  [%missing-vane way]
        (~(got by van.mod) way)
      (~(plow va [vil vane]) now peek)
    ::
    ::  |pith: operate on arvo internals
    ::
    ++  pith
      |%
      ++  gest
        |=  =ovum
        ^-  $>(%pass ball)
        =^  way=term  wire.ovum  wire.ovum
        ::
        ::  %$: default, routed to arvo-proper as trivial vase
        ::  @:  route to vane as $hobo
        ::
        =/  =vase
          ?-  way
            %$  noun/card.ovum
            @   [cell/[atom/tas/`%soft %noun] soft/card.ovum]
          ==
        [%pass wire.ovum way &/vase]
      ::
      ::  |what: update engine
      ::
      ::    +kel: (maybe) initiate a kernel update
      ::    +lod: continue with update after kernel +load
      ::    +mod: update the modules of the kernel
      ::
      ++  what
        |_  fil=(list (pair path (cask)))
        ::
        ++  kel
          ^+  ..pith
          =/  del  (~(group adapt fat.mod.sol) fil)
          =/  tub  (~(usurp adapt fat.mod.sol) del)
          ?~  tub
            (mod del |)
          =/  gat  (boot kel.ver.zen [hun arv]:p.u.tub)
          ..pith(but `[gat q.u.tub fil])
        ::
        ++  lod
          |=  kel=(list (pair path (cask)))
          ^+  ..pith
          =.  fat.mod.sol  (~(gas of fat.mod.sol) kel)
          %+  mod
            (~(group adapt fat.mod.sol) fil)
          %+  lien  kel
          |=  [p=path *]
          ?=([%sys ?(%arvo %hoon) *] p)
        ::
        ++  mod
          |=  [del=news all=?]
          ^+  ..pith
          =^  job=oped  fat.mod.sol  (~(adorn adapt fat.mod.sol) del all)
          =?  lul.mod.sol  ?=(^ lul.job)
            (smit:va "lull" pit /sys/lull/hoon u.lul.job)
          =?  zus.mod.sol  ?=(^ zus.job)
            (smit:va "zuse" lul.mod.sol /sys/zuse/hoon u.zus.job)
          %-  %+  need:wyrd   kel.ver.zen
              :~  lull/;;(@ud q:(slap lul.mod.sol limb/%lull))
                  zuse/;;(@ud q:(slap zus.mod.sol limb/%zuse))
              ==
          %=    ..pith
              van.mod
            %+  roll  van.job
            |=  [[nam=term txt=cord] van=_van.mod.sol]
            ^+  van
            =/  nex  (create:va our zus.mod.sol nam /sys/vane/[nam]/hoon txt)
            =/  nav  (~(get by van) nam)
            =?  nex  ?=(^ nav)  (update:va vase.u.nav nex)
            (~(put by van) nam (settle:va nex))
          ==
        --
      ::
      ++  call
        |=  =waif
        ^+  ..pith
        ?^  dud  ~>(%mean.'pith: goof' !!)
        ?-  -.waif
        ::
        ::  %trim: clear state
        ::
        ::    clears compiler caches if high-priority
        ::    XX add separate $wasp if this should happen last
        ::
          %trim  =?  van.mod  =(0 p.waif)
                   (~(run by van.mod) |=(=vane vane(worm *worm)))
                 (emit $/~ (spam /arvo !>(waif)))
        ::
          %verb  ..pith(lac.fad ?~(p.waif !lac.fad u.p.waif))
          %what  ~(kel what p.waif)
          %whey  ..pith(out [[//arvo mass/whey] out])
        ::
            %whiz
          ..pith(van.mod (~(run by van.mod) |=(vane (settle:va:part vase))))
        ==
      ::
      ++  peek
        ^-  roon
        |=  [lyc=gang pov=path car=term bem=beam]
        ^-  (unit (unit cage))
        ?.  ?&  =(our p.bem)
                ?=(%$ q.bem)
                =([%da now] r.bem)
            ==
          ~
        ?+  s.bem  ~
          [%whey ~]      ``mass/!>(whey)
          [%fad %lac ~]  ``noun/!>(lac.fad)
          [%zen %lag ~]  ``noun/!>(lag.zen)
          [%zen %ver ~]  ``noun/!>(ver.zen)
          [%mod %fat *]  ``noun/!>((~(dip of fat.mod) t.t.s.bem))
        ==
      ::
      ++  poke
        |=  =ovum
        ^+  ..pith
        ?~  wire.ovum
          ~>(%mean.'pith: bad wire' !!)
        ::
        ?.  ?=(?(%crud %wack %wyrd) p.card.ovum)
          (emit $/~ [*duct (gest ovum)] ~)
        ::
        =/  buz  ~>  %mean.'pith: bad wasp'
                 ;;(wasp card.ovum)
        ?-  -.buz
        ::
        ::  %crud: forward error notification
        ::
          %crud  =?  lag.zen  ?&  ?=(%exit mote.goof.buz)
                                  ?=(^ tang.goof.buz)
                                  ?=([%leaf *] i.tang.goof.buz)
                                  ?=(%wyrd (crip p.i.tang.goof.buz))
                              ==
                   ~&(%lagging &)
                 (emit $/~ [*duct hurl/[goof.buz (gest ovum.buz)]] ~)
        ::
        ::  XX review
        ::
          %wack  ..pith(eny (shaz (cat 3 eny p.buz)))
        ::
        ::  %wyrd: check for runtime kelvin compatibility
        ::
          %wyrd  ?.  (sane:wyrd kel.p.buz)
                   ~>(%mean.'wyrd: insane' !!)
                 %-  %+  need:wyrd  kel.p.buz
                     ^-  wynn
                     :~  hoon/hoon-version
                         arvo/arvo
                         lull/;;(@ud q:(slap lul.mod limb/%lull))
                         zuse/;;(@ud q:(slap zus.mod limb/%zuse))
                     ==
                 =?  lag.zen  !=(rev.ver.zen rev.p.buz)  ~&(%unlagging |)
                 ..pith(ver.zen p.buz)
        ==
      ::
      ++  spam
        |=  [=wire =vase]
        ^-  (list move)
        %+  turn
          %+  sort  ~(tap by van.mod)
          |=([[a=@tas *] [b=@tas *]] (aor a b))
        |=([way=term *] `move`[*duct %pass wire way `maze`&/vase])
      ::
      ++  xeno
        |=  =ovum
        ^+  ..pith
        ..pith(out [ovum out])
      --
    --
  --
::
++  symp                                                ::  symbol or empty
  |=  a=*  ^-  @tas
  ?.(&(?=(@ a) ((sane %tas) a)) %$ a)
::
++  boot
  |=  [kel=wynn hun=(unit @t) van=@t]
  ^-  $-(heir (trap ^))
  ~>  %mean.'arvo: upgrade failed'
  ~>  %slog.[0 'arvo: beginning upgrade']
  ?~  hun
    =/  gat
      ~>  %slog.[0 'arvo: compiling next arvo']
      ~>  %bout
      %-  road  |.
      (slap !>(..ride) (rain /sys/arvo/hoon van))
    =/  lod
      (slap (slot 7 gat) [%limb %load])
    |=  =heir
    |.  ~>  %slog.[0 'arvo: +load next']
    ;;(^ q:(slam lod !>(heir)))
  ::
  ::  hyp: hoon core type
  ::  hoc: hoon core
  ::  cop: compiler gate
  ::
  =/  [hyp=* hoc=* cop=*]
    ::  compile new hoon.hoon source with the current compiler
    ::
    =/  raw
      ~>  %slog.[0 'arvo: compiling hoon']
      ~>  %bout
      (road |.((ride %noun u.hun)))
    ::  activate the new compiler gate, producing +ride
    ::
    =/  cop  .*(0 +.raw)
    ::  find the kelvin version number of the new compiler
    ::
    =/  nex
      ;;(@ .*(cop q:(~(mint ut p.raw) %noun [%limb %hoon-version])))
    ::  require single-step upgrade
    ::
    ?.  |(=(nex hoon-version) =(+(nex) hoon-version))
      =*  ud  |=(a=@ (scow %ud a))
      ~_  leaf/"cannot upgrade to hoon %{(ud nex)} from %{(ud hoon-version)}"
      !!
    ::  require runtime compatibility
    ::
    %-  (need:wyrd kel [hoon/nex ~])
    ::
    ::  if we're upgrading language versions, recompile the compiler
    ::
    =^  hot=*  cop
      ?:  =(nex hoon-version)
        [raw cop]
      =/  hot
        ~>  %slog.[0 leaf/"arvo: recompiling hoon %{(scow %ud nex)}"]
        ~>  %bout
        (road |.((slum cop [%noun u.hun])))
      [hot .*(0 +.hot)]
    ::  extract the hoon core from the outer gate (+ride)
    ::
    =/  hoc  .*(cop [%0 7])
    ::  compute the type of the hoon.hoon core
    ::
    =/  hyp  -:(slum cop [-.hot '+>'])
    ::
    [hyp hoc cop]
  ::
  ::  compile arvo
  ::
  =/  rav
    ~>  %slog.[0 'arvo: compiling next arvo']
    ~>  %bout
    (road |.((slum cop [hyp van])))
  ::  activate arvo and extract the arvo core from the outer gate
  ::
  =/  voc  .*(hoc [%7 +.rav %0 7])
  ::
  ::  extract the upgrade gate +load
  ::
  =/  lod
    ::  vip: type of the arvo.hoon core
    ::  fol: formula for the +load gate
    ::
    =/  vip  -:(slum cop [-.rav '+>'])
    =/  fol  +:(slum cop [vip 'load'])
    ::  produce the upgrade gate
    ::
    .*(voc fol)
  ::
  |=  =heir
  |.  ~>  %slog.[1 'arvo: +load next']
  ;;(^ (slum lod heir))
::
++  viol                                                ::  vane tools
  |=  but=type
  ^-  vile
  =+  pal=|=(a=@t ^-(type (~(play ut but) (vice a))))
  :*  typ=(pal '$:type')
      duc=(pal '$:duct')
      wir=(pal '$:wire')
      dud=(pal '=<($ (unit goof))')  ::  XX misparse
  ==
::
++  grow
  |=  way=term
  ?+  way  way
    %a  %ames
    %b  %behn
    %c  %clay
    %d  %dill
    %e  %eyre
    %g  %gall
    %i  %iris
    %j  %jael
    %k  %khan
    %l  %lick
  ==
--  =>
::
::  cached reflexives
::
=/  pit=vase  !>(..part)
=/  vil=vile  (viol p.pit)
::
::  arvo state, as a discriminable sample
::
=|  [_arvo soul]
=*  sol  ->
|%
::  +load: upgrade from previous state
::
++  load                                                ::   +4
  |=  hir=$<(%grub heir)
  ^-  ^
  ~|  %load
  ::  store persistent state
  ::
  =.  sol
    ?-  -.hir
      ?(%240 %239 %238 %237)  soul.hir
    ==
  ::  clear compiler caches
  ::
  =.  van.mod  (~(run by van.mod) |=(=vane vane(worm *worm)))
  ::
  %-  %+  need:wyrd  kel.ver.zen
      ^-  wynn
      :~  hoon/hoon-version
          arvo/arvo
          lull/;;(@ud q:(slap lul.mod limb/%lull))
          zuse/;;(@ud q:(slap zus.mod limb/%zuse))
      ==
  ::  restore working state and resume
  ::
  =/  zef=(each (pair (list ovum) soul) (trap ^))
    loop:(~(jump le:part [pit vil] sol) debt.hir)
  ?-  -.zef
    %&  [p.p.zef ..load(sol q.p.zef)]
    %|  $:p.zef
  ==
::
::  +peek: external inspect
::
++  peek                                                ::  +22
  |=  $:  lyc=gang
          $=  nom
          %+  each  path
          $%  [%once vis=view syd=desk tyl=spur]
              [%beam omen]  :: XX unfortunate naming
          ==
      ==
  ^-  (unit (cask))
  =/  hap=(unit [pat=? omen])
    ?-  nom
      [%& *]        ?~(mon=(de-omen p.nom) ~ `[| u.mon])
      [%| %beam *]  `[| vis bem]:p.nom
      [%| %once *]  `[& vis.p.nom [our syd.p.nom da/now] tyl.p.nom]
    ==
  ::
  ?~  hap  ~
  =/  pro  (~(peek le:part [pit vil] sol) lyc / [vis bem]:u.hap)
  ?:  |(?=(~ pro) ?=(~ u.pro))  ~
  =/  dat=(cask)  [p q.q]:u.u.pro
  ?.  pat.u.hap  `dat
  `[%omen (en-omen [vis bem]:u.hap) dat]
::
::  +poke: external apply
::
++  poke                                                ::  +23
  |=  [now=@da ovo=ovum]
  ^-  ^
  ::  this assertion is not yet viable, as vere's timestamps
  ::  are too unreliable. sad!
  ::
  :: ?.  (gth now now.sol)
  ::   ~|  poke/[now=now last=now.sol wire.ovo p.card.ovo]
  ::   ~>(%mean.'time-marches-on' !!)
  ::
  =:  eny.sol  (shaz (cat 3 eny now))  ::  XX review
      now.sol  now
    ==
  ::
  ~|  poke/p.card.ovo
  =/  zef=(each (pair (list ovum) soul) (trap ^))
    loop:(~(poke le:part [pit vil] sol) ovo)
  ?-  -.zef
    %&  [p.p.zef ..poke(sol q.p.zef)]
    %|  $:p.zef
  ==
::
::  +wish: external compute
::
++  wish                                                ::  +10
  |=  txt=@
  q:(slap zus.mod (ream txt))
--  =>
::
::  larval stage
::
::    The true Arvo kernel knows who it is. It should not *maybe*
::    have an identity, nor should it contain multitudes. This outer
::    kernel exists to accumulate identity, entropy, and the
::    standard library. Upon having done so, it upgrades itself into
::    the true Arvo kernel. Subsequent upgrades will fall through
::    the larval stage directly into the actual kernel.
::
::    For convenience, this larval stage also supports hoon compilation
::    with +wish and vane installation with the %veer event.
::
=>  |%
    ++  molt
      |=  [now=@da grub]
      ^-  (unit $>(_arvo heir))
      ?.  &(?=(^ who) ?=(^ eny) ?=(^ ver) ?=(^ fat) ?=(^ lul) ?=(^ zus))
        ~
      =/  lul  $:u.lul
      =/  zus  $:u.zus
      %-  %+  need:wyrd  kel.u.ver
          ^-  wynn
          :~  hoon/hoon-version
              arvo/arvo
              lull/;;(@ud q:(slap lul limb/%lull))
              zuse/;;(@ud q:(slap zus limb/%zuse))
          ==
      =/  nav  %-  ~(run by van)
               |=(a=(trap vase) (settle:va:part (slym $:a u.who)))
      :^  ~  arvo  *debt
      [[u.who now u.eny] [lac] [u.ver |] u.fat lul zus nav]
    ::
    ++  what
      =>  |%
          ++  smit
            |=  [cap=tape sub=(trap vase) pax=path txt=@t]
            ^-  (trap vase)
            ~>  %slog.[0 leaf/"{cap}: {(scow uv+(mug txt))}"]
            %-  road  |.
            ~_  leaf/"{cap}: build failed"
            (swat sub (rain pax txt))
          --
      ::
      |=  [grub fil=(list (pair path (cask)))]
      ^-  grub
      =*  gub  +<-
      =/  taf  (fall fat *(axal (cask)))
      =/  del  (~(group adapt:part taf) fil)
      =/  tub  (~(usurp adapt:part taf) del)
      ?:  &(?=(^ dir.taf) ?=(^ tub))
        ~>(%mean.'arvo: larval reboot' !!)    :: XX support
      ::
      ::  require, and unconditionally adopt, initial kernel source
      ::
      =?  taf  =(~ dir.taf)     ::  XX TMI
        ~|  %larval-need-kernel
        ?>  &(?=(^ tub) ?=(^ hun.p.u.tub))
        (~(gas of taf) q.u.tub)
      ::
      =^  job=oped:part  taf  (~(adorn adapt:part taf) del |)
      =?  lul  ?=(^ lul.job)
       `(smit "lull" |.(pit) /sys/lull/hoon u.lul.job)
      =?  zus  ?=(^ zus.job)
        ?.  ?=(^ lul)
         ~|(%larval-need-lull !!)
        `(smit "zuse" u.lul /sys/zuse/hoon u.zus.job)
      =?  van  !=(~ van.job)    ::  XX TMI
        ?.  ?=(^ zus)
         ~|(%larval-need-zuse !!)
        %+  roll  van.job
        |=  [[nam=term txt=cord] =_van]
        ^+  van
        %+  ~(put by van)  nam
        (smit "vane: %{(trip nam)}" u.zus /sys/vane/[nam]/hoon txt)
      gub(fat `taf)
    --
::
::  larval state, as a discriminable sample
::
=|  [%grub _arvo grub]
=*  gub  ->+
::
|%
++  load                                                ::   +4
  |=  hir=heir
  ?:  ?=(%grub -.hir)
    ~>(%mean.'arvo: larval reboot' !!)    :: XX support
  (^load hir)
::
++  peek  _~                                            ::  +22
++  poke                                                ::  +23
  |=  [now=@da ovo=ovum]
  ^-  ^
  ~|  poke/p.card.ovo
  =/  wip
    ~>  %mean.'arvo: bad wisp'
    ;;(wisp card.ovo)
  ::
  =.  ..poke
    ?-    -.wip
      %verb  ..poke(lac ?~(p.wip !lac u.p.wip))
      %wack  ..poke(eny `p.wip)
      %what  ..poke(gub (what gub p.wip))
      %whom  ..poke(who ~|(%whom-once ?>(?=(~ who) `p.wip)))
    ::
      %wyrd  ?.  (sane:wyrd kel.p.wip)
                   ~>(%mean.'wyrd: insane' !!)
             %-  %+  need:wyrd  kel.p.wip
                 ^-  wynn
                 :*  hoon/hoon-version
                     arvo/arvo
                     ?~  lul  ~
                     :-  lull/;;(@ud q:(slap $:u.lul limb/%lull))
                     ?~  zus  ~
                     [zuse/;;(@ud q:(slap $:u.zus limb/%zuse)) ~]
                 ==
             ..poke(ver `p.wip)
    ==
  ::
  ::  upgrade once we've accumulated necessary state
  ::
  ?~  hir=(molt now gub)
    [~ ..poke]
  ~>  %slog.[0 leaf+"arvo: metamorphosis"]
  (load u.hir)
::
++  wish                                                ::  +10
  |=  txt=*
  q:(slap ?~(zus pit $:u.zus) (ream ;;(@t txt)))
--
::
::  Arvo formal interface
::
::    this lifecycle wrapper makes the arvo door (multi-armed core)
::    look like a gate (function or single-armed core), to fit
::    urbit's formal lifecycle function (see aeon:eden:part).
::    a practical interpreter can and will ignore it.
::
|=  [now=@da ovo=ovum]
^-  *
.(+> +:(poke now ovo))
