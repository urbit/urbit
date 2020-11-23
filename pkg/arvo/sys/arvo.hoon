::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    Postface                              ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
~>  %slog.[0 leaf+"arvo: assembly"]
~<  %slog.[0 leaf+"arvo: assembled"]
=<  ::
    ::  Arvo formal interface
    ::
    ::    this lifecycle wrapper makes the arvo door (multi-armed core)
    ::    look like a gate (function or single-armed core), to fit
    ::    urbit's formal lifecycle function. a practical interpreter
    ::    can ignore it.
    ::
    |=  [now=@da ovo=ovum]
    ^-  *
    ~>  %slog.[0 leaf+"arvo: formal event"]
    .(+> +:(poke now ovo))
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    volume 3, Arvo models and skeleton    ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
=>
|%
+|  %global
::
++  arvo  %arvo-kelvin
::
::  $arch: node identity
::  $axal: fundamental node, recursive
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
::  +omen: namespace path and data
::  $ship: network identity
::  $sink: subscription
::
+$  arch  (axil @uvI)
++  axal
  |$  [item]
  [fil=(unit item) dir=(map @ta $)] ::
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
      ::
      [%da p=@da]
      [%tas p=@tas]
      [%ud p=@ud]
  ==
+$  cage  (cask vase)
++  cask  |$  [a]  (pair mark a)
+$  desk  @tas
+$  dock  (pair @p term)
+$  gang  (unit (set ship))
+$  mark  @tas
++  omen  |$  [a]  (pair path (cask a))
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
::  $curd: tagged, untyped event
::  $duct: causal history
::  +hobo: %soft task builder
::  $goof: crash label and trace XX fail/ruin/crud/flaw/lack/miss
::  $mass: memory usage
::  $monk: general identity
::  $move: cause and action
::  $ovum: card with cause
::  $roof: namespace
::  $rook: meta-namespace
::  +room: generic namespace
::  $vane-sample: vane wrapper-gate aargument
::  $sley: namespace function
::  $slyd: super advanced
::  $slyt: old namespace
::  +wind: kernel action builder
::  $wire: event pretext
::  +wite: kernel action/error builder
::
+$  ball  (wite [vane=term task=maze] maze)
+$  curd  (cask)
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
+$  monk  (each ship (pair @tas @ta))
+$  move  [=duct =ball]
+$  ovum  (pair wire curd)
::
+$  roof  (room vase)                                   ::  namespace
+$  rook  (room meta)                                   ::  meta-namespace
++  room                                                ::  either namespace
  |$  [a]
  $~  =>(~ |~(* ~))
  $-  $:  lyc=gang                                      ::  leakset
          cyr=term                                      ::  perspective
          bem=beam                                      ::  path
      ==                                                ::
  %-  unit                                              ::  ~: unknown
  %-  unit                                              ::  ~ ~: invalid
  (cask a)
::
+$  vane-sample
  [our=ship now=@da eny=@uvJ rof=rook]
::
+$  sley
  $-  [* (unit (set monk)) term beam]
  (unit (unit cage))
+$  slyd
  $~  =>(~ |~(* ~))
  $-  [* (unit (set monk)) term beam]
  (unit (unit (cask meta)))
+$  slyt  $-(^ (unit (unit)))
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
::  $germ: worklist source and bar stack
::  $heir: upgradeable state
::  $plan: worklist
::  $soul: persistent state
::  $vane: kernel module
::  $vere: runtime version
::  $vile: reflexive constants
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
+$  heir
  $%  [_arvo =debt =soul]
  ==
+$  plan  (pair germ (list move))
+$  soul
  $:  our=ship                                        ::  identity
      eny=@uvJ                                        ::  entropy
      now=@da                                         ::  time
      lac=?                                           ::  laconic bit
      ver=vere                                        ::  runtime
      lag=_|                                          ::  upgrade blocked
      fat=(axal (cask))                               ::  filesystem
      zus=vase                                        ::  %zuse
      van=(map term vane)                             ::  modules
  ==
+$  vane  [=vase =worm]
+$  vere
  $:  $:  non=@ta
          rev=(pair term (trel @ud @ud @ud)) :: XX path?
      ==
      kel=wynn
  ==
+$  vile
  $:  typ=type    ::  -:!>(*type)
      duc=type    ::  -:!>(*duct)
      wir=type    ::  -:!>(*wire)
      dud=type    ::  -:!>(*(unit goof))
  ==
+$  wynn  (list (pair term @ud))
--
=>
~%  %hex  ..ut  ~
|%
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bE, Arvo core                ::
::
++  en-beam
  |=(b=beam =*(s scot `path`[(s %p p.b) q.b (s r.b) s.b]))
::
++  de-beam
  |=  p=path
  ^-  (unit beam)
  ?.  ?=([@ @ @ *] p)  ~
  ?~  who=(slaw %p i.p)  ~
  ?~  des=?~(i.t.p (some %$) (slaw %tas i.t.p))  ~  :: XX +sym ;~(pose low (easy %$))
  ?~  ved=(slay i.t.t.p)  ~
  ?.  ?=([%$ case] u.ved)  ~
  `(unit beam)`[~ [`ship`u.who `desk`u.des `case`p.u.ved] t.t.t.p]
::
++  en-sley
  |=  =roof
  ^-  sley
  |=  [typ=* fur=(unit (set monk)) ron=term bed=beam]
  =/  lyc=gang
    ?~  fur  ~
    :-  ~
    %-  ~(gas in *(set ship))
    (murn ~(tap in u.fur) |=(m=monk ?-(-.m %| ~, %& `p.m)))
  (roof lyc ron bed)
::
++  sloy
  ::  +sloy: adapter from old style scrys to new style scrys
  ::
  ::    This does path parsing which shows up hot, but removing the last +slay
  ::    here requires deeper interface changes.
  ::
  !:
  ~/  %sloy
  |=  sod/slyd
  ^-  slyt
  |=  {ref/* raw/*}
  =+  pux=((soft path) raw)
  ?~  pux  ~
  ?.  ?=({@ @ @ @ *} u.pux)  ~
  =+  :*  hyr=(slaw %tas i.u.pux)
          fal=(slaw %p i.t.u.pux)
          dyc=?~(i.t.t.u.pux (some %$) (slaw %tas i.t.t.u.pux))
          ved=(slay i.t.t.t.u.pux)
          tyl=t.t.t.t.u.pux
      ==
  ?~  hyr  ~
  ?~  fal  ~
  ?~  dyc  ~
  ?.  ?=(^ ved)  ~
  =/  ron=@tas  u.hyr
  =/  bed=beam
    [[u.fal u.dyc (case p.u.ved)] (flop tyl)]
  =/  bop=(unit (unit (cask meta)))
    (sod ref ~ ron bed)
  ?~  bop  ~
  ?~  u.bop  [~ ~]
  ::  XX figure out wth to do about hoon-version
  ::
  ?.  ?&  ?=([?(%151 %141) *] ref)
          -:(~(nets wa *worm) +.ref -.q.u.u.bop)
      ==
    ~>(%slog.[0 leaf+"arvo: scry-lost"] ~)
  [~ ~ +.q.u.u.bop]
::  +sloy-light: minimal parsing version of sloy
::
::    There are several places inside vanes where we manually call the scry
::    function raw, instead of passing it into +mink. In those cases, we're
::    paying the price to render the arguments as text, and then are
::    immediately parsing the passed in data. We can avoid that.
::
::    TODO: The entire scrying system needs to be cleaned up in a more
::    permanent way. This hack fixes up some print/parse costs, but doesn't
::    recover the print/parse costs of the scry itself, which we could prevent
::    if we didn't send (list @ta), but instead sent (list dime).
::
++  sloy-light
  ~/  %sloy-light
  |=  sod/slyd
  |=  [ref=* ron=@tas fal=@p dyc=@tas ved=case tyl=path]
  =/  bed=beam  [[fal dyc ved] tyl]
  =/  bop=(unit (unit (cask meta)))
    (sod ref ~ ron bed)
  ?~  bop  ~
  ?~  u.bop  [~ ~]
  ::  XX figure out wth to do about hoon-version
  ::
  ?.  ?&  ?=([?(%151 %141) *] ref)
          -:(~(nets wa *worm) +.ref -.q.u.u.bop)
      ==
    ~>(%slog.[0 leaf+"arvo: scry-dark"] ~)
  [~ ~ +.q.u.u.bop]
::  +wyrd: kelvin negotiation
::
::    specified but unimplemented:
::    arvo should produce a [wend/wynn] effect
::    to signal downgrade
::
++  wyrd
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
            leaf/"%{(trip p)} %{(scow %ud q)} required;"
            ?~  q.u.wyr
              leaf/"runtime missing support"
            leaf/"runtime only supports %{(scow %ud u.q.u.wyr)}"
        ==
    ~>  %mean.'arvo: upgrade blocked'
    ~>  %mean.'wyrd'
    !!
  ::
  |-  ^-  (unit (pair (pair term @ud) (unit @ud)))
  ?~  hav  ~
  ::
  ::  fel: %&: runtime kelvin for [i.hav]
  ::       %|: no specified runtime support
  ::
  =/  fel
    |-  ^-  (each @ud (pair term @ud))
    ?~  run  |/i.hav
    ?:(=(p.i.hav p.i.run) &/q.i.run $(run t.run))
  ::
  ?-  -.fel
    %|  `[p.fel ~]
    %&  ?.((lte p.fel q.i.hav) `[i.hav `p.fel] $(hav t.hav))
  ==
::
::  |de: axal engine
::
++  de
  =|  fat=(axal)
  |@
  ::
  ++  get
    |=  pax=path
    ^+  fat
    ?~  pax  fat
    =/  kid  (~(get by dir.fat) i.pax)
    ?~  kid  [~ ~]
    $(fat u.kid, pax t.pax)
  ::
  ++  put
    |*  [pax=path dat=*]
    =>  .(dat `_?>(?=(^ fil.fat) u.fil.fat)`dat)
    ^+  fat
    ?~  pax  fat(fil `dat)
    =/  kid  (~(get by dir.fat) i.pax)
    =/  new  (fall kid fat(fil ~, dir ~))
    fat(dir (~(put by dir.fat) i.pax $(fat new, pax t.pax)))
  ::
  ++  gas
    |=  lit=(list (pair path _?>(?=(^ fil.fat) u.fil.fat)))
    ^+  fat
    ?~  lit  fat
    $(fat (put p.i.lit q.i.lit), lit t.lit)
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
  --
::
::  |part: arvo structures and engines
::
++  part
  =>  |%
      ::  $card: tagged, untyped event
      ::  $ovum: card with cause
      ::  $hoof: hoon source
      ::  $news: collated updates
      ::
      ::    XX replace top-level structures
      ::
      +$  card  (cask)
      +$  ovum  [=wire =card]
      +$  hoof  @t
      +$  news
          $:  ::  sys: installs + replacements
              ::  use: non-system files
              ::
              sys=(map path (cask))
              use=(map path (cask))
          ==
      +$  seed  [hun=(unit hoof) arv=hoof]
      +$  sprig
        $:  zus=(unit hoof)
            van=(list (cask hoof))
        ==
      --
  ::
  ~%  %part  ..part  ~
  |%
  ::
  +|  %utilities
  ::
  ::    XX move into |wa?
  ::
  ::  +slur: slam a vase with a maze
  ::
  ++  slur
    |=  [sac=worm gat=vase sam=maze]
    ^-  [vase worm]
    =^  cur  sac  (~(slot wa sac) 6 gat)
    =^  hig  sac
      ?-  -.sam
        %&  (~(nest wa sac) p.cur p.p.sam)
        %|  (~(nets wa sac) p.cur p.p.sam)
      ==
    ?>  hig
    (~(slym wa sac) gat q.p.sam)
  ::  +slid: cons a vase onto a maze
  ::
  ++  slid
    |=  [hed=vase tal=maze]
    ^-  maze
    ?-  -.tal
      %&  [%& (slop hed p.tal)]
      %|  [%| [%cell p.hed p.p.tal] [q.hed q.p.tal]]
    ==
  ::
  +|  %engines
  ::
  ::  |adapt
  ::
  ++  adapt
    =>  |%
        ::  deep file as source
        ::
        ++  sole  |=(a=(cask) `hoof`?>(?=([%hoon @t] a) q.a))
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
      =/  hav  (~(get de fat) pax)
      =?  del  |(?=(~ fil.hav) !=(u.fil.hav dat))
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
        ((bond |.((need fil:(~(get de fat) /sys/arvo)))) arv)
      ~!  rav
      :+  ~
        [`(sole u.hun) (sole rav)]
      [[/sys/arvo rav] [/sys/hoon u.hun] ~]
    ::  +adorn: augment capabilities
    ::
    ++  adorn
      |=  [del=news all=?]
      ^-  (pair sprig _fat)
      ::  zuse: shared library
      ::
      =^  zus  fat
        ?^  hav=(~(get by sys.del) /sys/zuse)
          :-  `(sole u.hav)
          (~(put de fat) /sys/zuse u.hav)
        :_  fat
        ~|  %adorn-no-zuse
        ?.(all ~ `(sole (need fil:(~(get de fat) /sys/zuse))))
      ::  kernel modules
      ::
      ::    %zuse is the subject of the vanes; force all if we have a new %zuse
      ::
      =|  nav=(map term hoof)
      =?  nav  |(all ?=(^ zus))
        %-  ~(gas by nav)
        %+  turn
          ~(tap by dir:(~(get de fat) /sys/vane))
        |=([name=@ta _fat] [`@tas`name (sole (need fil))])
      ::
      =^  new  fat
        %^    spin
            %+  skim  ~(tap by sys.del)
            |=([p=path *] ?=([%sys %vane @tas ~] p))
          fat
        |=  [[p=path q=(cask)] taf=_fat]
        ^-  (pair (cask hoof) _fat)
        ?>  ?=([%sys %vane @tas ~] p)
        =*  nam  i.t.t.p
        ?>  ((sane %tas) nam)
        [[`@tas`nam (sole q)] (~(put de taf) p q)]
      ::
      =;  van
        [[zus van] fat]
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
    =>  |%
        ::  XX move next to +slap
        ::
        ++  swat
          |=  [vax=vase gen=hoon]
          ^-  (trap vase)
          =/  gun  (~(mint ut p.vax) %noun gen)
          |.([p.gun .*(q.vax q.gun)])
        ::
        ++  smit
          |=  [cap=tape pit=vase pax=path txt=@t]
          ^-  (trap vase)
          ~>  %slog.[0 leaf+"{cap}: {(scow p+(mug txt))}"]
          %-  road  |.
          ~_  leaf/cap
          (swat pit (rain pax txt))
        ::
        ++  create
          |=  [zus=vase lal=term pax=path txt=@t]
          ^-  vase
          (slym $:(smit "vane {<lal>}" zus pax txt) zus)
        ::
        ++  settle
          |=  van=vase
          ^-  (pair vase worm)
          =/  [rig=vase wor=worm]  (~(slym wa *worm) van *vane-sample)
          [van +:(~(slap wa wor) rig [%limb %scry])]
        ::
        ::  XX pass identity to preserve behavior?
        ::
        ++  update
          |=  [las=vase nex=vase]
          ^-  vase
          =/  sam=vase  (slap (slym las *vane-sample) [%limb %stay])
          =/  gat=vase  (slap (slym nex *vane-sample) [%limb %load])
          (slam gat sam)
        --
    ::
    ~%  %va  ..va  ~
    |_  [our=ship vil=vile vax=vase sac=worm]
    ::
    ::  |plow:va: operate in time and space
    ::
    ++  plow
      |=  [now=@da rok=rook]
      |%
      ::  +peek:plow:va: read from a local namespace
      ::
      ++  peek
        ^-  rook
        |=  [lyc=gang cyr=term bem=beam]
        ^-  (unit (unit (cask meta)))
        ::  namespace reads receive no entropy
        ::
        =/  sam=vane-sample  [our now *@uvJ rok]
        =^  rig  sac
          ~>  %mean.'peek: activation failed'
          (~(slym wa sac) vax sam)
        =^  gat  sac
          ~>  %mean.'peek: call failed'
          (~(slap wa sac) rig [%limb %scry])
        ::
        =/  mas=[gang term beam]  [lyc cyr bem]
        ::
        =^  pro  sac  (~(slym wa sac) gat mas)
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
        =/  sam=vane-sample  [our now eny rok]
        =^  rig  sac
          ~>  %mean.'spin: activation failed'
          (~(slym wa sac) vax sam)
        ::
        |%
        ::  +slix: en-hypo
        ::
        ++  slix
          |=  hil=maze
          ^-  maze
          ?-  -.hil
            %&  [%& (slop [typ.vil p.p.hil] p.hil)]
            %|  [%| [%cell typ.vil p.p.hil] p.hil]
          ==
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
          ::  sample is [duct (unit goof) (hypo (hobo task))]
          ::
          =/  sam=maze
            (slid duc (slid err (slix task)))
          =^  pro  sac  (slur sac gat sam)
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
          ::  sample is [wire duct (unit goof) (hypo sign=[term gift])]
          ::
          =/  sam=maze
            =*  tea  [wir.vil wire]
            (slid tea (slid duc (slid err (slix (slid src gift)))))
          =^  pro  sac  (slur sac gat sam)
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
      ~?  !lac  ["" %unix p.card.ovum wire.ovum now]
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
      (~(lod what:pith fil.debt) kel.debt)
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
      ~?  &(!lac ?=(^ dud))  %goof
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
        ~?  &(!lac !=(%$ vane.gem))
          :-  (runt [(lent bars.gem) '|'] "")
          :^  %pass  [vane.gem vane]
            ?:  ?=(?(%deal %deal-gall) +>-.task)
              :-  :-  +>-.task
                  ;;([[ship ship] term term] [+>+< +>+>- +>+>+<]:task)
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
        ~?  !lac
          :-  (runt [(lent bars.gem) '|'] "")
          [%slip vane.gem (symp +>-.task) duct]
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
        ~?  &(!lac !=(%$ way) |(!=(%blit +>-.gift) !=(%d vane.gem)))
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
      =;  mod=(list mass)
        :+  %arvo  %|
        :~  hoon+&+pit
            zuse+&+zus
            vane+|+mod
        ==
      ::
      =/  von
        %+  turn  
          (sort ~(tap by van) |=([[a=@tas *] [b=@tas *]] (aor a b)))
        |=([lal=@tas =vane] (cat 3 %vane- lal)^vane)
      ::
      :~  :+  %reports  %|
          %+  turn  von
          =/  bem=beam  [[our %home da+now] /whey]
          |=  [lal=@tas =vane]
          =/  met  (peek ~ (rsh 3 5 lal) bem)
          ?>  &(?=(^ met) ?=(^ u.met))  :: XX make optional
          lal^|+;;((list mass) q.q.u.u.met)
      ::
          :+  %caches  %|
          %+  turn  von
          |=([lal=@tas =vane] lal^&+worm.vane)
      ::
          :+  %dregs  %|
          %+  turn  von
          |=([lal=@tas =vane] lal^&+vase.vane)
      ==
    ::  +peek: read from the entire namespace
    ::
    ++  peek
      ^-  rook
      |=  [lyc=gang cyr=term bem=beam]
      ^-  (unit (unit (cask meta)))
      ::
      ?:  ?=(%$ cyr)
        (peek:pith lyc %$ bem)
      ::
      ::  XX vane and care are concatenated
      ::
      =/  lal  (end 3 1 cyr)
      =/  ren  ;;(@t (rsh 3 1 cyr))
      ?.  (~(has by van) lal)
        ~
      (peek:(plow lal) lyc ren bem)
    ::  +call: advance to target
    ::
    ++  call
      |=  [=duct way=term task=maze]
      ^+  this
      ?:  ?=(%$ way)
        ~>  %mean.'call: bad waif'
        (call:pith ;;(waif:pith q.p.task))
      ::
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
      ::
      ::  the caller was a vane
      ::
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
      =.  van  (~(put by van) vane.gum [vax sac])
      (emit `plan`[`germ`gum `(list move)`moz])
    ::  +plow: operate on a vane, in time and space
    ::
    ++  plow
      |=  way=term
      ~|  [%plow-failed way]
      =/  =vane
        ~|  [%missing-vane way]
        (~(got by van) way)
      (~(plow va [our vil vane]) now peek)
    ::
    ::  |pith: operate on arvo internals
    ::
    ++  pith
      =>  |%
          ::  $waif: arvo effect, from anywhere
          ::  $wasp: arvo effect, from Outside
          ::
          +$  waif
            ::  %lyra: upgrade kernel
            ::  %trim: trim state, spam to all
            ::  %vega: notify vanes post upgrade
            ::  %what: update from files
            ::  %whey: produce $mass                    :: XX remove, scry
            ::  %verb: toggle laconicity
            ::  %veer: upgrade module
            ::  NB: %warn removed
            ::
            $%  [%lyra hun=(unit @t) van=@t]
                [%trim p=@ud]
                [%vega ~]
                [%what p=(list (pair path (cask)))]
                [%whey ~]
                [%verb p=(unit ?)]
                [%veer lal=@tas pax=path txt=@t]
            ==
          ::
          +$  wasp
            ::  %crud: reroute $ovum with $goof         ::  NB: different
            ::  %wack: iterate entropy
            ::  %wyrd: check/record runtime kelvin stack
            ::
            $%  [%crud =goof =ovum]
                [%wack p=@uvJ]
                [%wyrd p=vere]
            ==
          --
      ::
      |%
      ::
      ++  gest
        |=  =ovum
        ^-  $>(%pass ball)
        =^  way=term  wire.ovum  wire.ovum
        ::
        ::  XX uncomment to restore previous routing
        ::
        :: =?  way  ?=(%$ way)  (dint wire.ovum)
        ::
        ::  %$: default, routed to arvo-proper as trivial vase
        ::  @:  route to vane XX remove %soft, clam via %zuse
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
          =/  del  (~(group adapt fat) fil)
          =/  tub  (~(usurp adapt fat) del)
          ?~  tub
            (mod del |)
          =/  pos=plan
            [$/~ [*duct (gest [//arvo vega/~])] ~]
          =/  gat  (boot kel.ver [hun arv]:p.u.tub)
          %_  ..pith
            but  `[gat q.u.tub fil]
            run  (weld run [pos ~])
          ==
        ::
        ++  lod
          |=  kel=(list (pair path (cask)))
          ^+  ..pith
          =/  fat  (~(gas de fat) kel)
          %+  mod
            (~(group adapt fat) fil)
          %+  lien  kel
          |=  [p=path *]
          ?=([%sys ?(%arvo %hoon) *] p)
        ::
        ++  mod
          |=  [del=news all=?]
          ^+  ..pith
          =^  job=sprig  fat  (~(adorn adapt fat) del all)
          =?  zus  ?=(^ zus.job)
            $:(smit:va "zuse" pit /sys/zuse/hoon u.zus.job)
          %-  (wyrd kel.ver [zuse/;;(@ud q:(slap zus limb/%zuse)) ~])
          %=    ..pith
              van
            %+  roll  van.job
            |=  [[(cask hoof)] =_van]
            ^+  van
            =/  way  (wilt p)
            =/  nex  (create:va zus way /sys/vane/[p]/hoon q)
            =/  nav  (~(get by van) way)
            =?  nex  ?=(^ nav)  (update:va vase.u.nav nex)
            (~(put by van) way (settle:va nex))
          ==
        --
      ::
      ++  call
        |=  =waif
        ^+  ..pith
        ?^  dud  ~>(%mean.'pith: goof' !!)
        ?-  -.waif
          %lyra  =;  wat  $(waif wat)
                 :+  %what  [/sys/arvo hoon/van.waif]
                 ?~  hun.waif  ~
                 [[/sys/hoon hoon/u.hun.waif] ~]
        ::
        ::  %trim: clear state
        ::
        ::    clears compiler caches if high-priority
        ::    XX add separate $wasp if this should happen last
        ::
          %trim  =?  van  =(0 p.waif)
                   (~(run by van) |=(=vane vane(worm *worm)))
                 (emit $/~ (spam /arvo !>(waif)))
        ::
          %vega  (emit $/~ (spam /arvo !>(waif)))  :: XX also out
          %verb  ..pith(lac ?~(p.waif !lac u.p.waif))
        ::
          %veer  =/  pax
                   sys/?:(?=(%$ lal.waif) /zuse /vane/[(grow lal.waif)])
                 $(waif what/[[pax hoon/txt.waif] ~])
        ::
          %what  ~(kel what p.waif)
          %whey  ..pith(out [[//arvo mass/whey] out])
        ==
      ::
      ++  peek
        ^-  roof
        |=  [lyc=gang car=term bem=beam]
        ^-  (unit (unit cage))
        ?.  ?|  =(our p.bem)
                ?=(%$ q.bem)
                =([%da now] p.r.bem)
            ==
          ~
        ?+  s.bem  ~
          [%whey ~]  ``mass/!>(whey)
          [%lag ~]   ``noun/!>(lag)
        ==
      ::
      ++  poke
        |=  =ovum
        ^+  ..pith
        ?~  wire.ovum
          ~>(%mean.'pith: bad wire' !!)
        ::
        ?.  ?=(?(%crud %wack %wyrd) -.card.ovum)
          (emit $/~ [*duct (gest ovum)] ~)
        ::
        =/  buz  ;;(wasp card.ovum)
        ?-  -.buz
        ::
        ::  %crud: forward error notification
        ::
          %crud  =?  lag  ?&  ?=(%exit mote.goof.buz)
                              ?=(^ tang.goof.buz)
                              ?=(%leaf -.i.tang.goof.buz)  :: XX ?@
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
          %wyrd  %-  %+  wyrd  kel.p.buz
                     ^-  (list (pair term @))
                     :~  hoon/hoon-version
                         arvo/arvo
                         zuse/;;(@ q:(slap zus limb/%zuse))
                     ==
                 =?  lag  !=(rev.ver rev.p.buz)  ~&(%unlagging |)
                 ..pith(ver p.buz)
        ==
      ::
      ++  spam
        |=  [=wire =vase]
        ^-  (list move)
        %+  turn
          %+  sort  ~(tap by van)
          |=([[a=@tas *] [b=@tas *]] (aor a b))
        |=([way=term *] `move`[*duct %pass wire way `maze`&/vase])
      ::
      ++  xeno
        |=  =ovum
        ^+  this
        ::  XX update clients to %pass to arvo, remove
        ::
        ?:  ?=(?(%lyra %veer %verb %whey) -.card.ovum)
          ~>  %mean.'xeno: bad waif'
          (call ;;(waif:pith card.ovum))
        ::
        ::  XX uncomment to restore previous routing
        ::
        :: =.  wire.ovum  $/wire.ovum
        this(out [ovum out])
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
  ~>  %mean.'vega: ruin'
  ?~  hun
    =/  gat
      ~>  %slog.[0 leaf/"vega: compiling arvo"]
      %-  road  |.
      (slap !>(..ride) (rain /sys/arvo/hoon van))
    =/  lod
      (slap (slot 7 gat) [%limb %load])
    |=  =heir
    |.  ;;(^ q:(slam lod !>(heir)))
  ::
  ::  hyp: hoon core type
  ::  hoc: hoon core
  ::  cop: compiler gate
  ::
  =/  [hyp=* hoc=* cop=*]
    ::  compile new hoon.hoon source with the current compiler
    ::
    =/  raw
      ~>  %slog.[0 leaf/"vega: compiling hoon"]
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
      ::  XX revise hint
      ::
      ~>(%mean.'wyrd: vega:' !!)
    ::  require runtime compatibility
    ::
    %-  (wyrd kel [hoon/nex ~])
    ::
    ::  if we're upgrading language versions, recompile the compiler
    ::
    =^  hot=*  cop
      ?:  =(nex hoon-version)
        [raw cop]
      =/  hot
        ~>  %slog.[0 leaf+"vega: recompiling hoon %{<`@`nex>}"]
        (road |.((slum cop [%noun hun])))
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
    ~>  %slog.[0 leaf/"vega: compiling arvo"]
    (road |.((slum cop [hyp van])))
  ::  activate arvo and extract the arvo core from the outer gate
  ::
  =/  voc  .*(hoc [%7 +.rav %0 7])
  ::
  ::  extract the upgrade gate +load
  ::
  ::    XX +come is now ignored, remove?
  ::    XX could be a constant axis now (currently +10)
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
  |.  ;;(^ (slum lod heir))
::
++  viol                                                ::  vane tools
  |=  but/type
  ^-  vile
  =+  pal=|=(a/@t ^-(type (~(play ut but) (vice a))))
  :*  typ=(pal '$:type')
      duc=(pal '$:duct')
      wir=(pal '$:wire')
      dud=(pal '=<($ (unit goof))')  ::  XX misparse
  ==
::
++  dint                                              ::  input routing
  |=  hap=path  ^-  @tas
  ?+  hap  %$
    [%ames *]  %a
    [%boat *]  %c
    [%newt *]  %a
    [%sync *]  %c
    [%term *]  %d
    [%http-client *]  %i
    [%http-server *]  %e
    [%behn *]  %b
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
    %f  %ford
    %g  %gall
    %i  %iris
    %j  %jael
  ==
::
++  wilt
  |=  van=term
  ?+  van  van
    %ames  %a
    %behn  %b
    %clay  %c
    %dill  %d
    %eyre  %e
    %ford  %f
    %gall  %g
    %iris  %i
    %jael  %j
  ==
::
++  is  &
--
=>
::
::  persistent arvo state
::
=/  pit=vase  !>(..is)                                  ::
=/  vil=vile  (viol p.pit)                              ::  cached reflexives
=|  soul                                                ::
=*  sol  -
::  arvo: structural interface core
::
|%
::
::  +come: load incompatible
::
++  come  |=(* !!)                                      ::   +4
::
::  +load: load compatible, notifying vanes
::
++  load                                                ::  +10
  |=  hir=heir
  ^-  ^
  ~|  %load
  ::  store persistent state
  ::
  =.  sol
    ?-  -.hir
      _arvo  soul.hir
    ==
  ::  clear compiler caches
  ::
  =.  van  (~(run by van) |=(=vane vane(worm *worm)))
  ::
  %-  %+  wyrd  kel.ver
      ^-  (list (pair term @))
      :~  hoon/hoon-version
          arvo/arvo
          zuse/;;(@ q:(slap zus limb/%zuse))
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
++  peek                                                ::  +46
  |=  $:  lyc=gang
          $=  nom
          %+  each  path
          $%  [%once cyr=term syd=desk tyl=spur]
              [%beam cyr=term bem=beam]
          ==
      ==
  ^-  (unit (cask))
  =/  hap=(unit [pat=? cyr=term bem=beam])
    ?-  nom
      [%& *]        ?~  p.nom  ~
                    ?~  bem=(de-beam t.p.nom)  ~
                    `[| i.p.nom u.bem]
    ::
      [%| %beam *]  `[| cyr bem]:p.nom
    ::
      [%| %once *]  `[& cyr.p.nom [our syd.p.nom da/now] tyl.p.nom]
    ==
  ::
  ?~  hap  ~
  =/  pro  (~(peek le:part [pit vil] sol) lyc [cyr bem]:u.hap)
  ?:  |(?=(~ pro) ?=(~ u.pro))  ~
  =/  dat=(cask)  [p.u.u.pro q.q.u.u.pro]
  ?.  pat.u.hap  `dat
  `[%omen [cyr.u.hap (en-beam bem.u.hap)] dat]

::
::  +poke: external apply
::
++  poke                                                ::  +47
  |=  [now=@da ovo=ovum]
  ^-  ^
  =:  eny  (shaz (cat 3 eny now))  ::  XX review
      now  now
    ==
  ::
  ~|  poke+-.q.ovo
  =/  zef=(each (pair (list ovum) soul) (trap ^))
    loop:(~(poke le:part [pit vil] sol) ovo)
  ?-  -.zef
    %&  [p.p.zef ..poke(sol q.p.zef)]
    %|  $:p.zef
  ==
::
::  +wish: external compute
::
++  wish                                                ::  +22
  |=  txt/@
  q:(slap zus (ream txt))
--
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
    ::  $foal: larval state
    ::  $grub: larval events
    ::
    +$  foal
      $:  ::  who: identity once we know it
          ::  eny: entropy once we learn it
          ::  bod: %zuse once we receive it
          ::
          who=(unit ship)
          eny=(unit @)
          lac=?
          ver=(unit vere)
          fat=(unit (axal (cask)))
          bod=(unit (trap vase))
          van=(map term (trap vase))
      ==
    +$  grub
      $~  verb/~
      $%  $>(%verb waif:pith:le:part)
          $>(%veer waif:pith:le:part)
          $>(%wack wasp:pith:le:part)
          $>(%what waif:pith:le:part)
          [%whom p=ship]
          $>(%wyrd wasp:pith:le:part)
      ==
    ::
    ++  mint
      |=  [vax=vase lal=term pax=path txt=@t]
      ^-  (trap vase)
      =/  cap  ?:(?=(%$ lal) "zuse" "vane {<lal>}")
      (smit:va:part cap vax pax txt)
    ::
    ++  molt
      |=  [now=@da foal]
      ^-  (unit heir)
      ?.  &(?=(^ who) ?=(^ eny) ?=(^ ver) ?=(^ fat) ?=(^ bod))
        ~
      =/  zus  $:u.bod
      %-  %+  wyrd  kel.u.ver
          ^-  (list (pair term @))
          :~  hoon/hoon-version
              arvo/arvo
              zuse/;;(@ud q:(slap zus limb/%zuse))
          ==
      =/  nav  %-  ~(run by van)
               |=(a=(trap vase) (settle:va:part (slym $:a zus)))
      :^  ~  arvo  *debt
      [u.who u.eny now lac u.ver | u.fat zus nav]
    --
::
=|  foal
=*  fol  -
|%
++  come  ^come                                         ::   +4
++  load  ^load                                         ::  +10
++  peek  _~                                            ::  +46
++  poke                                                ::  +47
  |=  [now=@da ovo=ovum]
  ^-  ^
  =/  gub
    ~|  [p.ovo p.q.ovo]
    ~>  %mean.'arvo: bad grub'
    ;;(grub q.ovo)
  ::
  =.  ..poke
    |-  ^+  ..poke
    ?-    -.gub
        %verb  ..poke(lac ?~(p.gub !lac u.p.gub))
    ::
        %veer  =/  pax
                 sys/?:(?=(%$ lal.gub) /zuse /vane/[(grow lal.gub)])
               $(q.ovo what/[[pax hoon/txt.gub] ~])
    ::
        %wack  ..poke(eny `p.gub)
    ::
        %what  =/  taf  (fall fat *(axal (cask)))
               =/  del  (~(group adapt:part taf) p.gub)
               =/  tub  (~(usurp adapt:part taf) del)
               ?:  &(?=(^ dir.taf) ?=(^ tub))
                 ~|(%larval-reboot !!)   :: XX support
               ::
               ::  require, and unconditionally adopt, initial kernel source
               ::
               =?  taf  =(~ dir.taf)     ::  XX TMI
                 ~|  %larval-need-kernel
                 ?>  ?=(^ tub)
                 (~(gas de taf) q.u.tub)
               ::
               =^  job  taf  [p q]:(~(adorn adapt:part taf) del |)
               =?  bod  ?=(^ zus.job)
                 `(mint pit %$ /sys/zuse/hoon u.zus.job)
               %=    ..poke
                   fat  `taf
                   van
                 %+  roll  van.job
                 |=  [[(cask hoof:part)] =_van]
                 ^+  van
                 ?>  ?=(^ bod)
                 =/  way  (wilt p)
                 (~(put by van) way (mint $:u.bod way /sys/vane/[p]/hoon q))
               ==
    ::
        %whom  ..poke(who `p.gub)
        %wyrd  %-  %+  wyrd  kel.p.gub
                   ^-  (list (pair term @))
                   :*  hoon/hoon-version
                       arvo/arvo
                       ?~  bod  ~
                       [zuse/;;(@ud q:(slap $:u.bod limb/%zuse)) ~]
                   ==
               ..poke(ver `p.gub)
    ==
  ::
  ?~  hir=(molt now fol)
    [~ ..poke]
  ::
  ::  upgrade once we've accumulated necessary state
  ::
  ~>  %slog.[0 leaf+"arvo: metamorphosis"]
  (load u.hir)
::
++  wish                                                ::  +22
  |=  txt=*
  q:(slap ?~(bod pit $:u.bod) (ream ;;(@t txt)))
--
