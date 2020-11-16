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
::  $arch: fundamental node
::  $beak: global context
::  $beam: global name
::  $bone: opaque duct handle
::  $case: global version
::  $cage: marked vase
::  +cask: marked data builder
::  $desk: local workspace
::  $dock: message target
::  $mark: symbolic content type
::  $ship: network identity
::  $sink: subscription
::
+$  arch  [fil=(unit @uvI) dir=(map @ta ~)]
+$  beam  [beak s=path]
+$  beak  (trel ship desk case)
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
+$  mark  @tas
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
::  $scry-sample: vane +scry argument
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
+$  scry-sample
  [fur=(unit (set monk)) ren=@tas why=shop syd=desk lot=coin tyl=path]
+$  vane-sample
  [our=ship now=@da eny=@uvJ ski=slyd]
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
::  $vile: reflexive constants
::
::
+$  debt
  $:  ::  run: list of worklists
      ::  out: pending output
      ::
      run=(list plan)
      out=(list ovum)
  ==
+$  germ  [vane=term bars=(list duct)]
+$  heir
  $%  [%arvo-kelvin now=@da =debt =soul]
  ==
+$  plan  (pair germ (list move))
+$  soul
  $:  our=ship                                        ::  identity
      eny=@uvJ                                        ::  entropy
      lac=?                                           ::  laconic bit
      zus=vase                                        ::  %zuse
      van=(map term vane)                             ::  modules
  ==
+$  vane  [=vase =worm]
+$  vile
  $:  typ=type    ::  -:!>(*type)
      duc=type    ::  -:!>(*duct)
      wir=type    ::  -:!>(*wire)
      dud=type    ::  -:!>(*(unit goof))
  ==
--
=>
~%  %hex  ..ut  ~
|%
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bE, Arvo core                ::
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
  ::  we do not flop tyl because tyl wouldn't have been flopped by +en-beam
  ::
  =/  bed=beam
    [[fal dyc ved] tyl]
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
::  |part: arvo structures and engines
::
++  part
  =>  |%
      ::  $card: tagged, untyped event
      ::  $ovum: card with cause
      ::
      ::    XX replace top-level structures
      ::
      +$  card  (cask)
      +$  ovum  [=wire =card]
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
      |=  [now=@da sky=slyd]
      |%
      ::  +peek:plow:va: read from a local namespace
      ::
      ++  peek
        |=  [fur=(unit (set monk)) ren=@t bed=beam]
        ^-  (unit (unit (cask meta)))
        ::  namespace reads receive no entropy
        ::
        =/  sam=vane-sample  [our now *@uvJ sky]
        =^  rig  sac
          ~>  %mean.'peek: activation failed'
          (~(slym wa sac) vax sam)
        =^  gat  sac
          ~>  %mean.'peek: call failed'
          (~(slap wa sac) rig [%limb %scry])
        ::
        ;;  (unit (unit (cask meta)))
        %+  slum  q.gat
        ^-  scry-sample
        :*  fur
            ren
            [%& p.bed]
            q.bed
            `coin`[%$ r.bed]
            (flop s.bed)
        ==
      ::
      ::  |spin:plow:va: move statefully
      ::
      ++  spin
        |=  [hen=duct eny=@uvJ dud=(unit goof)]
        =*  duc  [duc.vil hen]
        =*  err  [dud.vil dud]
        =/  sam=vane-sample  [our now eny sky]
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
    =|  debt
    =*  nub  -
    =|  $:  ::  gem: worklist metadata
            ::  dud: propagate error
            ::  but: reboot gate
            ::
            gem=germ
            dud=(unit goof)
            but=(unit $-(heir (trap ^)))
        ==
    ::
    |_  [[pit=vase vil=vile] now=@da soul]
    +*  this  .
        sol   +<+>
    ::
    ::  +abet: finalize loop
    ::
    ++  abet
      ^-  (each (pair (list ovum) soul) (trap ^))
      ?~  but
        &/[(flop out) sol]
      |/(u.but [%arvo-kelvin now [run out] sol])
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
      this
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
          =/  met  (need (need (peek ** ~ (rsh 3 5 lal) bem)))
          lal^|+;;((list mass) q.q.met)
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
      ^-  slyd
      |=  [typ=* fur=(unit (set monk)) ron=term bed=beam]
      ^-  (unit (unit (cask meta)))
      ::
      ::  XX identity is defaulted to ship from beam
      ::
      =>  .(fur ?^(fur fur `[[%& p.bed] ~ ~]))
      ::  XX vane and care are concatenated
      ::
      =/  lal  (end 3 1 ron)
      =/  ren  ;;(@t (rsh 3 1 ron))
      ?.  (~(has by van) lal)
        ~
      (peek:(plow lal) fur ren bed)
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
            ::  %vega: notify vanes post upgrade
            ::  %whey: produce $mass                    :: XX remove, scry
            ::  %verb: toggle laconicity
            ::  %veer: upgrade module
            ::  %wack: iterate entropy                  :: XX move to $wasp
            ::  NB: %warn removed
            ::
            $%  [%lyra hun=(unit @t) van=@t]
                [%vega ~]
                [%whey ~]
                [%verb ~]
                [%veer lal=@tas pax=path txt=@t]
                [%wack p=@uvJ]
            ==
          ::
          +$  wasp
            ::  %crud: reroute $ovum with $goof         ::  NB: different
            ::  %trim: trim state, spam to all          ::  XX move to $waif
            ::
            $%  [%trim p=@ud]
                [%crud =goof =ovum]
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
      ++  call
        |=  =waif
        ^+  ..pith
        ?^  dud  ~>(%mean.'pith: goof' !!)
        ?-  -.waif
          %lyra  =/  pos=plan  [$/~ [*duct (gest [//arvo vega/~])] ~]
                 %_  ..pith
                   but  `(boot [hun van]:waif)
                   run  (weld run [pos ~])
                 ==
        ::
          %vega  (emit $/~ (spam /arvo !>(waif)))
          %verb  ..pith(lac !lac)
        ::
          %veer  ?:  ?=(%$ lal.waif)
                   ..pith(zus $:(smit:va "zuse" pit [pax txt]:waif))
                 ::
                 =/  nex  (create:va zus [lal pax txt]:waif)
                 =/  nav  (~(get by van) lal.waif)
                 =?  nex  ?=(^ nav)  (update:va vase.u.nav nex)
                 ..pith(van (~(put by van) lal.waif (settle:va nex)))
        ::
          %wack  ..pith(eny (shaz (cat 3 eny p.waif))) :: XX review
          %whey  ..pith(out [[//arvo mass/whey] out])
        ==
      ::
      ++  poke
        |=  =ovum
        ^+  ..pith
        ?~  wire.ovum
          ~>(%mean.'pith: bad wire' !!)
        ?~  buz=((soft wasp) card.ovum)
          (emit $/~ [*duct (gest ovum)] ~)
        ::
        ?-  -.u.buz
        ::
        ::  %trim: clear state
        ::
        ::    clears compiler caches if high-priority
        ::    XX add separate $wasp if this should happen last
        ::
          %trim  =?  van  =(0 p.u.buz)
                   (~(run by van) |=(=vane vane(worm *worm)))
                 (emit $/~ (spam t.wire.ovum !>(u.buz)))
        ::
        ::  %crud: forward error notification
        ::
          %crud  (emit $/~ [*duct hurl/[goof.u.buz (gest ovum.u.buz)]] ~)
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
        ?:  ?=(?(%lyra %veer %verb %wack %whey) -.card.ovum)
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
  |=  [hun=(unit @t) van=@t]
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
      ::  XX  %wyrd hint
      ::
      ~>(%mean.'wyrd: vega:' !!)
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
      %arvo-kelvin  soul.hir
    ==
  ::  clear compiler caches
  ::
  =.  van  (~(run by van) |=(=vane vane(worm *worm)))
  ::  restore working state and resume
  ::
  =/  zef=(each (pair (list ovum) soul) (trap ^))
    loop:(~(jump le:part [pit vil] now.hir sol) debt.hir)
  ?-  -.zef
    %&  [p.p.zef ..load(sol q.p.zef)]
    %|  $:p.zef
  ==
::
::  +peek: external inspect
::
++  peek                                                ::  +46
  |=  {now/@da hap/path}
  ^-  (unit)
  ?~  hap
    [~ ~ hoon-version]
  ::
  =/  el  ~(. le:part [pit vil] now sol)
  ?:  =(hap /whey)
    ``whey:el
  =/  rob
    ((sloy peek:el) [[151 %noun] hap])
  ?~  rob  ~
  ?~  u.rob  ~
  [~ u.u.rob]
::
::  +poke: external apply
::
++  poke                                                ::  +47
  |=  [now=@da ovo=ovum]
  ^-  ^
  =.  eny  (shaz (cat 3 eny now))  ::  XX review
  ::
  ~|  poke+-.q.ovo
  =/  zef=(each (pair (list ovum) soul) (trap ^))
    loop:(~(poke le:part [pit vil] now sol) ovo)
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
=|  $:  ::  who: identity once we know it
        ::  eny: entropy once we learn it
        ::  bod: %zuse once we receive it
        ::
        who=(unit ship)
        eny=(unit @)
        bod=(unit (trap vase))
        van=(map term (trap vase))
    ==
=>  |%
    ::  $grub: larval events
    ::
    +$  grub
      $%  $>(%veer waif:pith:le:part)
          $>(%wack waif:pith:le:part)   :: XX $wasp
          [%whom p=ship]
      ==
    ::
    ++  mint
      |=  [vax=vase lal=term pax=path txt=@t]
      ^-  (trap vase)
      =/  cap  ?:(?=(%$ lal) "zuse" "vane {<lal>}")
      (smit:va:part cap vax pax txt)
    ::
    ++  molt
      |=  $:  [our=ship now=@da eny=@uvJ]
              zus=vase
              van=(map term (trap vase))
          ==
      ^-  heir
      =/  nav  %-  ~(run by van)
               |=(a=(trap vase) (settle:va:part (slym $:a zus)))
      [%arvo-kelvin now *debt [our eny & zus nav]]
    --
::
|%
++  come  ^come                                         ::   +4
++  load  ^load                                         ::  +10
++  peek  _~                                            ::  +46
++  poke                                                ::  +47
  |=  [now=@da ovo=ovum]
  ^-  ^
  =/  gub
    ~>  %mean.'arvo: bad grub'
    ;;(grub q.ovo)
  ::
  =.  ..poke
    ?-    -.gub
        %veer  ?:  ?=(%$ lal.gub)
                 ..poke(bod `(mint pit [lal pax txt]:gub))
               =/  zus  =<($ (need bod))  ::  XX misparse
               ..poke(van (~(put by van) lal.gub (mint zus [lal pax txt]:gub)))
    ::
        %wack  ..poke(eny `p.gub)
        %whom  ..poke(who `p.gub)
    ==
  ::
  ::  upgrade once we've accumulated identity, entropy, and %zuse
  ::
  ?.  &(?=(^ who) ?=(^ eny) ?=(^ bod))
    [~ ..poke]
  ::
  ~>  %slog.[0 leaf+"arvo: metamorphosis"]
  (load (molt [u.who now u.eny] $:u.bod van))
::
++  wish                                                ::  +22
  |=  txt=*
  q:(slap ?~(bod pit $:u.bod) (ream ;;(@t txt)))
--
