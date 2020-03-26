::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    Postface                              ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
~>  %slog.[0 leaf+"%arvo-assembly"]
=-  ~>  %slog.[0 leaf+"%arvo-assembled"]
    -
=<  ::
    ::  Arvo formal interface
    ::
    ::    this lifecycle wrapper makes the arvo door (multi-armed core)
    ::    look like a gate (function or single-armed core), to fit
    ::    urbit's formal lifecycle function. a practical interpreter
    ::    can ignore it.
    ::
    |=  [now=@da ovo=*]
    ^-  *
    ~>  %slog.[0 leaf+"arvo-event"]
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
::  $pane: kernel modules
::  $pone: kernel modules old
::  $vane: kernel module
::  $vile: reflexive constants
::
+$  pane  (list (pair @tas vase))
+$  pone  (list (pair @tas vise))
+$  vane  [=vase =worm]
+$  vile
  $:  typ=type    ::  -:!>(*type)
      duc=type    ::  -:!>(*duct)
      wir=type    ::  -:!>(*wire)
      dud=type    ::  -:!>(*(unit goof))
  ==
--
=>
~%  %hex  +>  ~
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
          dyc=(slaw %tas i.t.t.u.pux)
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
  ~%  %part  +>  ~
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
    ~/  %va
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
    =>  |%
        ::  $germ: worklist source and bar stack
        ::  $plan: worklist
        ::
        +$  germ  [vane=term bars=(list duct)]
        +$  plan  (pair germ (list move))
        --
    ::
    ~%  %le  +>+>  ~
    =|  $:  ::  run: list of worklists
            ::  out: pending output
            ::  gem: worklist metadata
            ::  dud: propagate error
            ::
            run=(list plan)
            out=(list ovum)
            gem=germ
            dud=(unit goof)
        ==
    ::
    |_  $:  our=ship
            now=@da
            eny=@uvJ
            lac=?
            vil=vile
            van=(map term vane)
        ==
    +*  this  .
    ::  +abet: finalize loop
    ::
    ++  abet
      ^-  (pair (list ovum) (list (pair term vane)))
      :-  (flop out)
      %+  sort
        ~(tap by van)
      |=([[a=@tas *] [b=@tas *]] (aor a b))
    ::  +emit: enqueue a worklist with source
    ::
    ++  emit
      |=  pan=plan
      this(run [pan run])
    ::  +poke: prepare a worklist-of-one from outside
    ::
    ++  poke
      |=  [vane=term =ovum]
      ^+  this
      ~>  %mean.'arvo: poke crashed'
      ~?  !lac  ["" %unix p.card.ovum wire.ovum now]
      =/  =maze
        =/  =type  [%cell [%atom %tas `%soft] %noun]
        [%& type [%soft card.ovum]]
      =/  =move
        ~|  [%bad-wire wire.ovum]
        ?>  ?=([%$ *] wire.ovum)
        [duct=~ %pass t.wire.ovum vane maze]
      (emit [%$ ~] move ~)
    ::  +crud: prepare a worklist-of-one with error report from outside
    ::
    ++  crud
      |=  [vane=term =goof =ovum]
      ^+  this
      ~>  %mean.'arvo: crud crashed'
      ~?  !lac  ["" %unix %crud p.card.ovum wire.ovum now]
      =/  =maze
        =/  =type  [%cell [%atom %tas `%soft] %noun]
        [%& type [%soft card.ovum]]
      =/  =move
        ~|  [%bad-wire wire.ovum]
        ?>  ?=([%$ *] wire.ovum)
        [duct=~ %hurl goof %pass t.wire.ovum vane maze]
      (emit [%$ ~] move ~)
    ::  +spam: prepare a worklist for all targets
    ::
    ++  spam
      |=  =ovum
      ^+  this
      ~>  %mean.'arvo: spam crashed'
      ::  fix up wire on %vega from previous kernel
      ::
      =?  wire.ovum  =(ovum [/ %vega ~])  //arvo
      =/  ord=(list (pair term *))
        %+  sort
          ~(tap by van)
        |=([[a=@ *] [b=@ *]] (aor b a))
      |-  ^+  this
      ?~  ord
        this
      =.  this  (poke p.i.ord ovum)
      $(ord t.ord)
    ::  +loop: until done
    ::
    ++  loop
      ^+  this
      ?~  run
        this
      ?:  =(~ q.i.run)    :: XX TMI
        loop(run t.run)
      =.  dud  ~
      =.  gem  p.i.run
      =^  mov  q.i.run  q.i.run
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
        ?~  duct
          ::
          ::  the caller was Outside
          ::
          ~|  [%xeno wire (symp -.q.p.gift)]
          ?>  ?=([%$ *] wire)
          (xeno wire gift)
        ::
        ::  the caller was a vane
        ::
        =^  vane=term  wire
          ~|  [%give duct.move (symp -.q.p.gift)]
          ?>(?=(^ wire) wire)
        ::
        ~?  &(!lac |(!=(%blit +>-.gift) !=(%d vane.gem)))
          :-  (runt [(lent bars.gem) '|'] "")
          :^  %give  vane.gem
            ?:  ?=(%unto +>-.gift)
              [+>-.gift (symp +>+<.gift)]
            (symp +>-.gift)
          duct.move
        ::
        (take duct wire vane gift)
      ::
      ::  %hurl: action with error
      ::
          %hurl
        %=  $
          dud       `goof.ball.move
          ball.move  wite.ball.move
        ==
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
      ::
      ::  XX vane and care are concatenated
      ::
      =/  lal  (end 3 1 ron)
      =/  ren  ;;(@t (rsh 3 1 ron))
      ?.  (~(has by van) lal)
        ~
      (peek:(plow lal) fur ren bed)
    ::  +xeno: stash pending output
    ::
    ++  xeno
      |=  [=wire gift=maze]
      ^+  this
      this(out [[wire ;;(card q.p.gift)] out])
    ::  +call: advance to target
    ::
    ++  call
      |=  [=duct way=term task=maze]
      ^+  this
      %+  push  [way duct bars.gem]
      ~|  bar-stack=`(list ^duct)`[duct bars.gem]
      %.  task
      call:(spin:(plow way) duct eny dud)
    ::  +take: retreat along call-stack
    ::
    ++  take
      |=  [=duct =wire way=term gift=maze]
      ^+  this
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
    --
  --
::
++  symp                                                ::  symbol or empty
  |=  a=*  ^-  @tas
  ?.(&(?=(@ a) ((sane %tas) a)) %$ a)
::
++  vent                                                ::  vane core
  |=  [who=ship lal=@tas vil=vile bud=vase =vane]
  ~%  %vent  +>+  ~
  |%
  ++  ruck                                              ::  update vase
    |=  {pax/path txt/@ta}
    ^+  +>
    =-  ?:(?=(%| -.res) ((slog p.res) +>.$) p.res)
    ^=  res  %-  mule  |.
    ::  XX should use real entropy and the real date
    ::
    =/  arg=vane-sample
      [who ~2000.1.1 *@uvJ =>(~ |~(* ~))]
    =+  rig=(slym vase.vane arg)
    =+  gen=(rain pax txt)
    =+  rev=(slym (slap bud gen) bud)
    =+  syg=(slym rev arg)
    ::  update the vane itself
    ::
    ::    We don't cache the n+slap/+slam types because they're only used once
    ::    right here; they'll never be used again.
    ::
    =.  vase.vane
      ~|  %load-lost
      (slam (slap syg [%limb %load]) (slap rig [%limb %stay]))
    ::  prime the new compiler cache
    ::
    prime
  ::  reset and prime the worm cache for scrys
  ::
  ::    If the +slap/+slym in scry isn't cached, we spend the majority of
  ::    the time in a scry in the compiler. The +scry gate cannot have side
  ::    effects so we can't modify the cache at access time. So we seed the
  ::    cache with all the things +scry will need when we install the vane
  ::
  ++  prime
    ^+  ..prime
    ::
    %_    ..prime
        worm.vane
      ::  reset cache and add in vane activation entry
      ::
      =^  rig  worm.vane
        (~(slym wa *worm) vase.vane *vane-sample)
      ::  cache the access of the %scry arm
      ::
      +:(~(slap wa worm.vane) rig [%limb %scry])
    ==
  --
::
++  vint                                                ::  create vane
  |=  $:  who=ship
          lal=@tas
          vil=vile
          bud=vase
          pax=path
          txt=@ta
      ==
  =-  ?:(?=(%| -.res) ((slog p.res) ~) (some p.res))
  ^=  res  %-  mule  |.
  ~|  [%failed-vint lal]
  =+  gen=(rain pax txt)
  ~&  [%vane-parsed `@p`(mug gen)]
  =+  pro=(vent who lal vil bud [(slym (slap bud gen) bud) *worm])
  ~&  [%vane-compiled `@p`(mug pro)]
  prime:pro
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
  |=  hap/path  ^-  @tas
  ?+  hap  ~|([%bad-dint hap] !!)
    {@ $ames *}  %a
    {@ $boat *}  %c
    {@ $newt *}  %a
    {@ $sync *}  %c
    {@ $term *}  %d
    {@ $http-client *}  %i
    {@ $http-server *}  %e
    {@ $behn *}  %b
  ==
::
++  is  &
--
=<  ::  Arvo larval stage
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
    =/  pit=vase  !>(..is)
    =|  $:  ::  who: our identity once we know it
            ::  eny: entropy once we learn it
            ::  bod: %zuse once we receive it
            ::
            who=(unit ship)
            eny=(unit @)
            bod=(unit vase)
        ==
    ::  larval Arvo structural interface
    ::
    |%
    ++  come  ^come                                     ::   4
    ++  load  ^load                                     ::  10
    ++  peek  |=(* ~)                                   ::  46
    ::
    ++  poke  |=  *                                     ::  47
              ^-  [(list ovum) *]
              =>  .(+< ;;([now=@da ovo=ovum] +<))
              ^-  [(list ovum) *]
              =.  +>.$
                ?+  -.q.ovo
                  ::  ignore unrecognized
                  ::
                  ~&  [%larval-ignore p.ovo -.q.ovo]
                  +>.$
                ::  install %zuse or vane
                ::
                    %veer
                  ^+  +>.$
                  ::  use the maximum comet if we don't know who we are yet
                  ::
                  =/  our
                    ?^  who
                      u.who
                    =/  fip=ship  (dec (bex 128))
                    ~>(%slog.[0 leaf+"arvo: larval identity {(scow %p fip)}"] fip)
                  =.  ..veer  (veer our now q.ovo)
                  +>.$(bod ?^(bod bod `bud.^poke))
                ::  add entropy
                ::
                    %wack
                  ^+  +>.$
                  ?>  ?=(@ q.q.ovo)
                  +>.$(eny `q.q.ovo)
                ::  become who you were born to be
                ::
                    %whom
                  ^+  +>.$
                  ?>  ?=(@ q.q.ovo)
                  +>.$(who `q.q.ovo)
                ==
              ::  upgrade once we've accumulated identity, entropy, and %zuse
              ::
              ?.  &(?=(^ who) ?=(^ eny) ?=(^ bod))
                [~ +>.$]
              ~>  %slog.[0 leaf+"arvo: metamorphosis"]
              =/  nyf
                (turn vanes.^poke |=([label=@tas =vane] [label vase.vane]))
              (load u.who now u.eny ova=~ u.bod nyf)
    ::
    ++  wish  |=  txt=*                                 ::  22
              ?>  ?=(@ txt)
              q:(slap ?~(bod pit u.bod) (ream txt))
    --
::
::  persistent arvo state
::
=/  pit=vase  !>(..is)                                  ::
=/  vil=vile  (viol p.pit)                              ::  cached reflexives
=|  $:  lac=_&                                          ::  laconic bit
        eny=@                                           ::  entropy
        our=ship                                        ::  identity
        bud=vase                                        ::  %zuse
        vanes=(list [label=@tas =vane])                 ::  modules
    ==                                                  ::
=<  ::  Arvo structural interface
    ::
    |%
    ++  come  |=  [@ @ @ (list ovum) vise pone]         ::   4
              ^-  [(list ovum) _+>]
              ~&  %hoon-come
              =^  rey  +>+  (^come +<)
              [rey +>.$]
    ::
    ++  load  |=  [@ @ @ (list ovum) vase pane]         ::  10
              ^-  [(list ovum) _+>]
              ~&  %hoon-load
              =^  rey  +>+  (^load +<)
              [rey +>.$]
    ::
    ++  peek  |=  *                                     ::  46
              =/  rob  (^peek ;;([@da path] +<))
              ?~  rob  ~
              ?~  u.rob  ~
              [~ u.u.rob]
    ::
    ++  poke  |=  *                                     ::  47
              =>  .(+< ;;([now=@da ovo=ovum] +<))
              =^  ova  +>+.$  (^poke now ovo)
              =|  out=(list ovum)
              |-  ^-  [(list ovum) *]
              ?~  ova
                [(flop out) +>.^$]
              ::  upgrade the kernel
              ::
              ?:  ?=(%lyra -.q.i.ova)
                %+  fall
                  (vega now t.ova ;;([@ @] +.q.i.ova))
                [~ +>.^$]
              ::  iterate over effects, handling those on arvo proper
              ::  and passing the rest through as output
              ::
              =^  vov  +>+.^$  (feck now i.ova)
              =?  out  ?=(^ vov)  [+.vov out]
              $(ova t.ova)
    ::
    ++  wish  |=(* (^wish ;;(@ta +<)))             ::  22
    --
::  Arvo implementation core
::
|%
++  come                                                ::  load incompatible
  |=  [who=ship now=@da yen=@ ova=(list ovum) dub=vise nyf=pone]
  ^+  [ova +>]
  =/  fyn  (turn nyf |=([a=@tas b=vise] [a (slim b)]))
  (load who now yen ova (slim dub) fyn)
::
++  load                                                ::  load compatible
  |=  [who=ship now=@da yen=@ ova=(list ovum) dub=vase nyf=pane]
  ^+  [ova +>]
  =:  our  who
      eny  yen
      bud  dub
      vanes  (turn nyf |=({a/@tas b/vise} [a [b *worm]]))
    ==
  =|  out=(list ovum)
  |-  ^-  [(list ovum) _+>.^$]
  ?~  ova
    [(flop out) +>.^$]
  ::  iterate over effects, handling those on arvo proper
  ::  and passing the rest through as output
  ::
  ::    In practice, the pending effects after an upgrade
  ::    are the %veer moves to install %zuse and the vanes,
  ::    plus a %vega notification that the upgrade is complete.
  ::
  ::    N.B. this implementation assumes that %vega will be
  ::    at the end of :ova.
  ::
  ?:  ?=(%vega -.q.i.ova)
    =^  zef=(list ovum)  vanes
      =<  abet:loop
      %.  i.ova
      %~  spam  le:part
      [our now eny lac vil (~(gas by *(map term vane)) vanes)]
    ::
    $(out [i.ova out], ova (weld t.ova zef))
  ::
  =^  vov  +>.^$  (feck now i.ova)
  =?  out  ?=(^ vov)  [+.vov out]
  $(ova t.ova)
::
++  peek                                                ::  external inspect
  |=  {now/@da hap/path}
  ^-  (unit (unit))
  ?~  hap  [~ ~ hoon-version]
  %.  [[151 %noun] hap]
  %-  sloy
  %~  peek  le:part
  [our now eny lac vil (~(gas by *(map term vane)) vanes)]
::
++  poke                                                ::  external apply
  |=  [now=@da ovo=ovum]
  =.  eny  (shaz (cat 3 eny now))
  ^-  [(list ovum) _+>.$]
  ::
  ::  These external events are actually effects on arvo proper.
  ::  They can also be produced as the effects of other events.
  ::  In either case, they fall through here to be handled
  ::  after the fact in +feck.
  ::
  ?:  ?=(?(%veer %verb %wack %warn) -.q.ovo)
    [[ovo ~] +>.$]
  ::
  ::  These external events (currently only %trim) are global
  ::  notifications, spammed to every vane
  ::
  ?:  ?=(%trim -.q.ovo)
    =>  .(ovo ;;((pair wire [%trim p=@ud]) ovo))
    =^  zef  vanes
      ^-  (pair (list ovum) (list (pair term vane)))
      =<  abet:loop
      %.  ovo
      %~  spam  le:part
      [our now eny lac vil (~(gas by *(map term vane)) vanes)]
    ::  clear compiler caches if high-priority
    ::
    =?  vanes  =(0 p.q.ovo)
      ~>  %slog.[0 leaf+"arvo: trim: clearing caches"]
      (turn vanes |=([a=@tas =vane] [a vase.vane *worm]))
    [zef +>.$]
  ::
  ::  Error notifications are unwrapped and routed as usual
  ::
  ?:  ?=(%crud p.q.ovo)
    ?.  ?=(^ q.q.ovo)
      ~|([%unknown-crud q.ovo] !!)
    ::
    =^  zef  vanes
      =*  el
        ~(. le:part [our now eny lac vil (~(gas by *(map term vane)) vanes)])
      ::
      =<  abet:loop
      ?@  -.q.q.ovo
        ::
        ::  legacy %crud, directly routed
        ::
        (poke:el (dint p.ovo) ovo)
      ::
      ::  modern %crud, unwrapped and routed w/ $goof
      ::
      =/  =goof  ;;(goof -.q.q.ovo)
      =/  =curd  ;;(curd +.q.q.ovo)
      (crud:el (dint p.ovo) goof p.ovo curd)
    ::
    [zef +>.$]
  ::  Normal events are routed to a single vane
  ::
  =^  zef  vanes
    =<  abet:loop
    %.  [(dint p.ovo) ovo]
    %~  poke  le:part
    [our now eny lac vil (~(gas by *(map term vane)) vanes)]
  ::
  [zef +>.$]
::  +feck: handle an arvo effect
::
++  feck
  |=  [now=@da ovo=ovum]
  ^-  [(unit ovum) _+>.$]
  ?+  -.q.ovo
      ::  pass through unrecognized effect
      ::
      [[~ ovo] +>.$]
  ::  toggle event verbose event printfs
  ::
      %verb
    [~ +>.$(lac !lac)]
  ::  install %zuse or vane
  ::
      %veer
    [~ (veer our now q.ovo)]
  ::  add data to memory profile
  ::
      %mass
    =.  q.q.ovo
      :-  %userspace
      :-  %|
      :~  hoon+&+pit
          zuse+&+bud
          :+  %caches  %|
          %+  turn
            %+  sort  vanes
            |=([a=[lab=@tas *] b=[lab=@tas *]] (aor lab.a lab.b))
          |=([label=@tas =vane] [(cat 3 %vane- label) %& worm.vane])
          q.q.ovo
          :+  %vases  %|
          %+  turn
            %+  sort  vanes
            |=([a=[lab=@tas *] b=[lab=@tas *]] (aor lab.a lab.b))
          |=([label=@tas =vane] [(cat 3 %vane- label) %& vase.vane])
          dot+&+.
      ==
    [[~ ovo] +>.$]
  ::  add entropy
  ::
      %wack
    ?>  ?=(@ q.q.ovo)
    =.  eny  (shaz (cat 3 eny q.q.ovo))
    [~ +>.$]
  ::  learn of event-replacement failure
  ::
      %warn
    :_  +>.$
    ?.  ?=(^ +.q.ovo)
      ~
    =/  msg=tape
      :(weld "(for %" (trip (symp +<.q.ovo)) ") failed")
    ~>  %slog.[0 leaf+(weld "arvo: replacement event " msg)]
    ?:  lac
      ~
    =/  rep
      %-  mule  |.
      ((slog (tang +>.q.ovo)) ~)
    ?.(?=(%& -.rep) ~ p.rep)
  ==
::
++  vega                                                ::  reboot kernel
  |=  $:  ::  now: current date
          ::  ova: actions to process after reboot
          ::  hun: hoon.hoon source
          ::  arv: arvo.hoon source
          ::
          now=@da
          ova=(list ovum)
          hun=@t
          van=@t
      ==
  ^-  (unit (pair (list ovum) *))
  ::  virtualize; dump error if we fail
  ::
  =-  ?:(?=(%| -.res) ((slog p.res) ~) `p.res)
  ^=  res  %-  mule  |.
  ::  produce a new kernel and an effect list
  ::
  ^-  (pair (list ovum) *)
  ::  compile the hoon.hoon source with the current compiler
  ::
  =/  raw
    ~&  [%hoon-compile `@p`(mug hun)]
    (ride %noun hun)
  ::  activate the new compiler gate, producing +ride
  ::
  =/  cop  .*(0 +.raw)
  ::  find the hoon version number of the new kernel
  ::
  =/  nex
    (@ .*(cop q:(~(mint ut p.raw) %noun [%limb %hoon-version])))
  ?>  |(=(nex hoon-version) =(+(nex) hoon-version))
  ::  if we're upgrading language versions, recompile the compiler
  ::
  ::    hot: raw compiler formula
  ::
  =>  ?:  =(nex hoon-version)
        [hot=`*`raw .]
      ~&  [%hoon-compile-upgrade nex]
      =/  hot  (slum cop [%noun hun])
      .(cop .*(0 +.hot))
  ::  extract the hoon core from the outer gate (+ride)
  ::
  =/  hoc  .*(cop [%0 7])
  ::  compute the type of the hoon.hoon core
  ::
  =/  hyp  -:(slum cop [-.hot '+>'])
  ::  compile arvo
  ::
  =/  rav
    ~&  [%arvo-compile `@p`(mug hyp) `@p`(mug van)]
    (slum cop [hyp van])
  ::  activate arvo, and extract the arvo core from the outer gate
  ::
  =/  voc  .*(hoc [%7 +.rav %0 7])
  ::  entry gate: ++load for the normal case, ++come for upgrade
  ::
  =/  gat
    =/  arm  ?:(=(nex hoon-version) 'load' 'come')
    ::  compute the type of the arvo.hoon core
    ::
    =/  vip  -:(slum cop [-.rav '+>'])
    ::  compute the formula for the upgrade gate
    ::
    =/  fol  +:(slum cop [vip arm])
    ::  produce the upgrade gate
    ::
    .*(voc fol)
  ::  upgrade gate sample
  ::
  =/  sam
    :*  our
        now
        eny
        ::  tack a notification onto the pending effects
        ::
        (weld ova [`ovum`[//arvo %vega ~] ~])
        bud
        (turn vanes |=([label=@tas =vane] [label vase.vane]))
    ==
  ::  call into the new kernel
  ::
  =/  out  (slum gat sam)
  ::  add types to the product
  ::
  [((list ovum) -.out) +.out]
::  +veer: install %zuse or a vane
::
::    Identity is in the sample so the larval stage
::    can use this as well.
::
++  veer
  |=  [who=ship now=@da fav=curd]
  =>  .(fav ;;({$veer lal/@ta pax/path txt/@t} fav))
  =-  ?:(?=(%| -.res) ((slog p.res) +>.$) p.res)
  ^=  res  %-  mule  |.
  ?:  =(%$ lal.fav)
    ~&  [%tang pax.fav `@p`(mug txt.fav)]
    =+  gen=(rain pax.fav txt.fav)
    =+  vax=(slap pit gen)
    +>.^$(bud vax)
  %_    +>.^$
      vanes
    |-  ^+  vanes
    ?~  vanes
      ~&  [%vane `@tas`lal.fav pax.fav `@p`(mug txt.fav)]
      =+  vin=(vint who lal.fav vil bud pax.fav txt.fav)
      ?~  vin
        vanes
      [[lal.fav vane:u.vin] vanes]
    ?.  =(lal.fav label.i.vanes)
      [i.vanes $(vanes t.vanes)]
    ~&  [%vane `@tas`lal.fav pax.fav `@p`(mug txt.fav)]
    :_  t.vanes
    :-  label.i.vanes
    ~|  [%failed-vane-activation now lal.fav]
    vane:(ruck:(vent who lal.fav vil bud [vase.vane.i.vanes *worm]) pax.fav txt.fav)
  ==
::
++  wish                                                ::  external compute
  |=  txt/@
  q:(slap bud (ream txt))
--
