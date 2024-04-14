::  $neo: New Shrub
::
::    Urbit is a namespace, from a path -> data
::    /~hastuc-dibtux/chats/unit-731 :: this a chat 
::    /~hastuc-dibtux/chats/unit-731/msg/~2024.1.27..10.30 :: this is a
::    message inside the chat
::
::    neo is a recipe for defining the kinds of data that live at these
::    paths. For instance, you would maybe like to define a chat
::    datatype, that could be bound into your namespace, so that your
::    friends could send you memes.
::
::
::
::
::
::  
|%
::  $care: Perspective
::    
+$  care
  $?  %x  :: single node
      %y  :: single node and immediate children
      %z  :: single node and all descendants
  ==
::  $tour: perspective and shrub
::    
+$  tour   [=care =pith]
::  $block: call-stack state for blocking semantics
::    
+$  block  [get=(set tour) err=(unit tang)]
::  $halt: Currently blocked flows and indices
::
+$  halt
  $:  by-tour=(map tour flow)
      by-flow=(jug flow tour)
      clog=(map flow (qeu move))
  ==
::  $flow: Call direction
::  
::    .p is the source
::    .q is the destination
::
+$  flow  (pair pith pith)
::  $disk: Reference to a suite of code
::
::    If sig case, then refers to the %std disk
+$  disk
  $@(~ [=ship =term])
::  $tack: Type of code being distributed
::
::    %con: CONverter of protocols
::    %imp: IMPlemenation of shrub
::    %pro: PROtocol (type)
::
+$  tack
  ?(%con %imp %pro)
::  $post: Name of code being distributed
::
+$  post  (pair tack stud)
::
::  +get-stud-name: Get name for $stud
::
++  get-stud-name
  |=  =stud
  ?@  stud  stud
  mark.stud
::  +drive: Path multiplexer core
::
++  drive
  |%
  ::  +en:drive: Multiplex several paths into one
  ::    
  ::    See also: (+de:drive)
  ++  en
    =|  res=pith
    |=  ps=(list pith)
    ?~  ps
      res
    $(res (welp res [%ud (lent i.ps)] i.ps), ps t.ps)
  ::  +de:drive: Demultiplex one path into several
  ::    
  ::    See also: (+en:drive)

  ++  de
    =|  res=(list pith)
    |=  pax=(pole iota)
    ^+  res
    ?:  =(~ pax)
      (flop res)
    =^  nex=pith  pax
      ?>  ?=([[%ud len=@] rest=*] pax)
      `[pith (pole iota)]`[(scag [len rest]:pax) (slag [len rest]:pax)]
    $(res [nex res])
  --
::  +ford: Container for build system bootstrapping
++  ford
  |%
  ++  desk
    |%
    ++  kids  *^kids
    --
  ++  is-stud
    |=  s=stud
    ?^  s  |
    =(%ford (end [3 4] s))
  ::  +riff:ford: Constant build system node
  ::  
  ::    Required for bootstrapping. This is used to put the reef and
  ::    other ford combinators into the build system to bootstrap
  ::    everything else. To update a riff, simply %make over the top
  ::
  ++  riff
    ^-  firm
    |%
    ++  state  %ford-out
    ++  poke   *(set stud)
    ++  kids  ~
    ++  deps  ~
    ++  form
      ^-  ^form
      |_  [=bowl =ever state-vase=vase *]
      +*  sta  !<([cache=(unit vase) ~] state-vase)
      ++  poke
        |=  =pail
        ^-  (quip card vase)
        !!
      ::
      ++  init
        |=  old=(unit vase)
        ^-  (quip card vase)
        =+  !<(ref=vase (need old))
        `!>(`[cache=(unit vase) ~]`[`ref ~])
      --
    --
  ::  +dep:ford: $fief for a ford dependency
  ::  
  ::    Handy shortcut to specifiy a dependency in the build system
  ++  dep  `fief`[& [%ford-out %ford-in] ~]
  ::  +get-output: pull build resuit of dependency
  ::
  ++  get-output
    |=  [=bowl =term]
    ^-  (unit vase)
    =/  outer   q.pail.q:(~(got by deps.bowl) term)
    =+  !<([vax=(unit vase) *] outer)
    vax
  ::
  ++  run
    |=  txt=@t
    (scan (trip txt) (rein *name))
  +$  loc
    [=disk =pith]
  ::  $lib:ford: Specification of library import
  ::
  +$  lib
    [face=term =loc]
  ::  $pro:ford: Specification of protocol import
  ::
  +$  pro
    [face=term =stud]
  +$  vale
    [face=term =stud]
  ::  $file:ford: Code with imports
  ::
  +$  file
    $:  pro=(list pro)
        :: grab=(list 
        lib=(list lib)
        =hoon
    ==
  ::  +rein:ford: Parse code with imports
  ++  rein
    |=  =name
    =<  apex
    |%
    ++  dis  
      ;~  pose
        (cold ~ cab)
        ;~((glue bar) ;~(pfix sig fed:ag) sym)
      ==
    ::  +lib-loc: Parse library location
    ::
    ++  lib-loc
      ;~(plug dis stip)
    ::  +old-nam: Parse file path (deprecated: XX revisit)
    ::
    ::     Examples:
    ::     Absolute ~hastuc-dibtux/src/foo/bar/test
    ::     Relative %^/bar
    ++  old-nam
      :: ^-  $-(nail (like name:neo))
      ;~  pose
        %+  sear 
          |=  [kets=(list) pit=pith]
          ^-  (unit ^name)
          %-  mole
          |.  ^-  ^name
          =.  pit  (scag (sub (lent pit) (lent kets)) pit)
          =-  ~&(parsed-name/- -)
          name(pith (welp pith.name pit))
        ;~(pfix cen ;~(plug (star ket) stip))                     :: relative
        ;~(plug ;~(pfix fas sig fed:ag) stip) :: absolute
      ==
    ::  +std:rein:ford: Parse import directive
    ::
    ::    Either  name:~ship/desk
    ::    or      name (from %std disk)
    ::
    ++  std
      ;~  pose
        ;~(plug sym ;~(pfix col sig fed:ag) ;~(pfix fas sym))
        sym
      ==
    ::  +pro:rein:ford: Parse protocol import directive
    ::
    ::    /@  foo=bar  :: imports %bar protocol from %std disk with name foo
    ::    /@  bar      :: imports %bar protocol from %std disk with name bar
    ::
    ++  pro
      :: ^-  $-(nail (like ^pro))
      %+  rune  pat
      ;~  pose
        ;~(plug sym ;~(pfix tis std))
        %+  cook
          |=  =stud
          ?@  stud  [stud stud]
          [mark.stud stud]
        std
      ==
    ++  lib
      %+  rune  hep
      ;~  pose
        ;~(plug sym ;~(pfix tis lib-loc))
        %+  cook
          |=  [=disk =pith]
          ^-  ^lib
          =/  last  (rear pith)
          ?>  ?=(@ last)
          [`@tas`last disk pith]
        lib-loc
      ==

    ::  +old-lib: Parse arbitrary library import directive
    ::
    ::    Unused, todo revive with more recursive build system
    ::
    ::    /-  face=~hastuc-dibtux/foo/bar <- imports ~hastuc-dibtux
    ::    /-  %^/bar <- imports bar/hoon up one level with face bar
    ::
    ++  old-lib
      :: ^-  $-(nail (like ^lib))
      %+  rune  hep
      ;~  pose
        ;~(plug sym ;~(pfix tis old-nam))
        %+  cook
          |=  n=^name
          =/  last  (rear pith.n)
          :_  n
          ?@  last  last
          (scot last)
        old-nam
      ==
    ++  rune
      |*  [car=rule rul=rule]
      (ifix [;~(plug fas car gap) gay] rul)

    ++  libs
      :: ^-  $-(nail (like (list ^lib)))
      (star lib)
    ++  pros
      :: ^-  $-(nail (like (list ^pro)))
      (star pro)
    ++  hone
      :: ^-  $-(nail (like hoon))
      =+  vaz=(vang & (en-path:^name name))
      (ifix [gay gay] tall:vaz)
    ++  apex
      :: ^-  rule
      ;~  plug 
        pros
        libs
        hone
      ==
    --
  ::  +with-face:ford: Decorate vase with face
  ::
  ++  with-face
    |=  [fac=@tas =vase]
    vase(p [%face fac p.vase])
  ::  +with-faces:ford: Decorate vases with faces, slopped onto reef
  ::
  ++  with-faces
    |=  [reef=vase faces=(list (pair term vase))]
    ?~  faces
      reef
    $(reef (slop (with-face i.faces) reef), faces t.faces)
  --
::  +behn: Timer vane
++  behn
  |%
  ::  $req: Timer request
  ::
  ::    %wait: Set timer
  ::    %rest: Cancel timer
  ::
  +$  req  $>(?(%rest %wait) task:^behn)
  ::  $behnres: Timer response
  ::
  ::    %wake: Timer went off
  ::
  +$  res  $>(%wake gift:^behn)
  --
::  +clay: Filesystem overlay
::
++  clay
  |%
  ::  $peer:clay: Request for file(s) subscription
  ::
  +$  peer  [=desk =path]
  ::
  ::  $req:clay: Filesystem request
  ::   
  ::    %peer: Setup file subscription at .pith
  ::    %pull: Cancel file subscripiton at .pith
  ::
  +$  req
    $%  [%peer =pith =peer]
        [%pull =pith]
    ==
  ::  $res:clay: Filesystem response
  +$  res  [=pith case=@ud files=(axol cage)]
  --
::
::  $ever: Total shrub version
::
::    .node is incremented only when the shrub itself changes i.e. it
::    versions the %x care
::    .tree is incremented when the shrub or any of its children changes
::    i.e. it versions the %y care
::
+$  ever  [node=@ud tree=@ud]
::  $once: Partial version
::
::    Identify shrub by either %node or %tree, as per $ever
::
+$  once  $%([%node p=@ud] [%tree p=@ud])
::
::  $road: fully qualified path
+$  road   [=name =once grab=pith]
:: * A `$bolt` is a `[=stud =once]`
::
::  $peer: Subscription
+$  peer
  [=pulp =path]

::  $tone: parent change tracking
::
+$  tone
  $%  [%peer =peer]
      [%rely =term =pith]
  ==
::  $sound: internal change tracking listeners
::
+$  sound
  (jug tour tone)
::
::  $noise: external change tracking listeners
+$  noise
  (jug tour rely)
::  $rave: foreign dependency
+$  rave
  [=term =pith]
::  $riot: foreign mirror
+$  riot
  [=cane deps=(set rave) =slip]
::
::  $ring: node change tracking
::
+$  ring
  $%  [%dep p=term =care q=name]
      [%sync ~]
  ==
::
+$  out
  $%  [%sync p=tour]
      [%stop p=tour]
  ==
++  pave
  |=  p=path
  ^-  pith
  %+  turn  p
  |=  i=@ta
  (fall (rush i spot:stip) [%ta i])
::
++  stip                                                ::  typed path parser 
  =<  swot
  |%
  ++  swot  |=(n=nail `(like pith)`(;~(pfix fas (more fas spot)) n))
  ::
  ++  spot
    %+  sear
      |=  a=*
      ^-  (unit iota)
      ?+  a  ~
        @      ?:(((sane %tas) a) [~ `@tas`a] ~)
        [@ @]  ((soft iota) a)
      ==
    %-  stew
    ^.  stet  ^.  limo
    :~  :-  'a'^'z'  sym
        :-  '$'      (cold [%tas %$] buc)
        :-  '0'^'9'  bisk:so
        :-  '-'      tash:so
        :-  '.'      zust:so
        :-  '~'      ;~(pfix sig ;~(pose (stag %da (cook year when:so)) crub:so (easy [%n ~])))
        :-  '\''     (stag %t qut)
    ==
  --
::
++  goon
  |%
  ::  $date: date w/ TZ offset
  +$  date   [dat=@da off=@ud]
  ::  $size: size of a rect
  +$  size   [w=@ud h=@ud]
  ::  $hsrc:  HTTP source (URL)
  +$  hsrc   @t
  ::  $dims: Spatial dimensions
  +$  dims   [ideal=size min=(unit size)]
  ::  $dimt: Temporal dimension
  +$  dimt   [len=@dr sta=@ud]
  +$  scar
    $?  %patp
        %patud
        %cord
        %patda
        %date
        %img
        %video
        %audio
    ==
  +$  clot
    $?  [%patp p=@p]
        [%patud p=@ud]
        [%cord p=cord]
        [%patda p=@da]
        [%date =date]
        [%img =hsrc =dims]
        [%video =hsrc =dims =dimt]
        [%audio =hsrc =dimt]
    ==
  --

++  pike
  =<  pike
  |%
  ++  card
    $%  [%peek =path]
        [%grab items=(list item)]
    ==
  ++  sign
    $%  [%peek =cage]
        [%grab items=(list clot:goon)]
    ==
  +$  item
    $:  lede=cord
        info=cord
        err=(unit cord)
        =scar:goon
    ==
  +$  bowl
    $:  our=@p
        wer=name
        eny=@uvJ
        now=@da
    ==
  +$  input  [=bowl syn=(unit sign)]
  ++  raw
    |%
    ++  output
      |*  a=mold
      $~  [%done *a]
      $%  [%emit =card]
          [%cont self=(form a)]
          [%fail err=(pair term tang)]
          [%done value=a]
      ==
    ++  form  |*(a=mold $-(input (output a)))
    --
  ++  fail
    |=  err=(pair term tang)
    |=  input
    [~ %fail err]
  ++  pikv
    (pike vase)
  ++  pike
    |*  a=mold
    |%
    ++  output  (output:raw a)
    ++  form    (form:raw a)
    ++  pure    
      |=  arg=a
      ^-  form
      |=  input
      [%done arg]
    ++  bind
      |*  b=mold
      |=  [m-b=(form:raw b) fun=$-(b form)]
      ^-  form
      =*  loop  $
      |=  in=input
      =/  b-res=(output:raw b)
        (m-b in)
      ^-  output
      ?-    -.b-res
        %emit   [%emit card.b-res]
        %cont   [%cont loop(m-b self.b-res)]
        %fail   [%fail err.b-res]
        %done   [%cont (fun value.b-res)]
      ==
    +$  eval-form
      $:  =form
      ==
    ::
    ::  Convert initial form to eval-form
    ::
    ++  from-form
      |=  =form
      ^-  eval-form
      form
    ::
    ::  The cases of results of +take
    ::
    +$  eval-result
      $%  [%emit car=card]
          [%fail err=(pair term tang)]
          [%done value=a]
      ==
    ++  take
      |=  [=eval-form =input]
      ^-  [=eval-result _eval-form]
      =*  take-loop  $
      :: =?  car.input  ?=(^ car.input)
      =/  =output  (form.eval-form input)
      ?-    -.output
          %emit  [[%emit card.output] eval-form]
          %fail  [[%fail err.output] eval-form]
          %done  [[%done value.output] eval-form]
          %cont 
        %_  take-loop
          form.eval-form  self.output
          input    [bowl.input ~]
        ==
      ==
    --
  --
::  $stud: mark name
+$  stud
  $@  @tas                                 ::  auth=urbit
  $:  mark=@tas                            :: 
      [=ship =desk]
  ==                                            ::
::
++  pith
  |^  $+(pith ^pith)
  ++  en-tape
    |=  pit=$
    (spud (pout pit))
  ++  sub
    |=  [from=$ del=$]
    ~|  pith-sub/[from del]
    !.
    |-  ^+  from
    ?~  del  from
    ?>  ?=(^ from)
    ?>  =(i.del i.from)
    $(del t.del, from t.from)
  ::
  ++  en-cord
    |=  pit=$
    (spat (pout pit))
  ::
  ++  prefix
    =|  res=$
    |=  [long=$ curt=$]
    ^-  (unit _res)
    ?~  curt  `(flop res)
    ?~  long  ~
    ?.  =(i.long i.curt)
      ~
    $(long t.long, curt t.curt, res [i.long res])
  ::
  ++  suffix
    |=  [long=$ curt=$]
    ^-  _curt
    ?~  curt
      long
    ?~  long
      ~
    $(curt t.curt, long t.long)
  --
++  name
  =<  name
  |%
  +$  name  [=ship =pith]  
  ++  rule
    :: ^-  _|~(nail *(like name))
    ;~(plug ;~(pfix fas sig fed:ag) stip)
  ++  en-pith
    |=  nam=name
    ^-  pith
    [p/ship.nam pith.nam]
  ++  en-tape
    |=  nam=name
    (spud (pout (en-pith nam)))
  ++  en-path
    |=  nam=name
    (pout (en-pith nam)) 
  ++  de-pith  |=(pith ~|(de-pith/+< (need (de-pith-soft +<))))
  ++  de-pith-soft
    |=  =pith
    ^-  (unit name)
    ?.  ?=([[%p @] *] pith)
      ~
    `[+.i.pith t.pith]
  --
++  axol  ^axal
++  axol-of  ^of
++  axal
  |$  [item]  
  [fil=(unit item) kid=(map iota $)]
++  axil
  |$  [item]
  [fil=(unit item) kid=(map pith item)]
++  of
  =|  fat=(axal)
  |@ 
  ++  view
    =|  res=(map pith _?>(?=(^ fil.fat) u.fil.fat))
    |=  [=care pax=pith]
    =.  fat  (dip pax)
    =?  res  ?=(^ fil.fat)
     (~(put by res) ~ u.fil.fat)
    ?-  care
      %x  res
      %y  =.(fat snip (~(uni by res) tar))
      %z  (~(uni by res) tar)
    ==
  ::
  ++  anc-jab
    |*  [pax=pith fun=$-(* *)]
    ^+  fat
    ?~  pax
      fat
    =?  fil.fat  ?=(^ fil.fat)
      `(fun u.fil.fat)
    fat(kid (~(put by kid.fat) i.pax $(fat (~(got by kid.fat) i.pax), pax t.pax)))
    
  ::
  ++  anc
    =|  res=(list pith)
    =|  cur=pith
    |=  pax=pith
    ^-  (set pith)
    ?~  pax
      (~(gas in *(set pith)) res)
    =?  res  ?=(^ fil.fat)
      [cur res]
    $(fat (~(got by kid.fat) i.pax), pax t.pax, cur (snoc cur i.pax))
  ++  parent
    =|  res=(unit pith)
    =|  cur=pith
    |=  pax=pith
    |-  ^+  res
    ?~  pax
      res
    =?  res  ?=(^ fil.fat)
      `cur
    =/  nex  (~(get by kid.fat) i.pax)
    ?~  nex
      res
    $(fat u.nex, pax t.pax, cur (snoc cur i.pax))
  ++  snip
    |-  ^+  fat
    =*  loop  $
    %_    fat
        kid
      %-  ~(run by kid.fat)
      |=  f=_fat
      ?^  fil.f
        [`u.fil.f ~]
      loop(fat f)
    ==
  ::
  ++  kid
    |=  pax=pith
    ^-  (map pith _?>(?=(^ fil.fat) u.fil.fat))
    =.  fat  (dip pax)
    =.  fat  snip
    =.  fil.fat  ~
    tar
  ::
  ++  kids
    |=  pax=pith
    ^-  (axil _?>(?=(^ fil.fat) u.fil.fat))
    :-  (get pax)
    (kid pax)
  ::
  ++  del
    |=  pax=pith
    ^+  fat
    ?~  pax  [~ kid.fat]
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  fat
    fat(kid (~(put by kid.fat) i.pax $(fat u.kid, pax t.pax)))
  ::
  ::  Descend to the axal at this path
  ::
  ++  dip
    |=  pax=pith
    ^+  fat
    ?~  pax  fat
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  [~ ~]
    $(fat u.kid, pax t.pax)
  ::
  ++  gas
    |*  lit=(list (pair pith _?>(?=(^ fil.fat) u.fil.fat)))
    ^+  fat
    ?~  lit  fat
    $(fat (put p.i.lit q.i.lit), lit t.lit)
  ++  got
    |=  pax=pith
    ~|  missing-room/pax
    (need (get pax))
  ++  gut
    |*  [pax=pith dat=*]
    =>  .(dat `_?>(?=(^ fil.fat) u.fil.fat)`dat, pax `pith`pax)
    ^+  dat
    (fall (get pax) dat)
  ::
  ++  get
    |=  pax=pith
    fil:(dip pax)
  ::  Fetch file at longest existing prefix of the path
  ::
  ++  fit
    |=  pax=pith
    ^+  [pax fil.fat]
    ?~  pax  [~ fil.fat]
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  [pax fil.fat]
    =/  low  $(fat u.kid, pax t.pax)
    ?~  +.low
      [pax fil.fat]
    low
  ::
  ++  has
    |=  pax=pith
    !=(~ (get pax))
  ::  Delete subtree
  ::
  ++  lop
    |=  pax=pith
    ^+  fat
    ?~  pax  fat
    |-
    ?~  t.pax  fat(kid (~(del by kid.fat) i.pax))
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  fat
    fat(kid (~(put by kid.fat) i.pax $(fat u.kid, pax t.pax)))
  ::
  ++  put
    |*  [pax=pith dat=*]
    =>  .(dat `_?>(?=(^ fil.fat) u.fil.fat)`dat, pax `pith`pax)
    |-  ^+  fat
    ?~  pax  fat(fil `dat)
    =/  kid  (~(gut by kid.fat) i.pax ^+(fat [~ ~]))
    fat(kid (~(put by kid.fat) i.pax $(fat kid, pax t.pax)))
  ::
  ++  tap
    =|  pax=pith
    =|  out=(list (pair pith _?>(?=(^ fil.fat) u.fil.fat)))
    |-  ^+   out
    =?  out  ?=(^ fil.fat)  :_(out [pax u.fil.fat])
    =/  kid  ~(tap by kid.fat)
    |-  ^+   out
    ?~  kid  out
    %=  $
      kid  t.kid
      out  ^$(pax (weld pax /[p.i.kid]), fat q.i.kid)
    ==
  ::  Serialize to map
  ::
  ++  tar
    (~(gas by *(map pith _?>(?=(^ fil.fat) u.fil.fat))) tap)
  --
+$  pate  [[%p p=ship] q=pith]
++  petty-port
  |*  a=mold
  ^-  port
  [a a]
+$  dita  (each iota aura)
+$  pish  (list dita)
+$  conf  (map term pith)
+$  card  (pair pith note)
+$  request
  [src=pith dest=pith val=*]
+$  ack  (unit nack)
+$  nack
  $%  [%get p=(set pith)]
      [%sec p=(set pith)]
      [%err p=tang]
  ==
+$  update  diff
+$  watch  (list update)
::
+$  err
  $%  [%here =pith]
      [%goof err=*]
      [%fail err=*]
  ==

+$  response-status
  $%  [%done ~]
      err
  ==
+$  page  (pair stud *)
:: +$  cage  (pair stud vase)
::
+$  made
  [=stud init=(unit vase) =conf]
+$  note
  $%  [%make made] :: todo: configuration values, init cannot be ^ if installing over
      [%poke =pail]
      [%tomb cas=(unit case)]    :: tom
      [%link from=pith src=stud] :: XX deprecate
  ==
+$  raw-poke
  (pair flow vial)
+$  yard
  $+  yard
  $~  ~
  (map iota hall)
+$  rely
  [=term =stem]
+$  mode  ?(%add %dif %del)
::
+$  dish  (pair pith mode)
::
+$  stem
  $~  [[0 0] %x %stud *vase]
  %+  pair  ever
  $%  [%x =pail]
      [%y =pail kids=(map pith [=ever =mode =pail])]
      [%z =pail kids=(map pith [=ever =mode =pail])]
  ==
+$  twig
  $~  [[0 0] %x %stud ~]
  %+  pair  ever
  $%  [%x =vial]
      [%y =vial kids=(map pith [=ever =mode =vial])]
      [%z =vial kids=(map pith [=ever =mode =vial])]
  ==
++  bulb  [=ever =pail]
++  wand
  |^
  $~  [%x [0 0] [%$ ~] ~]
  $:  =care
      =ever
      =vial
      kids=(map pith [=ever =vial])
  ==
  ++  make
    |=  [=care at=pith ax=(axal room)]
    ^-  ^$
    =/  rot  (need fil.ax)
    :+  care  ever.icon.rot
    :-  [state.rot q.state.icon.rot]
    %-  ~(gas by *(map pith [ever vial]))
    =-  %+  turn  ~(tap by -)
        |=([k=pith v=[ever vial]] [(welp at k) v])
    ?-    care
        %x  ~
        %y
      =.  ax  ~(snip of ax)
      (~(run by ~(tar of ax)) |=(r=room [ever.icon.r (to-vial:room r)]))
    ::
        %z
      (~(run by ~(tar of ax)) |=(r=room [ever.icon.r (to-vial:room r)]))
    ==  
  --
+$  pulp  ?(%noun %json)
+$  cane
  $~  [%x [0 0] [%$ *vase] ~]
  $:  =care
      =ever
      =pail
      kids=(map pith [=ever =pail])
  ==
::
++  make-cane
  |=  [=care at=pith ax=(axal room)]
  ^-  cane
  =/  rot  (need fil.ax)
  :+  care  ever.icon.rot
  :-  [state.rot state.icon.rot]
  %-  ~(gas by *(map pith [ever pail]))
  =-  %+  turn  ~(tap by -)
      |=([k=pith v=[ever pail]] [(welp at k) v])
  ?-    care
      %x  ~
      %y
    =.  ax  ~(snip of ax)
    =.  fil.ax  ~
    (~(run by ~(tar of ax)) |=(r=room [ever.icon.r (to-pail:room r)]))
  ::
      %z
    =.  fil.ax  ~
    (~(run by ~(tar of ax)) |=(r=room [ever.icon.r (to-pail:room r)]))
  ==
++  dejs
  =,  dejs:format
  |%
  ++  pail
    |=  fun=$-([@ json] vase)
    |=  jon=json
    ^-  ^pail
    ?>  ?=(^ jon)
    ?>  ?=(%o -.jon)
    =/  std  (~(got by p.jon) %stud)
    =/  dat  (~(got by p.jon) %data)
    ?>  ?=(%s -.std)
    [`@tas`p.std (fun [p.std dat])]
  --

++  enjs
  =,  enjs:format
  |%
  ++  ever
    |=  eve=^ever
    ^-  json
    %-  pairs
    :~  node/(numb node.eve)
        tree/(numb tree.eve)
    ==
  ::
  ++  cane
    |=  [can=^cane con=$-(pail json)]
    ^-  json
    =,  enjs:format
    %-  pairs
    :~  care/s/care.can
        ever/(ever ever.can)
        pail/(con pail.can)
        :-  %kids
        %-  pairs
        %+  turn  ~(tap by kids.can)
        |=  [pit=pith eve=^ever pal=pail]
        ^-  [@t json]
        :-  (en-cord:pith pit)
        %-  pairs
        :~  ever/(ever eve)
            pail/(con pal)
        ==
    ==
  --
++  en-html
  |_  [basename=pith here=pith]
  ++  basetape  |=(=care (en-tape:pith :(welp basename ~[care] here)))
  ++  ever
    |=  eve=^ever
    ^-  manx
    ;dl.ever-neo
      ;dt: Node
      ;dd: {(a-co:co node.eve)}
    ::
      ;dt: Tree
      ;dd: {(a-co:co tree.eve)}
    ==
  ++  link
    |=  [=care pax=pith]
    ^-  manx
    =/  href=tape  "{(welp (basetape care) (en-tape:pith pax))}.html"
    ;a(href href): As {<care>}
  ++  path
    |=  pax=pith
    ^-  manx
    ;details
      ;summary: {(en-tape:pith pax)}
      ;ul
        ;li
          ;+  (link %x pax)
        ==
        ;li
          ;+  (link %y pax)
        ==
        ;li
          ;+  (link %z pax)
        ==
      ==
    ==
  ++  stud
    |=  std=^stud
    ^-  tape
    ?@  std
      (trip std)
    "{(trip mark.std)}/{(scow %p ship.std)}/{(trip desk.std)}"
    
  ::
  ++  cane
    |=  [can=^cane con=$-(pail manx)]
    ^-  manx
    ;dl.cane-neo
      ;dt: Care
      ;dd.care-neo: {(trip care.can)}
    ::
      ;dt: Ever
      ;dd
        ;+  (ever ever.can)
      ==
      ;dt: Type
      ;dd: {(stud p.pail.can)}
    ::
      ;dt: Value
      ;dd
        ;+  (con pail.can)
      ==
    ::
      ;dt: Children
      ;dd
        ;*  
        %+  turn  ~(tap by kids.can)
        |=  [pit=pith eve=^ever pal=pail]
        ^-  manx
        ;dl
          ;dt: Path
          ;dd
            ;+  (path pit)
          ==
        ::
          ;dt: Ever
          ;dd 
            ;+  (ever eve)
          ==
        ::
          ;dt: Value
          ;dd
            ;+  (con pal)
          ==
        ==
      ==
    ==
  ++  lift-to-hymn
    |=  [pax=pith in=manx]
    ^-  manx
    ;html
      ;head
        ;title: {(en-tape:pith pax)}
      ==
      ;body
        ;+  in  
      ==
    ==

  --
::
::  !!stud refers to imp/ different from $vial
+$  diff  [=pith =ever dat=(unit vial)]
:: +$  mace  [=pith =ever =diff]
::
++  sign
  |^
  $%  [%poke status=response-status]
      [%conf conf]
  ==
  +$  conf
    $%  [%val ~]
        [%pith ~ q=pith]
    ==
  --
+$  pail  (pair stud vase)
:: $ewer: deprecated
+$  ewer  (pair stud vase)
+$  vial  (pair stud *)
+$  move  (pair pith card)
+$  icon
  $:  =ever
      state=vase
      history=(list *)
      migration=(list *)
  ==
:: subscription metadata
+$  brig
  $+  brig  (axal cane)
  
+$  fleet
  $+  fleet
  $~  ~
  (map ship brig)
+$  hall  hall:room
::  $room: state of a shrub
::    
::    TODO: refactor for networking?
++  room
  =<  room
  |%
  +$  hall
    $%  [%exit pith]
        [%room room]
    ==
  +$  room
    $+  room
    $:  code=stud
        state=stud
        =conf
        =icon
    ==
  ++  to-vial
    |=  rom=room
    ^-  vial
    [state.rom q.state.icon.rom]
  ::
  ++  to-pail
    |=  rom=room
    ^-  pail
    [state.rom state.icon.rom]
  ::
  ++  de-hall-soft
    |=  hal=hall
    ^-  (unit room)
    ?.  ?=(%room -.hal)
      ~
    `+.hal
   ++  de-hall
     |=  hal=hall
     (need (de-hall-soft hal))
  --
+$  bowl
  $:  src=name :: [=ship =pith]
      our=@p   :: our
      were=pith :: XX: rename to here
      here=pith :: 
      now=@da
      deps=(map term (pair pith cane))
      kids=(map pith pail)
  ==
++  quay
  =<  quay
  |%
  +$  quay   (pair port (unit (pair care kids)))
  ++  get-care
    |=  q=quay
    ^-  care
    ?~  q.q
      %x
    p.u.q.q
  --
+$  fief  [required=? =quay]
+$  dock  [=port =kids]
+$  port :: TODO: how to specify behaviour
  [state=stud diff=stud] :: state, diff actually $stud
+$  slip
  [state=stud diffs=(set stud) =kids]
+$  deps  (map term fief)
+$  kids  (map pish port)
::  $firm: type of the value in the urbit namespace
::
+$  firm
  $_  ^&
  |%
  ::  $state: the state of this value in the urbit namespace
  ::
  ::    For instance, a message would be
  ::    ```hoon
  ::    [author=ship time-sent=time message=txt]
  ::    ```
  ::
  ::    ```
  ++  state  *stud
  ::  $poke: a poke is a request to change a value in teh urbit
  ::  namespace.
  ::
  ::    For instance a blocked list that is a set of users would be
  ::      [%add who=user]
  ::      [%del who=user]
  ::
  ::
  ++  poke   *(set stud)
  ++  form   *^form
  ::
  ::  +kids: Some nodes in the namespace define what children are
  ::  allowed to be under them. For instance, it should not  be allowed
  ::  to create /~hastuc-dibtux/chats/unit-731/blog-post-1. This is
  ::  nonsensical because blog posts don't go in chats.
  ++  kids   *(map pish port)
  ::
  ::  +deps: Some nodes in the namespace might like to hear about other
  ::  things that happen in the namespace. For instance, a substack-type
  ::  software would like to know where the wallet software is located
  ::  in the name
  ++  deps   *(map term fief)
  --
::
+$  form
  $_  ^|
  |_  [=bowl =icon]
  ::  +reduce: apply %poke, producing state and IO
  ::
  ::    ('liam'' ~) [%add who='ruby'] -> ('liam' 'ruby')
  ::    ('liam' 'ruby' ~) [%del who='ruby'] -> ('liam')
  ++  poke
    |~  [=stud val=vase]
    *(quip card vase)
  ++  init
    |~  old=(unit vase)
    *(quip card vase)
  --
++  peon
  |%
  ++  match
    |=  [nedl=pish hstk=pith]
    ^-  ?
    ?~  nedl
      =(~ hstk)
    ?~  hstk
      |
    ?:  ?=(%& -.i.nedl)
      &(=(p.i.nedl i.hstk) $(nedl t.nedl, hstk t.hstk))
    ?@  i.hstk
      =(p.i.nedl %tas)
    &(=(-.i.hstk p.i.nedl) $(nedl t.nedl, hstk t.hstk))
      
  ++  find
    |=  [pax=pith pis=(set pish)]
    ^-  (unit pish)
    ?~  lis=(skim ~(tap in pis) |=(pish (match +< pax)))
      ~
    `i.lis
  --
--
