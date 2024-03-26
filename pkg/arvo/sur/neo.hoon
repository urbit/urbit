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
+$  care
  $?  %x  :: single node
      %y  :: single node and immediate children
      %z  :: single node and all descendants
  ==
+$  tour   [=care =pith]
+$  block  [get=(set tour) err=(unit tang)]
+$  curb   @ud :: blocking request ID
+$  halt
  $:  by-tour=(map tour flow)
      by-flow=(jug flow tour)
      clog=(map flow (qeu move))
  ==
+$  flow  (pair pith pith)
+$  disk
  $@(~ [=ship =term])
+$  tack
  ?(%con %imp %pro)
+$  post  (pair tack stud)
++  get-stud-name
  |=  =stud
  ?@  stud  stud
  mark.stud
::
++  ford
  |%
  ++  dep  `fief`[& %x %ford-in %ford-out]
  ++  get-output
    |=  [=bowl =term]
    ^-  (unit vase)
    =+  !<([vax=(unit vase) *] q.pail.q:(~(got by deps.bowl) term))
    vax
  ::
  ++  run
    |=  txt=@t
    (scan (trip txt) (rein /))
  +$  lib
    [face=(unit term) =name]
  +$  pro
    [face=term =stud]
  +$  vale
    [face=term =stud]
  +$  file
    $:  pro=(list pro)
        :: grab=(list 
        lib=(list lib)
        =hoon
    ==
  ++  rein
    |=  pax=path
    =<  apex
    |%
    ++  nam
      :: ^-  $-(nail (like name:neo))
      ;~(plug ;~(pfix fas sig fed:ag) stip)
    ++  std
      ;~  pose
        ;~(plug sym ;~(pfix col sig fed:ag) ;~(pfix fas sym))
        sym
      ==
    ++  pro
      :: ^-  $-(nail (like ^pro))
      %+  rune  pat
      ;~  pose
        %+  cook
          |=  =stud
          ?@  stud  [stud stud]
          [mark.stud stud]
        std
        ;~(plug sym ;~(pfix gap std))
      ==
    ++  lib
      :: ^-  $-(nail (like ^lib))
      %+  rune  cen
      ;~  pose
        (stag ~ nam)
        ;~(plug (stag ~ sym) ;~(pfix gap nam))
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
      =+  vaz=(vang & pax)
      (ifix [gay gay] tall:vaz)
    ++  apex
      :: ^-  rule
      ;~  plug 
        pros
        libs
        hone
      ==
    --
  ++  with-face
    |=  [fac=@tas =vase]
    vase(p [%face fac p.vase])
  ++  with-faces
    |=  [reef=vase faces=(list (pair term vase))]
    ?~  faces
      reef
    $(reef (slop reef (with-face i.faces)), faces t.faces)
  --
++  behn
  |%
  +$  req  $>(?(%rest %wait) task:^behn)
  +$  res  $>(%wake gift:^behn)
  --

::  Total version
+$  ever  [node=@ud tree=@ud]
::  $once: reference to version
+$  once  $%([%node p=@ud] [%tree p=@ud])
::  $road: fully qualified path
+$  road   [=name =once grab=pith]
:: +$  pike   (each road name)
:: * A `$bolt` is a `[=stud =once]`
::
::  $tone: parent change tracking
+$  tone
  $%  [%dep =term =care =name]
      [%sync =care]
  ==
::  $sound: change tracking listeners
+$  sound
  $:  voice=(jug pith [to=pith =term]) :: %x listeners
      tones=(jug pith [to=pith =term]) :: %y listeners
      noise=(jug pith [to=pith =term]) :: %z listeners
  ==
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
        :-  '~'      ;~(pfix sig ;~(pose crub:so (easy [%n ~])))
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
  ++  en-cord
    |=  pit=$
    (spat (pout pit))
  ++  prefix
    =|  res=$
    |=  [long=$ curt=$]
    ^-  (unit _res)
    ?~  curt  `(flop res)
    ?~  long  ~
    ?.  =(i.long i.curt)
      ~
    $(long t.long, curt t.curt, res [i.long res])
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
+$  note
  $%  [%make =code init=(unit vase) =conf] :: todo: configuration values, init cannot be ^ if installing over
      [%poke =pail]
      [%tomb =case]
      [%link from=pith src=stud]
  ==
+$  poke
  (pair pith *)
+$  yard
  $+  yard
  $~  ~
  (map iota hall)
+$  rely
  [=term =stem]
+$  mode  ?(%add %dif %del)
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
    :-  [state:q.span.rot q.state.icon.rot]
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
  :-  [state:q.span.rot state.icon.rot]
  %-  ~(gas by *(map pith [ever pail]))
  =-  %+  turn  ~(tap by -)
      |=([k=pith v=[ever pail]] [(welp at k) v])
  ?-    care
      %x  ~
      %y
    =.  ax  ~(snip of ax)
    (~(run by ~(tar of ax)) |=(r=room [ever.icon.r (to-pail:room r)]))
  ::
      %z
    (~(run by ~(tar of ax)) |=(r=room [ever.icon.r (to-pail:room r)]))
  ==
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
+$  code
  $%  [%clay p=path]
      [%stud p=stud]
  ==
+$  span  (pair code firm)
+$  icon
  $:  =ever
      state=vase
      history=(list *)
      migration=(list *)
  ==
:: subscription metadata
+$  jail
  $+  jail
  $~  ~
  (map iota cell)
+$  cell
  $+  cell
  [case=@ud state=vase =span =jail]
+$  brig
  $+  brig  (axal cell)
  
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
    $~  [*span ~ *icon]
    $:  =span
        =conf
        =icon
    ==
  ++  to-vial
    |=  rom=room
    ^-  vial
    [state:q.span.rom q.state.icon.rom]
  ::
  ++  to-pail
    |=  rom=room
    ^-  pail
    [state:q.span.rom state.icon.rom]
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
  $:  src=name
      our=@p
      were=pith :: XX: rename to here
      here=pith
      now=@da
      deps=(map term (pair pith cane))
      kids=(map pith vase) :: XX: vase ->
  ==
+$  quay
  $%  [%x =port]
      [%y =dock]
      [%z =dock]
  ==
+$  fief  [required=? =quay]
+$  dock  [=port =kids]
+$  port :: TODO: how to specify behaviour
  [state=stud diff=stud] :: state, diff actually $stud
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
--
