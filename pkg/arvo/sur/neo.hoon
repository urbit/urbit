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
    $:  wer=name
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
  |^  ,[=ship =pith]  
  ++  en-pith
    |=  nam=$
    ^-  pith
    [p/ship.nam pith.nam]
  ++  en-tape
    |=  nam=$
    (spud (pout (en-pith nam)))
  ++  en-path
    |=  nam=$
    (pout (en-pith nam)) 
  ++  de-pith  |=(pith ~|(de-pith/+< (need (de-pith-soft +<))))
  ++  de-pith-soft
    |=  =pith
    ^-  (unit ^$)
    ?.  ?=([[%p @] *] pith)
      ~
    `[+.i.pith t.pith]
  --
+$  axal
  $~  [~ ~]
  [fil=(unit hall) kid=(map iota axal)]
++  of
  |_  fat=axal
  ++  del
    |=  pax=pith
    ^+  fat
    ?~  pax  [~ kid.fat]
    =/  kid  (~(get by kid.fat) i.pax)
    ?~  kid  fat
    fat(kid (~(put by kid.fat) i.pax $(fat u.kid, pax t.pax)))
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
    |=  lit=(list (pair pith hall))
    ^+  fat
    ?~  lit  fat
    $(fat (put p.i.lit q.i.lit), lit t.lit)
  ++  got-room
    |=  pax=pith
    ^-  room
    (de-hall:room (got pax))
  ++  got
    |=  pax=pith
    ~|  missing-room/pax
    (need (get pax))
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
    |=  [pax=pith dat=hall]
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
    (~(gas by *(map pith hall)) tap)
  --
+$  pate  [[%p p=ship] q=pith]
++  petty-port
  |*  a=mold
  ^-  port
  [a a]
+$  dita  (each iota aura)
+$  pish  (list dita)
+$  conf  (map term pith)
+$  card
  $%  [%arvo note-arvo]
      [%neo note]
  ==
+$  request
  [src=pith dest=pith val=*]
+$  response
  [src=pith dest=pith status=response-status]
+$  diff
  $%  [%poke p=*]
      [%init p=*]
  ==
+$  update
  $:  =pith
      case=@ud
      =diff
  ==
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
::
+$  note
  %+  pair  pith
  $%  [%make =stud init=(unit vase) =conf] :: todo: configuration values, init cannot be ^ if installing over
      [%poke val=*]
      [%tomb =case]
      [%link from=pith src=stud]
  ==
+$  poke
  (pair pith *)
+$  yard
  $+  yard
  $~  ~
  (map iota hall)
++  yird
  |%
  +$  yird
    $+  yird
    $~  ~
    (map iota $+(yird-inner $~([%& *hall] (each hall yird))))
  ++  put
    |=  [y=yird p=pith h=hall]
    ^+  y
    ?>  ?=(^ p)
    =/  in  (~(got by y) i.p)
    ?>  ?=(%| -.in)
    %+  ~(put by y)  i.p
    ?:  =(~ t.p)
      &/h
    [%| (~(put by y) i.p [%| $(y p.in, p t.p)])]
  ++  get-fit
    |=  [y=yird p=pith]
    ^-  (unit [pith hall])
    ?>  ?=(^ p)
    =/  in  (~(get by y) i.p)
    ?~  in
      ~
    ?~  t.p
      ?>  ?=(%& -.u.in)
      `[~ p.u.in]
    ?.  ?=(%| -.u.in)
      `[t.p p.u.in]
    $(y p.u.in, p t.p)

  ++  get
    |=  [y=yird p=pith]
    ^-  (unit hall)
    ?>  ?=(^ p)
    =/  in  (~(get by y) i.p)
    ?~  in
      ~
    ?~  t.p
      ?>  ?=(%& -.u.in)
      `p.u.in
    ?.  ?=(%| -.u.in)
      ~
    $(y p.u.in, p t.p)
  --
::
+$  sign-conf
  $%  [%val p=term]
      [%pith p=term q=pith]
  ==
+$  sign-neo
  $%  [%poke dest=pith status=response-status]
      [%conf p=sign-conf]
  ==
+$  sign
  $+  sign
  $%  [%arvo p=sign-arvo]
      [%neo p=sign-neo]
  ==

+$  move  (pair pith card)
+$  span  (pair stud firm)
+$  icon
  [case=@ud state=vase history=(list *) migration=(list *)]
:: subscription metadata
+$  jail
  $+  jail
  $~  ~
  (map iota cell)
+$  cell
  $+  cell
  [case=@ud state=vase =span =jail]
+$  brig
  $+  brig
  $~  ~
  (map pith cell)
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
  $:  src=@p
      our=@p
      were=pith
      now=@da
      deps=(map term (pair pith vase))
      kids=(map pith vase)
  ==
+$  fief
  [required=? =port]
+$  port :: TODO: how to specify behaviour
  [state=* diff=*] :: state, diff actually $stud
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
  ++  state  ** :: stud
  ::  $poke: a poke is a request to change a value in teh urbit
  ::  namespace.
  ::
  ::    For instance a blocked list that is a set of users would be
  ::      [%add who=user]
  ::      [%del who=user]
  ::
  ::
  ++  poke   ** :: stud
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
  ++  call
    |~  [prev=vase val=*]
    *(list card)
  ::  +reduce: apply %poke, producing state
  ::
  ::    ('liam'' ~) [%add who='ruby'] -> ('liam' 'ruby')
  ::    ('liam' 'ruby' ~) [%del who='ruby'] -> ('liam')
  ++  reduce
    |~  val=*
    *vase
  ++  take
    |~  =sign
    *(list card)
  ++  born
    *(list card)
  ++  init
    |~  old=(unit vase)
    *vase
  ++  echo
    |~  [=pith val=*]
    *(list card)
  --
--
