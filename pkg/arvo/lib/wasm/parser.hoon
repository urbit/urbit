::  WebAssembly parser
::
/-  wasm-lia
=>  wasm-lia
~%  %wasm-parser-v0  +  ~
|%
++  parser
  =/  sur  wasm-sur
  |%
  ::  ++main: parsing function. Extends octstream with
  ::  trailing zeros, then applies ++module:r parsing rule.
  ::
  ++  main
    |=  wasm=octs
    =,  sur
    ^-  module
    =|  out=module
    =/  bytes=tape  (trip q.wasm)
    ::  add trailing zeros
    ::
    =.  bytes
      %+  weld  bytes
      ^-  tape
      (reap (sub p.wasm (lent bytes)) '\00')
    (scan bytes module:r)
    ::
    ::  |r: core with parsing rules.
    ::  Parsing rules often use the same name as the types
    ::  in /sur/wasm/hoon. Whenever you need a type make sure
    ::  to point at the structure file. For example,
    ::  num-type in |r refers to the parsing rule, which
    ::  returns a noun with the type num-type:sur
    ::
  ++  r
    |%
    ::  ++womp: returns a mold of a product of rule
    ::
    ++  womp
      |*  rul=rule
      $_  =+  vex=(rul)
      ?>  ?=(^ q.vex)
      p.u.q.vex
    ::  ++bild: connects an edge with a rule and a rule-
    ::  producing gate, returning the result of the
    ::  rule produced by slamming the gate with
    ::  the result of the first rule
    ::
    ++  bild
      |*  [vex=edge gat=_=>(rule |*(* *rule))]
      ?~  q.vex
        vex
      %.  [vex (gat p.u.q.vex)]
      (comp |*([a=* b=*] b))
    ::  ++bonk: rule modifier. Applies two rules to a nail,
    ::  failing if either failed or if the rules returned
    ::  different continuation. Returns the result of
    ::  the second rule otherwise.
    ::
    ++  bonk
      |*  [tet=rule fes=rule]
      |=  tub=nail
      ^+  (fes)
      =+  try=(tet tub)
      ?~  q.try  try
      =+  zen=(fes tub)
      ?~  q.zen  zen
      ?:  =(q.u.q.try q.u.q.zen)
        zen
      (fail tub)
    ::  ++feel: rule modifier. Tests equality of
    ::  the parsing result with a given noun
    ::
    ++  feel
      |*  [a=* sef=rule]
      |=  tub=nail
      =+  vex=(sef tub)
      ?~  q.vex  vex
      ?:  =(a p.u.q.vex)
        vex
      [p=p.vex q=~]
    ::  ++u-n: parse uN integer as an atom
    ::
    ++  u-n
      |=  n-bits=@
      =*  this  $
      %+  knee  *@
      |.  ~+
      ;~  pose
        ::  multiple byte case
        ::
        ?:  (lte n-bits 7)  fail
        %+  cook
          |=  [n=@ m=@]
          %+  add
            (mul 128 m)
          (sub n 128)
        ;~  plug
          (shim 128 255)
          this(n-bits (sub n-bits 7))
        ==
        ::  single byte case
        ::
        (cook ,@ (shim 0 (dec (bex (min n-bits 7)))))
      ==
    ::  ++s-n: parse sN integer as a @s
    ::
    ++  s-n
      |=  n-bits=@
      =*  this  $
      %+  knee  *@s
      |.  ~+
      ;~  pose
        ::  single byte: positive
        ::
        (cook (cury new:si &) (shim 0 (dec (bex (min (dec n-bits) 6)))))
        ::  single byte: negative
        ::
        %+  cook
          |=  n=@
          =,  si
          (dif (new & n) --128)
        ;~  simu
          (shim 64 127)
          (shim (sub 128 (min 128 (bex (dec n-bits)))) 127)
        ==
        ::  multiple bytes
        ::
        ?:  (lte n-bits 7)  fail
        %+  cook
          |=  [n=@s m=@s]
          =,  si
          (sum (dif n --128) (pro --128 m))
        ;~  plug
          (cook (cury new:si &) (shim 128 255))
          this(n-bits (sub n-bits 7))
        ==
      ==
    ::
    ++  u32  (u-n 32)
    ++  u64  (u-n 64)
    ++  f32
      %+  cook
        |=  =(list @)
        ;;  @rs
        (rep 3 list)
      (stun [4 4] next)
    ::
    ++  f64
      %+  cook
        |=  =(list @)
        ;;  @rd
        (rep 3 list)
      (stun [8 8] next)
    ::  ++vec: parse .wasm vector of rule
    ::
    ++  vec
      |*  rul=rule
      ;~  bild
        u32
        |=  n=@
        (stun [n n] rul)
      ==
    ::
    ++  name     (cook crip (vec next))
    ++  vec-u32  (vec u32)
    ++  num-type
      %+  cook  |=(num-type:sur +<)
      ;~  pose
        (cold %i32 (just '\7f'))
        (cold %i64 (just '\7e'))
        (cold %f32 (just '\7d'))
        (cold %f64 (just '\7c'))
      ==
    ::
    ++  vec-type  (cold %v128 (just '\7b'))
    ::
    ++  ref-type
      %+  cook  |=(ref-type:sur +<)
      ;~  pose
        (cold %extn (just '\6f'))
        (cold %func (just '\70'))
      ==
    ::
    ++  valtype
      %+  cook  |=(valtype:sur +<)
      ;~(pose num-type vec-type ref-type)
    ::
    ++  func-type
      %+  cook  |=(func-type:sur +<)
      ;~  pfix
        (just '\60')
        ;~(plug (vec valtype) (vec valtype))
      ==
    ::
    ++  limits
      %+  cook  |=(limits:sur +<)
      ;~  pose
        ;~(plug (cold %flor (just '\00')) u32)
        ;~(plug (cold %ceil (just '\01')) u32 u32)
      ==
    ::
    ::  Instruction and expression parsing rules
    ::
    ++  expr
      %+  knee  *expression:sur
      |.  ~+
      ;~(sfix (star instr) end)
    ::
    ++  expr-pair
      %+  knee  [*expression:sur *expression:sur]
      |.  ~+
      ;~  plug
        (star instr)
        ;~  pose
          (cold ~ end)
          (ifix [else end] (star instr))
        ==
      ==
    ::
    ++  end        (just '\0b')
    ++  else       (just '\05')
    ++  const-i32  (just '\41')
    ++  const-i64  (just '\42')
    ++  const-f32  (just '\43')
    ++  const-f64  (just '\44')
    ++  block-op   (just '\02')
    ++  loop-op    (just '\03')
    ++  if-op      (just '\04')
    ::
    ++  form-ranges
      |=  l=(list @)
      ^-  (list ?(@ [@ @]))
      ?~  l  ~
      =+  a=i.l
      =+  b=i.l
      |-  ^-  (list ?(@ [@ @]))
      ?~  t.l
        :_  ~
        ?:  =(a b)  a
        [a b]
      ?>  (gth i.t.l b)
      ?:  =(i.t.l +(b))
        $(b i.t.l, t.l t.t.l)
      :-  ?:  =(a b)  a
          [a b]
      $(a i.t.l, b i.t.l, t.l t.t.l)
    ::
    ++  instr
      ~+  %-  stew  ^.  stet
      ;:  welp
        ^.  limo
        :~
          ['\fc' ;~(pfix next fc)]
          ['\fd' ;~(pfix next fd)]
          ['\1c' select-vec]
          ['\0e' br-table]
          ['\02' block]
          ['\03' loop]
          ['\04' if]
          ['\d0' ;~(pfix next (stag %ref-null ref-type))]
          ['\d2' ;~(pfix next (stag %ref-func u32))]
        ==
      ::
        %-  turn  :_  (late instr-zero)
        %-  form-ranges
        %+  skim  (gulf 0 255)
        |=(n=@ ?=(^ (op-map n)))
      ::
        %+  turn
          %-  form-ranges
          %+  skim  (gulf 0 255)
          |=(n=@ ?=(^ (handle-one-arg-i32 n 0)))
        %-  late
        %+  sear  handle-one-arg-i32
        ;~(plug next u32)
      ::
        ^.  limo
        :~
          ['\41' (cook handle-const-i32 ;~(pfix next (s-n 32)))]
          ['\42' (cook handle-const-i64 ;~(pfix next (s-n 64)))]
          ['\43' (cook handle-const-f32 ;~(pfix next f32))]
          ['\44' (cook handle-const-f64 ;~(pfix next f64))]
        ==
      ::
        %+  turn
          %-  form-ranges
          %+  skim  (gulf 0 255)
          |=(n=@ ?=(^ (handle-two-args-i32 n 0 0)))
        %-  late
        %+  sear  handle-two-args-i32
        ;~(plug next u32 u32)
      ::
      ==
    ::
    ++  select-vec
      %+  cook  |=(instruction:sur +<)
      ;~  pfix
        next
        %+  stag  %select
        %+  stag  %~
        (vec valtype)
      ==
    ::
    ++  br-table
      %+  cook  handle-br-table
      ;~(pfix next ;~(plug vec-u32 u32))
    ::
    ++  instr-zero  (sear op-map next)
    ::
    ++  block
      %+  cook  handle-block
      ;~(pfix next ;~(plug block-type expr))
    ::
    ++  loop
      %+  cook  handle-loop
      ;~(pfix next ;~(plug block-type expr))
    ::
    ++  if
      %+  cook  handle-if
      ;~  pfix
        next
        ;~(plug block-type expr-pair)
      ==
    ::
    ++  block-type
      %+  cook  |=(block-type:sur +<)
      ;~  pose
        (cold [~ ~] (just '\40'))
        ;~(plug (easy ~) (sear get-valtype next) (easy ~))
      ::
        %+  sear
          |=  a=@s
          ^-  (unit @)
          ?.  (syn:si a)  ~
          `(abs:si a)
        (s-n 33)
      ::
      ==
    ::
    ::  All handle-X functions must return `instruction` type
    ::
    ::
    ++  handle-one-arg-i32
      |=  [op=char arg=@]
      ^-  (unit instruction:sur)
      ?+  op  ~
        %0xc   `[%br arg]
        %0xd   `[%br-if arg]
        %0x10  `[%call arg]
        %0x20  `[%local-get arg]
        %0x21  `[%local-set arg]
        %0x22  `[%local-tee arg]
        %0x23  `[%global-get arg]
        %0x24  `[%global-set arg]
        %0x25  `[%table-get arg]
        %0x26  `[%table-set arg]
        %0x3f  `[%memory-size ?>(=(arg 0) %0)]
        %0x40  `[%memory-grow ?>(=(arg 0) %0)]
      ==
    ::
    ::
    ++  handle-two-args-i32
      |=  [op=char arg1=@ arg2=@]
      ^-  (unit instruction:sur)
      ?+  op  ~
          %0x11
        `[%call-indirect arg1 arg2]
      ::
          %0x28
        `[%load %i32 [arg1 arg2] ~ ~]
      ::
          %0x29
        `[%load %i64 [arg1 arg2] ~ ~]
      ::
          %0x2a
        `[%load %f32 [arg1 arg2] ~ ~]
      ::
          %0x2b
        `[%load %f64 [arg1 arg2] ~ ~]
      ::
          %0x2c
        `[%load %i32 [arg1 arg2] `%8 `%s]
      ::
          %0x2d
        `[%load %i32 [arg1 arg2] `%8 `%u]
      ::
          %0x2e
        `[%load %i32 [arg1 arg2] `%16 `%s]
      ::
          %0x2f
        `[%load %i32 [arg1 arg2] `%16 `%u]
      ::
          %0x30
        `[%load %i64 [arg1 arg2] `%8 `%s]
      ::
          %0x31
        `[%load %i64 [arg1 arg2] `%8 `%u]
      ::
          %0x32
        `[%load %i64 [arg1 arg2] `%16 `%s]
      ::
          %0x33
        `[%load %i64 [arg1 arg2] `%16 `%u]
      ::
          %0x34
        `[%load %i64 [arg1 arg2] `%32 `%s]
      ::
          %0x35
        `[%load %i64 [arg1 arg2] `%32 `%u]
      ::
          %0x36
        `[%store %i32 [arg1 arg2] ~]
      ::
          %0x37
        `[%store %i64 [arg1 arg2] ~]
      ::
          %0x38
        `[%store %f32 [arg1 arg2] ~]
      ::
          %0x39
        `[%store %f64 [arg1 arg2] ~]
      ::
          %0x3a
        `[%store %i32 [arg1 arg2] `%8]
      ::
          %0x3b
        `[%store %i32 [arg1 arg2] `%16]
      ::
          %0x3c
        `[%store %i64 [arg1 arg2] `%8]
      ::
          %0x3d
        `[%store %i64 [arg1 arg2] `%16]
      ::
          %0x3e
        `[%store %i64 [arg1 arg2] `%32]
      ==
    ::
    ++  handle-br-table
      |=  [vec=(list @) i=@]
      ^-  instruction:sur
      [%br-table vec i]
    ::
    ++  handle-block
      |=  [type=block-type:sur body=expression:sur]
      ^-  $>(%block instruction:sur)
      [%block type body]
    ::
    ++  get-valtype
      |=  byte=@
      ^-  (unit valtype:sur)
      ?+  byte  ~
        %0x7f  `%i32
        %0x7e  `%i64
        %0x7d  `%f32
        %0x7c  `%f64
      ==
    ++  handle-loop
      |=  [type=block-type:sur body=expression:sur]
      ^-  instruction:sur
      [%loop type body]
    ::
    ++  handle-if
      |=  $:  type=block-type:sur
              body-true=expression:sur
              body-false=expression:sur
          ==
      ^-  instruction:sur
      [%if type body-true body-false]
    ::
    ++  handle-const-f64
      |=  i=@rd
      ^-  instruction:sur
      [%const %f64 i]
    ::
    ++  handle-const-f32
      |=  i=@rs
      ^-  instruction:sur
      [%const %f32 i]
    ::
    ++  handle-const-i32
      |=  i=@s
      ^-  instruction:sur
      =;  i-unsigned=@
        [%const %i32 i-unsigned]
      =,  si
      ?:  (syn i)
        +:(old i)
      (sub (bex 32) +:(old i))
    ::
    ++  handle-const-i64
      |=  i=@s
      ^-  instruction:sur
      =;  i-unsigned=@
        [%const %i64 i-unsigned]
      =,  si
      ?:  (syn i)
        +:(old i)
      (sub (bex 64) +:(old i))
    ::  ++fc: 0xFC extension parser
    ::
    ++  fc
      |^
      %+  cook  |=(instruction:sur +<)
      ;~  pose
        zero-args
        one-arg
        two-args
      ==
      ::
      ++  zero-args  (sear handle-zero u32)
      ++  one-arg    (sear handle-one ;~(plug u32 u32))
      ++  two-args   (sear handle-two ;~(plug u32 u32 u32))
      ++  handle-zero
        |=  op=@
        ^-  (unit instruction:sur)
        ?.  (lte op 7)  ~
        :-  ~
        :*
          %trunc
        ::  Type
        ::
          ?:((lte op 3) %i32 %i64)
        ::  Source type
        ::
          `?:(=(0 (mod (div op 2) 2)) %f32 %f64)
        ::  Mode
        ::
          `?:(=(0 (mod op 2)) %s %u)
        ::
          &  ::  saturated
        ==
      ::
      ++  handle-one
        |=  [op=@ arg=@]
        ^-  (unit instruction:sur)
        ?+  op  ~
          %9   `[%data-drop arg]
          %11  `[%memory-fill ?>(?=(%0 arg) arg)]
          %13  `[%elem-drop arg]
          %15  `[%table-grow arg]
          %16  `[%table-size arg]
          %17  `[%table-fill arg]
        ==
      ::
      ++  handle-two
        |=  [op=@ arg1=@ arg2=@]
        ^-  (unit instruction:sur)
        ?+  op  ~
          %8   `[%memory-init arg1 ?>(?=(%0 arg2) arg2)]
          %10  `[%memory-copy ?>(?=(%0 arg1) arg1) ?>(?=(%0 arg2) arg2)]
          %12  `[%table-init arg1 arg2]
          %14  `[%table-copy arg1 arg2]
        ==
      ::
      --
    ::  ++fd: 0xFD extension parser
    ::
    ++  fd
      |^
      ::  Opcode and immediate parameters
      ::
      ;~  pose
        (sear memarg ;~(plug u32 u32 u32))
        (sear mem-lane ;~(plug u32 ;~(plug u32 u32) next))
        (cook const ;~(pfix (feel 12 u32) (stun [16 16] next)))
        (cook shuffle ;~(pfix (feel 13 u32) (stun [16 16] next)))
        (sear lane ;~(plug u32 next))
        (sear simd-map u32)
      ==
      ::
      ++  memarg
        |=  [op=@ mem=[@ @]]
        ^-  (unit instruction:sur)
        =;  =(unit instr-vec:sur)
          ?~  unit  ~
          `[%vec u.unit]
        ?+  op  ~
          %0   `[%load mem ~]
          %1   `[%load mem ~ %8 %extend %s]
          %2   `[%load mem ~ %8 %extend %u]
          %3   `[%load mem ~ %16 %extend %s]
          %4   `[%load mem ~ %16 %extend %u]
          %5   `[%load mem ~ %32 %extend %s]
          %6   `[%load mem ~ %32 %extend %u]
          %7   `[%load mem ~ %8 %splat]
          %8   `[%load mem ~ %16 %splat]
          %9   `[%load mem ~ %32 %splat]
          %10  `[%load mem ~ %64 %splat]
          %92  `[%load mem ~ %32 %zero]
          %93  `[%load mem ~ %64 %zero]
          %11  `[%store mem]
        ==
      ::
      ++  mem-lane
        |=  [op=@ mem=[@ @] l=@]
        ^-  (unit instruction:sur)
        =;  =(unit instr-vec:sur)
          ?~  unit  ~
          `[%vec u.unit]
        ?+  op  ~
          %84  `[%load-lane mem %8 l]
          %85  `[%load-lane mem %16 l]
          %86  `[%load-lane mem %32 l]
          %87  `[%load-lane mem %64 l]
          %88  `[%store-lane mem %8 l]
          %89  `[%store-lane mem %16 l]
          %90  `[%store-lane mem %32 l]
          %91  `[%store-lane mem %64 l]
        ==
      ::
      ++  const
        |=  =(list @)
        ^-  instruction:sur
        :^  %vec  %const  %v128
        (rep 3 list)
      ::
      ++  shuffle
        |=  =(list @)
        ^-  instruction:sur
        [%vec %shuffle list]
      ::
      ++  lane
        |=  [op=@ l=@]
        ^-  (unit instruction:sur)
        =;  =(unit instr-vec:sur)
          ?~  unit  ~
          `[%vec u.unit]
        ?+  op  ~
          %21  `[%extract %i8 l %s]
          %22  `[%extract %i8 l %u]
          %23  `[%replace %i8 l]
          %24  `[%extract %i16 l %s]
          %25  `[%extract %i16 l %u]
          %26  `[%replace %i16 l]
          %27  `[%extract %i32 l %u]
          %28  `[%replace %i32 l]
          %29  `[%extract %i64 l %u]
          %30  `[%replace %i64 l]
          %31  `[%extract %f32 l %u]
          %32  `[%replace %f32 l]
          %33  `[%extract %f64 l %u]
          %34  `[%replace %f64 l]
        ==
      --
    ::
    ::  Section rules
    ::
    ::  Type section
    ::
    ++  type-section
      %+  cook  |=(type-section:sur +<)
      (vec func-type)
    ::  Import section
    ::
    ++  import-section
      %+  cook  |=(import-section:sur +<)
      (vec import)
    ::
    ++  import
      %+  cook  |=(import:sur +<)
      ;~  plug
        name
        name
        import-desc
      ==
    ::
    ++  import-desc
      ;~  pose
        import-func
        import-tabl
        import-memo
        import-glob
      ==
    ::
    ++  import-func  ;~(plug (cold %func (just '\00')) u32)
    ++  import-tabl  ;~(plug (cold %tabl (just '\01')) ref-type limits)
    ++  import-memo  ;~(plug (cold %memo (just '\02')) limits)
    ++  import-glob
      ;~  plug
        (cold %glob (just '\03'))
        valtype
        con-var
      ==
    ::
    ++  con-var
      ;~  pose
        (cold %con (just '\00'))
        (cold %var (just '\01'))
      ==
    ::  Function section
    ::
    ++  function-section
      %+  cook  |=(function-section:sur +<)
      (vec u32)
    ::  Table section
    ::
    ++  table-section
      %+  cook  |=(table-section:sur +<)
      (vec table)
    ::
    ++  table  ;~(plug ref-type limits)
    ::  Memory section
    ::
    ++  memory-section
      %+  cook  |=(memory-section:sur +<)
      (vec limits)
    ::  Global section
    ::
    ++  global-section
      %+  cook  |=(global-section:sur +<)
      (vec global)
    ::
    ++  global
      %+  cook  |=(global:sur +<)
      ;~  plug
        valtype
        con-var
        const-expr
      ==
    ::
    ++  const-expr  ;~(sfix const-instr end)  ::  single instruction
    ++  const-instr
    %+  sear
      |=  i=instruction:sur
      ^-  (unit const-instr:sur)
      ?.  ?=(const-instr:sur i)
        ~
      `i
    instr
    ::  Export section
    ::
    ++  export-section
      %+  cook  export-section:sur
      (vec export)
    ::
    ++  export
      ;~  plug
        name
        %+  cook  |=(export-desc:sur +<)
        ;~  pose
          ;~(plug (cold %func (just '\00')) u32)
          ;~(plug (cold %tabl (just '\01')) u32)
          ;~(plug (cold %memo (just '\02')) u32)
          ;~(plug (cold %glob (just '\03')) u32)
        ==
      ==
    ::  Start section
    ::
    ++  start-section
      %+  cook  |=(start-section:sur +<)
      (stag ~ u32)
    ::  Element section
    ::
    ++  elem-section
      %+  cook  |=(elem-section:sur +<)
      (vec elem)
    ::
    ++  elem
      ;~  pose
        elem-0
        elem-1
        elem-2
        elem-3
        elem-4
        elem-5
        elem-6
        elem-7
      ==
    ::
    ++  elem-kind  (just '\00')
    ++  elem-0
      %+  cook  handle-elem-0
      ;~  pfix  (just '\00')
        ;~(plug const-expr (vec u32))
      ==
    ::
    ++  elem-1
      %+  cook  handle-elem-1
      ;~  pfix  (just '\01')
        ;~(plug elem-kind (vec u32))
      ==
    ::
    ++  elem-2
      %+  cook  handle-elem-2
      ;~  pfix  (just '\02')
        ;~(plug u32 const-expr elem-kind (vec u32))
      ==
    ::
    ++  elem-3
    %+  cook  handle-elem-3
    ;~  pfix  (just '\03')
      ;~(plug elem-kind (vec u32))
    ==
    ::
    ++  elem-4
      %+  cook  handle-elem-4
      ;~  pfix  (just '\04')
        ;~(plug const-expr (vec expr))
      ==
    ::
    ++  elem-5
      %+  cook  handle-elem-5
      ;~  pfix  (just '\05')
        ;~(plug ref-type (vec expr))
      ==
    ::
    ++  elem-6
      %+  cook  handle-elem-6
      ;~  pfix  (just '\06')
        ;~(plug u32 const-expr ref-type (vec expr))
      ==
    ::
    ++  elem-7
      %+  cook  handle-elem-7
      ;~  pfix  (just '\07')
        ;~(plug ref-type (vec expr))
      ==
    ::
    ++  handle-elem-0
      |=  [e=const-instr:sur y=(list @)]
      ^-  elem:sur
      :+  %func
        %+  turn  y
        |=  y=@
        ^-  $>(?(%ref-func %ref-null) instruction:sur)
        [%ref-func y]
      [%acti 0 e]
    ::
    ++  handle-elem-1
      |=  [et=@ y=(list @)]
      ^-  elem:sur
      :+  ?+  et  ~|(%unrecognized-elem-kind !!)
            %0x0  %func
          ==
        %+  turn  y
        |=  y=@
        ^-  $>(?(%ref-func %ref-null) instruction:sur)
        [%ref-func y]
      [%pass ~]
    ::
    ++  handle-elem-2
      |=  [x=@ e=const-instr:sur et=@ y=(list @)]
      ^-  elem:sur
      :+  ?+  et  ~|(%unrecognized-elem-kind !!)
            %0x0  %func
          ==
        %+  turn  y
        |=  y=@
        ^-  $>(?(%ref-func %ref-null) instruction:sur)
        [%ref-func y]
      [%acti x e]
    ::
    ++  handle-elem-3
      |=  [et=@ y=(list @)]
      ^-  elem:sur
      :+  ?+  et  ~|(%unrecognized-elem-kind !!)
            %0x0  %func
          ==
        %+  turn  y
        |=  y=@
        ^-  $>(?(%ref-func %ref-null) instruction:sur)
        [%ref-func y]
      [%decl ~]
    ::
    ++  handle-elem-4
      |=  [e=const-instr:sur el=(list expression:sur)]
      ^-  elem:sur
      :+  %func
        %+  turn  el
        |=  ex=expression:sur
        ^-  $>(?(%ref-func %ref-null) instruction:sur)
        ?>  ?=([[%ref-func *] ~] ex)
        i.ex
      [%acti 0 e]
    ::
    ++  handle-elem-5
      |=  [et=ref-type:sur el=(list expression:sur)]
      ^-  elem:sur
      :+  et
        %+  turn  el
        |=  ex=expression:sur
        ^-  $>(?(%ref-func %ref-null) instruction:sur)
        ?>  ?=([[%ref-func *] ~] ex)
        i.ex
      [%pass ~]
    ::
    ++  handle-elem-6
      |=  [x=@ e=const-instr:sur et=ref-type:sur el=(list expression:sur)]
      ^-  elem:sur
      :+  et
        %+  turn  el
        |=  ex=expression:sur
        ^-  $>(?(%ref-func %ref-null) instruction:sur)
        ?>  ?=([[%ref-func *] ~] ex)
        i.ex
      [%acti x e]
    ::
    ++  handle-elem-7
      |=  [et=ref-type:sur el=(list expression:sur)]
      ^-  elem:sur
      :+  et
        %+  turn  el
        |=  ex=expression:sur
        ^-  $>(?(%ref-func %ref-null) instruction:sur)
        ?>  ?=([[%ref-func *] ~] ex)
        i.ex
      [%decl ~]
    ::  Code section
    ::
    ++  code-section
      %+  cook  |=(code-section:sur +<)
      (vec code)
    ::
    ++  code  (bonk (vec next) ;~(pfix u32 func))
    ++  func
      ;~  plug
        (cook handle-locals (vec locals))
        expr
      ==
    ::
    ++  locals  ;~(plug u32 valtype)
    ::  ++handle-locals: concatenate locals declaration
    ::
    ++  handle-locals
      |=  l=(list [n=@ v=valtype:sur])
      ^-  (list valtype:sur)
      ?~  l  ~
      %+  weld  (reap n.i.l v.i.l)
      $(l t.l)
    ::  Data section
    ::
    ++  data-section
      %+  cook  |=(data-section:sur +<)
      (vec data)
    ::
    ++  data
      %+  cook  |=(data:sur +<)
      ;~    pose
          ;~  plug
            (cold %acti (just '\00'))
            const-expr
            (cook to-octs (vec next))
          ==
      ::
          ;~  plug
            (cold %pass (just '\01'))
            (cook to-octs (vec next))
          ==
      ::
          ;~    plug
              (cold %acti (just '\02'))
              ;~    pfix
                  u32
                  ;~  plug
                    const-expr
                    (cook to-octs (vec next))
      ==  ==  ==  ==
    ::
    ++  to-octs
      |=  =tape
      ^-  octs
      :-  (lent tape)
      (rep 3 tape)
    ::
    ++  datacnt-section
      %+  cook  |=(datacnt-section:sur +<)
      (stag ~ u32)
    ::
    ++  module
      %+  cook  |=(module:sur +<)
      ;~  pfix
        magic
        version
        (ifix [customs customs] module-contents)
      ==
    ::
    ++  module-contents
      ;~  (glue customs)
        (check 1 type-section *type-section:sur)
        (check 2 import-section *import-section:sur)
        (check 3 function-section *function-section:sur)
        (check 4 table-section *table-section:sur)
        (check 5 memory-section *memory-section:sur)
        (check 6 global-section *global-section:sur)
        (check 7 export-section *export-section:sur)
        (check 8 start-section *start-section:sur)
        (check 9 elem-section *elem-section:sur)
        (check 12 datacnt-section *datacnt-section:sur)
        (check 10 code-section *code-section:sur)
        (check 11 data-section *data-section:sur)
      ==
    ::
    ++  check
      |*  [id=@ sec=rule def=*]
      ;~  pose
      ::  section present
      ::
        ;~  pfix
          (just `@`id)
          (bonk (vec next) ;~(pfix u32 sec))
        ==
      ::  section missing
      ::
        (easy `(womp sec)`def)
      ==
    ::
    ++  customs
      %-  star
      ;~  plug
        (just '\00')
        (vec next)
      ==
    ::
    ++  magic  (jest '\00asm')
    ++  version
      ;~  plug
        (just '\01')
        (just '\00')  ::  leading zeros shenanigans
        (just '\00')
        (just '\00')
      ==
    ::
    --  :: |r
  ::
  ++  op-map
    |=  op=char
    ~+
    ^-  (unit instruction:sur)
    ?+  op  ~
      %0x0   `[%unreachable ~]
      %0x1   `[%nop ~]
      %0xf   `[%return ~]
      %0x1a  `[%drop ~]
      %0x1b  `[%select ~]
      %0xa7  `[%wrap ~]
      %0xb6  `[%demote ~]
      %0xbb  `[%promote ~]
      %0xd1  `[%ref-is-null ~]
    ::
        eqz-opcodes:sur
      :-  ~
      :-  %eqz
      ?-  op
        %0x45  %i32
        %0x50  %i64
      ==
    ::
        eq-opcodes:sur
      :-  ~
      :-  %eq
      ?-  op
        %0x46  %i32
        %0x51  %i64
        %0x5b  %f32
        %0x61  %f64
      ==
    ::
        ne-opcodes:sur
      :-  ~
      :-  %ne
      ?-  op
        %0x47  %i32
        %0x52  %i64
        %0x5c  %f32
        %0x62  %f64
      ==
    ::
        lt-opcodes:sur
      :-  ~
      :-  %lt
      ?-  op
        %0x48  [%i32 `%s]
        %0x49  [%i32 `%u]
        %0x53  [%i64 `%s]
        %0x54  [%i64 `%u]
        %0x5d  [%f32 ~]
        %0x63  [%f64 ~]
      ==
    ::
        gt-opcodes:sur
      :-  ~
      :-  %gt
      ?-  op
        %0x4a  [%i32 `%s]
        %0x4b  [%i32 `%u]
        %0x55  [%i64 `%s]
        %0x56  [%i64 `%u]
        %0x5e  [%f32 ~]
        %0x64  [%f64 ~]
      ==
    ::
        le-opcodes:sur
      :-  ~
      :-  %le
      ?-  op
        %0x4c  [%i32 `%s]
        %0x4d  [%i32 `%u]
        %0x57  [%i64 `%s]
        %0x58  [%i64 `%u]
        %0x5f  [%f32 ~]
        %0x65  [%f64 ~]
      ==
    ::
        ge-opcodes:sur
      :-  ~
      :-  %ge
      ?-  op
        %0x4e  [%i32 `%s]
        %0x4f  [%i32 `%u]
        %0x59  [%i64 `%s]
        %0x5a  [%i64 `%u]
        %0x60  [%f32 ~]
        %0x66  [%f64 ~]
      ==
    ::
        clz-opcodes:sur
      :-  ~
      :-  %clz
      ?-  op
        %0x67  %i32
        %0x79  %i64
      ==
    ::
        ctz-opcodes:sur
      :-  ~
      :-  %ctz
      ?-  op
        %0x68  %i32
        %0x7a  %i64
      ==
    ::
        popcnt-opcodes:sur
      :-  ~
      :-  %popcnt
      ?-  op
        %0x69  %i32
        %0x7b  %i64
      ==
    ::
        add-opcodes:sur
      :-  ~
      :-  %add
      ?-  op
        %0x6a  %i32
        %0x7c  %i64
        %0x92  %f32
        %0xa0  %f64
      ==
    ::
        sub-opcodes:sur
      :-  ~
      :-  %sub
      ?-  op
        %0x6b  %i32
        %0x7d  %i64
        %0x93  %f32
        %0xa1  %f64
      ==
    ::
        mul-opcodes:sur
      :-  ~
      :-  %mul
      ?-  op
        %0x6c  %i32
        %0x7e  %i64
        %0x94  %f32
        %0xa2  %f64
      ==
    ::
        div-opcodes:sur
      :-  ~
      :-  %div
      ?-  op
        %0x6d  [%i32 `%s]
        %0x6e  [%i32 `%u]
        %0x7f  [%i64 `%s]
        %0x80  [%i64 `%u]
        %0x95  [%f32 ~]
        %0xa3  [%f64 ~]
      ==
    ::
        rem-opcodes:sur
      :-  ~
      :-  %rem
      ?-  op
        %0x6f  [%i32 %s]
        %0x70  [%i32 %u]
        %0x81  [%i64 %s]
        %0x82  [%i64 %u]
      ==
    ::
        and-opcodes:sur
      :-  ~
      :-  %and
      ?-  op
        %0x71  %i32
        %0x83  %i64
      ==
    ::
        or-opcodes:sur
      :-  ~
      :-  %or
      ?-  op
        %0x72  %i32
        %0x84  %i64
      ==
    ::
        xor-opcodes:sur
      :-  ~
      :-  %xor
      ?-  op
        %0x73  %i32
        %0x85  %i64
      ==
    ::
        shl-opcodes:sur
      :-  ~
      :-  %shl
      ?-  op
        %0x74  %i32
        %0x86  %i64
      ==
    ::
        shr-opcodes:sur
      :-  ~
      :-  %shr
      ?-  op
        %0x75  [%i32 %s]
        %0x76  [%i32 %u]
        %0x87  [%i64 %s]
        %0x88  [%i64 %u]
      ==
    ::
        rotl-opcodes:sur
      :-  ~
      :-  %rotl
      ?-  op
        %0x77  %i32
        %0x89  %i64
      ==
    ::
        rotr-opcodes:sur
      :-  ~
      :-  %rotr
      ?-  op
        %0x78  %i32
        %0x8a  %i64
      ==
    ::
        abs-opcodes:sur
      :-  ~
      :-  %abs
      ?-  op
        %0x8b  %f32
        %0x99  %f64
      ==
    ::
        neg-opcodes:sur
      :-  ~
      :-  %neg
      ?-  op
        %0x8c  %f32
        %0x9a  %f64
      ==
    ::
        ceil-opcodes:sur
      :-  ~
      :-  %ceil
      ?-  op
        %0x8d  %f32
        %0x9b  %f64
      ==
    ::
        floor-opcodes:sur
      :-  ~
      :-  %floor
      ?-  op
        %0x8e  %f32
        %0x9c  %f64
      ==
    ::
        trunc-opcodes:sur
      :-  ~
      :-  %trunc
      ?-  op
        %0x8f  [%f32 ~ ~ |]
        %0x9d  [%f64 ~ ~ |]
        %0xa8  [%i32 `%f32 `%s |]
        %0xa9  [%i32 `%f32 `%u |]
        %0xaa  [%i32 `%f64 `%s |]
        %0xab  [%i32 `%f64 `%u |]
        %0xae  [%i64 `%f32 `%s |]
        %0xaf  [%i64 `%f32 `%u |]
        %0xb0  [%i64 `%f64 `%s |]
        %0xb1  [%i64 `%f64 `%u |]
      ==
    ::
        nearest-opcodes:sur
      :-  ~
      :-  %nearest
      ?-  op
        %0x90  %f32
        %0x9e  %f64
      ==
    ::
        sqrt-opcodes:sur
      :-  ~
      :-  %sqrt
      ?-  op
        %0x91  %f32
        %0x9f  %f64
      ==
    ::
        min-opcodes:sur
      :-  ~
      :-  %min
      ?-  op
        %0x96  %f32
        %0xa4  %f64
      ==
    ::
        max-opcodes:sur
      :-  ~
      :-  %max
      ?-  op
        %0x97  %f32
        %0xa5  %f64
      ==
    ::
        copysign-opcodes:sur
      :-  ~
      :-  %copysign
      ?-  op
        %0x98  %f32
        %0xa6  %f64
      ==
    ::
        extend-opcodes:sur
      :-  ~
      :-  %extend
      ?-  op
        %0xac  [%i64 %i32 %32 %s]
        %0xad  [%i64 %i32 %32 %u]
        %0xc0  [%i32 %i32 %8 %s]
        %0xc1  [%i32 %i32 %16 %s]
        %0xc2  [%i64 %i64 %8 %s]
        %0xc3  [%i64 %i64 %16 %s]
        %0xc4  [%i64 %i64 %32 %s]
      ==
    ::
        convert-opcodes:sur
      :-  ~
      :-  %convert
      ?-  op
        %0xb2  [%f32 %i32 %s]
        %0xb3  [%f32 %i32 %u]
        %0xb4  [%f32 %i64 %s]
        %0xb5  [%f32 %i64 %u]
        %0xb7  [%f64 %i32 %s]
        %0xb8  [%f64 %i32 %u]
        %0xb9  [%f64 %i64 %s]
        %0xba  [%f64 %i64 %u]
      ==
    ::
        reinterpret-opcodes:sur
      :-  ~
      :-  %reinterpret
      ?-  op
        %0xbc  [%i32 %f32]
        %0xbd  [%i64 %f64]
        %0xbe  [%f32 %i32]
        %0xbf  [%f64 %i64]
      ==
    ::
    ==  ::  |op-map
  ::
  ++  simd-map
    |=  op=@
    ~+
    |^
    ^-  (unit instruction:sur)
    =;  =(unit instr-vec:sur)
      ?~  unit  ~
      `[%vec u.unit]
    ?+  op    ~
      %14   `[%swizzle ~]
      %98   `[%popcnt ~]
      %77   `[%not ~]
      %78   `[%and ~]
      %79   `[%andnot ~]
      %80   `[%or ~]
      %81   `[%xor ~]
      %82   `[%bitselect ~]
      %83   `[%any-true ~]
      %130  `[%q15mul-r-sat ~]
      %186  `[%dot ~]
      %94   `[%demote ~]
      %95   `[%promote ~]
    ::
      ?(%106 %148)  `(nearest op)
      ?(%227 %239)  `(sqrt op)
      ?(%231 %243)  `(div op)
      ?(%234 %246)  `(pmin op)
      ?(%235 %247)  `(pmax op)
      ?(%123 %155)  `(avgr op)
      ?(%103 %116)  `(ceil op)
      ?(%104 %117)  `(floor op)
    ::
      ?(%99 %131 %163 %195)   `(all-true op)
      ?(%100 %132 %164 %196)  `(bitmask op)
      ?(%101 %102 %133 %134)  `(narrow op)
      ?(%107 %139 %171 %203)  `(shl op)
      ?(%124 %125 %126 %127)  `(extadd op)
      ?(%250 %251 %254 %255)  `(convert op)
    ::
      ?(%149 %181 %213 %230 %242)       `(mul op)
      ?(%15 %16 %17 %18 %19 %20)        `(splat op)
      ?(%35 %45 %55 %214 %65 %71)       `(eq op)
      ?(%36 %46 %56 %215 %66 %72)       `(ne op)
      ?(%96 %128 %160 %192 %224 %236)   `(abs op)
      ?(%97 %129 %161 %193 %225 %237)   `(neg op)
      ?(%105 %122 %248 %249 %252 %253)  `(trunc op)
    ::
      ?(%37 %38 %47 %48 %57 %58 %216 %67 %73)  `(lt op)
      ?(%39 %40 %49 %50 %59 %60 %217 %68 %74)  `(gt op)
      ?(%41 %42 %51 %52 %61 %62 %218 %69 %75)  `(le op)
      ?(%43 %44 %53 %54 %63 %64 %219 %70 %76)  `(ge op)
    ::
      ?(%108 %109 %140 %141 %172 %173 %204 %205)  `(shr op)
      ?(%118 %119 %150 %151 %182 %183 %232 %244)  `(min op)
      ?(%120 %121 %152 %153 %184 %185 %233 %245)  `(max op)
    ::
      ?(%110 %111 %112 %142 %143 %144 %174 %206 %228 %240)  `(add op)
      ?(%113 %114 %115 %145 %146 %147 %177 %209 %229 %241)  `(sub op)
    ::
      ?(%135 %136 %137 %138 %167 %168 %169 %170 %199 %200 %201 %202)  `(extend op)
      ?(%156 %157 %158 %159 %188 %189 %190 %191 %220 %221 %222 %223)  `(extmul op)
    ==
    ::
    ++  nearest
      |=  op=?(%106 %148)
      ^-  instr-vec:sur
      :-  %nearest
      ?-  op
        %106  %f32
        %148  %f64
      ==
    ::
    ++  sqrt
      |=  op=?(%227 %239)
      ^-  instr-vec:sur
      :-  %sqrt
      ?-  op
        %227  %f32
        %239  %f64
      ==
    ::
    ++  div
      |=  op=?(%231 %243)
      ^-  instr-vec:sur
      :-  %div
      ?-  op
        %231  %f32
        %243  %f64
      ==
    ::
    ++  pmin
      |=  op=?(%234 %246)
      ^-  instr-vec:sur
      :-  %pmin
      ?-  op
        %234  %f32
        %246  %f64
      ==
    ::
    ++  pmax
      |=  op=?(%235 %247)
      ^-  instr-vec:sur
      :-  %pmax
      ?-  op
        %235  %f32
        %247  %f64
      ==
    ::
    ++  avgr
      |=  op=?(%123 %155)
      ^-  instr-vec:sur
      :-  %avgr
      :_  %u
      ?-  op
        %123  %i8
        %155  %i16
      ==
    ::
    ++  ceil
      |=  op=?(%103 %116)
      ^-  instr-vec:sur
      :-  %ceil
      ?-  op
        %103  %f32
        %116  %f64
      ==
    ::
    ++  floor
      |=  op=?(%104 %117)
      ^-  instr-vec:sur
      :-  %floor
      ?-  op
        %104  %f32
        %117  %f64
      ==
    ::
    ++  all-true
      |=  op=?(%99 %131 %163 %195)
      ^-  instr-vec:sur
      :-  %all-true
      ?-  op
        %99   %i8
        %131  %i16
        %163  %i32
        %195  %i64
      ==
    ::
    ++  bitmask
      |=  op=?(%100 %132 %164 %196)
      ^-  instr-vec:sur
      :-  %bitmask
      ?-  op
        %100  %i8
        %132  %i16
        %164  %i32
        %196  %i64
      ==
    ::
    ++  narrow
      |=  op=?(%101 %102 %133 %134)
      ^-  instr-vec:sur
      :-  %narrow
      ?-  op
        %101  [%i8 %s]
        %102  [%i8 %u]
        %133  [%i16 %s]
        %134  [%i16 %u]
      ==
    ::
    ++  shl
      |=  op=?(%107 %139 %171 %203)
      ^-  instr-vec:sur
      :-  %shl
      ?-  op
        %107  %i8
        %139  %i16
        %171  %i32
        %203  %i64
      ==
    ::
    ++  extadd
      |=  op=?(%124 %125 %126 %127)
      ^-  instr-vec:sur
      :-  %extadd
      ?-  op
        %124  [%i16 %s]
        %125  [%i16 %u]
        %126  [%i32 %s]
        %127  [%i32 %u]
      ==
    ::
    ++  convert
      |=  op=?(%250 %251 %254 %255)
      ^-  instr-vec:sur
      :-  %convert
      ?-  op
        %250  [%f32 %s]
        %251  [%f32 %u]
        %254  [%f64 %s]
        %255  [%f64 %u]
      ==
    ::
    ++  mul
      |=  op=?(%149 %181 %213 %230 %242)
      ^-  instr-vec:sur
      :-  %mul
      ?-  op
        %149  %i16
        %181  %i32
        %213  %i64
        %230  %f32
        %242  %f64
      ==
    ::
    ++  splat
      |=  op=?(%15 %16 %17 %18 %19 %20)
      ^-  instr-vec:sur
      :-  %splat
      ?-  op
        %15  %i8
        %16  %i16
        %17  %i32
        %18  %i64
        %19  %f32
        %20  %f64
      ==
    ::
    ++  eq
      |=  op=?(%35 %45 %55 %214 %65 %71)
      ^-  instr-vec:sur
      :-  %eq
      ?-  op
        %35   %i8
        %45   %i16
        %55   %i32
        %214  %i64
        %65   %f32
        %71   %f64
      ==
    ::
    ++  ne
      |=  op=?(%36 %46 %56 %215 %66 %72)
      ^-  instr-vec:sur
      :-  %ne
      ?-  op
        %36   %i8
        %46   %i16
        %56   %i32
        %215  %i64
        %66   %f32
        %72   %f64
      ==
    ::
    ++  abs
      |=  op=?(%96 %128 %160 %192 %224 %236)
      ^-  instr-vec:sur
      :-  %abs
      ?-  op
        %96   %i8
        %128  %i16
        %160  %i32
        %192  %i64
        %224  %f32
        %236  %f64
      ==
    ::
    ++  neg
      |=  op=?(%97 %129 %161 %193 %225 %237)
      ^-  instr-vec:sur
      :-  %neg
      ?-  op
        %97   %i8
        %129  %i16
        %161  %i32
        %193  %i64
        %225  %f32
        %237  %f64
      ==
    ::
    ++  trunc
      |=  op=?(%105 %122 %248 %249 %252 %253)
      ^-  instr-vec:sur
      :-  %trunc
      ?-  op
        %105  [%f32 %f32 %s]
        %122  [%f64 %f64 %s]
        %248  [%i32 %f32 %s]
        %249  [%i32 %f32 %u]
        %252  [%i32 %f64 %s]
        %253  [%i32 %f64 %u]
      ==
    ::
    ++  lt
      |=  op=?(%37 %38 %47 %48 %57 %58 %216 %67 %73)
      ^-  instr-vec:sur
      :-  %lt
      ?-  op
        %37   [%i8 %s]
        %38   [%i8 %u]
        %47   [%i16 %s]
        %48   [%i16 %u]
        %57   [%i32 %s]
        %58   [%i32 %u]
        %216  [%i64 %s]
        %67   [%f32 %s]
        %73   [%f64 %s]
      ==
    ::
    ++  gt
      |=  op=?(%39 %40 %49 %50 %59 %60 %217 %68 %74)
      ^-  instr-vec:sur
      :-  %gt
      ?-  op
        %39   [%i8 %s]
        %40   [%i8 %u]
        %49   [%i16 %s]
        %50   [%i16 %u]
        %59   [%i32 %s]
        %60   [%i32 %u]
        %217  [%i64 %s]
        %68   [%f32 %s]
        %74   [%f64 %s]
      ==
    ::
    ++  le
      |=  op=?(%41 %42 %51 %52 %61 %62 %218 %69 %75)
      ^-  instr-vec:sur
      :-  %le
      ?-  op
        %41   [%i8 %s]
        %42   [%i8 %u]
        %51   [%i16 %s]
        %52   [%i16 %u]
        %61   [%i32 %s]
        %62   [%i32 %u]
        %218  [%i64 %s]
        %69   [%f32 %s]
        %75   [%f64 %s]
      ==
    ::
    ++  ge
      |=  op=?(%43 %44 %53 %54 %63 %64 %219 %70 %76)
      ^-  instr-vec:sur
      :-  %ge
      ?-  op
        %43   [%i8 %s]
        %44   [%i8 %u]
        %53   [%i16 %s]
        %54   [%i16 %u]
        %63   [%i32 %s]
        %64   [%i32 %u]
        %219  [%i64 %s]
        %70   [%f32 %s]
        %76   [%f64 %s]
      ==
    ::
    ++  shr
      |=  op=?(%108 %109 %140 %141 %172 %173 %204 %205)
      ^-  instr-vec:sur
      :-  %shr
      ?-  op
        %108  [%i8 %s]
        %109  [%i8 %u]
        %140  [%i16 %s]
        %141  [%i16 %u]
        %172  [%i32 %s]
        %173  [%i32 %u]
        %204  [%i64 %s]
        %205  [%i64 %u]
      ==
    ::
    ++  min
      |=  op=?(%118 %119 %150 %151 %182 %183 %232 %244)
      ^-  instr-vec:sur
      :-  %min
      ?-  op
        %118  [%i8 %s]
        %119  [%i8 %u]
        %150  [%i16 %s]
        %151  [%i16 %u]
        %182  [%i32 %s]
        %183  [%i32 %u]
        %232  [%f32 %s]
        %244  [%f64 %s]
      ==
    ::
    ++  max
      |=  op=?(%120 %121 %152 %153 %184 %185 %233 %245)
      ^-  instr-vec:sur
      :-  %max
      ?-  op
        %120  [%i8 %s]
        %121  [%i8 %u]
        %152  [%i16 %s]
        %153  [%i16 %u]
        %184  [%i32 %s]
        %185  [%i32 %u]
        %233  [%f32 %s]
        %245  [%f64 %s]
      ==
    ::
    ++  add
      |=  op=?(%110 %111 %112 %142 %143 %144 %174 %206 %228 %240)
      ^-  instr-vec:sur
      :-  %add
      ?-  op
        %110  [%i8 ~]
        %111  [%i8 ~ %s]
        %112  [%i8 ~ %u]
        %142  [%i16 ~]
        %143  [%i16 ~ %s]
        %144  [%i16 ~ %u]
        %174  [%i32 ~]
        %206  [%i64 ~]
        %228  [%f32 ~]
        %240  [%f64 ~]
      ==
    ::
    ++  sub
      |=  op=?(%113 %114 %115 %145 %146 %147 %177 %209 %229 %241)
      ^-  instr-vec:sur
      :-  %sub
      ?-  op
        %113  [%i8 ~]
        %114  [%i8 ~ %s]
        %115  [%i8 ~ %u]
        %145  [%i16 ~]
        %146  [%i16 ~ %s]
        %147  [%i16 ~ %u]
        %177  [%i32 ~]
        %209  [%i64 ~]
        %229  [%f32 ~]
        %241  [%f64 ~]
      ==
    ::
    ++  extend
      |=  op=?(%135 %136 %137 %138 %167 %168 %169 %170 %199 %200 %201 %202)
      ^-  instr-vec:sur
      :-  %extend
      ?-  op
        %135  [%i16 %s %low]
        %136  [%i16 %s %high]
        %137  [%i16 %u %low]
        %138  [%i16 %u %high]
        %167  [%i32 %s %low]
        %168  [%i32 %s %high]
        %169  [%i32 %u %low]
        %170  [%i32 %u %high]
        %199  [%i64 %s %low]
        %200  [%i64 %s %high]
        %201  [%i64 %u %low]
        %202  [%i64 %u %high]
      ==
    ::
    ++  extmul
      |=  op=?(%156 %157 %158 %159 %188 %189 %190 %191 %220 %221 %222 %223)
      ^-  instr-vec:sur
      :-  %extmul
      ?-  op
        %156  [%i16 %s %low]
        %157  [%i16 %s %high]
        %158  [%i16 %u %low]
        %159  [%i16 %u %high]
        %188  [%i32 %s %low]
        %189  [%i32 %s %high]
        %190  [%i32 %u %low]
        %191  [%i32 %u %high]
        %220  [%i64 %s %low]
        %221  [%i64 %s %high]
        %222  [%i64 %u %low]
        %223  [%i64 %u %high]
      ==
    ::
    --  ::  |simd-map
  ::
  --
--  ::  |parser