::  Instruction definitions
::
::  This lib defines instructions as functions
::  local-state -> local-state, as well as exposes
::  the gates to the +apply arm of the interpreter engine.
::
::  To add a r-ary numerical instruction (an instruction
::  that only involves numerical stack values):
::    1. Register the instruction in respective r-ary-num
::    type
::    2. Add an entry to the respective map m in r-ary:fetch
::    3. Define an arm for that instruction. The resulting
::    gate must take an instruction, producing a gate which
::    takes r vals and returns a single val to be pushed on
::    the stack
::
::  Addition of non-numerical instructions, i.e. instructions
::  that read and/or write to fields other than the stack or
::  involve references is slightly more general and requires
::  writing a gate $-(local-state local-state). Look at
::  the examples in set:fetch
::
::::  /hoon/op-def/lib
  ::
/+  wasm-validator
=>  wasm-validator
~%  %wasm-op-def-v0  +  ~
|%
++  op-def
  =,  engine-sur
  ::  "All operators use round-to-nearest ties-to-even,
  ::  except where otherwise specified."
  ::
  =/  rs  ~(. rs %n)
  =/  rd  ~(. rd %n)
  |%
  ::  ++mayb: turn output of gat into a unit
  ::
  ++  mayb
    |*  gat=$-(* *)
    |:  a=+6.gat
    ^-  (unit _$:gat)
    `(gat a)
  ::  ++sure: unwrap output of gat
  ::
  ++  sure
    |*  gat=$-(* (unit))
    |*  a=_+6.gat
    (need (gat a))
  ::
  :: ++torn: map a list with a gate, collapsing units
  ::
  ++  torn
    |*  [a=(list) b=$-(* (unit))]
    =|  l=(list _(need $:b))
    |-  ^-  (unit (list _(need $:b)))
    ?~  a  `(flop l)
    =+  c=(b i.a)
    ?~  c  ~
    $(a t.a, l [u.c l])
  ::
  ++  lane-size
    |=  lt=lane-type
    ^-  @
    (slav %ud (rsh 3 lt))
  ::
  ++  fuse                        ::  from ~paldev
    |*  [a=(list) b=(list)]
    ^-  (list [_?>(?=(^ a) i.a) _?>(?=(^ b) i.b)])
    ?~  a  ~
    ?~  b  ~
    :-  [i.a i.b]
    $(a t.a, b t.b)
  ::
  ::  ++chap: check branching signal, apply instruction
  ::
  ++  chap
    |=  [l=local-state instr=$-(local-state local-state)]
    ^-  local-state
    ?^  br.stack.l  l
    (instr l)
  ::
  ++  page-size  ^~((bex 16))
  ::  +place: places list `b` into list `a`, overwriting contents of `a`
  ::
  ++  place
    |*  [a=(list) off=@ b=(list)]
    |-  ^+  b
    ?~  b  a
    ?>  (lth off (lent a))
    $(a (shot a off i.b), b t.b, off +(off))
  ::  +lim-min: get minimum from limits
  ::
  ++  lim-min
    |=  l=limits
    ^-  @
    ?:  ?=(%flor -.l)
      p.l
    p.l
  ::  +lim-max: get maximum from limits
  ::
  ++  lim-max
    |=  l=limits
    ^-  (unit @)
    ?:  ?=(%flor -.l)
      ~
    `q.l
  ::
  ++  lte-lim
    |=  [a=@ l=limits]
    ^-  ?
    ?:  ?=(%flor -.l)
      &
    (lte a q.l)
  ::  +change: convert stack values to coin-wasm list
  ::
  ++  change
    |=  [a=(list valtype) b=(list val)]
    ^-  (list coin-wasm)
    ?.  &(?=(^ a) ?=(^ b))
      ?>  &(?=(~ a) ?=(~ b))
      ~
    :_  $(a t.a, b t.b)
    ;;  coin-wasm  ::  the compiler isn't convinced that the
    ?-    i.a      ::  expression below is a coin-wasm, why? :(
        ?(num-type vec-type)
      [i.a ?>(?=(@ i.b) i.b)]
    ::
        %extn
      ?>(?=([%ref %extn ~] i.b) i.b)  ::  return non-null external references?
    ::
        %func
      ?>(?=([%ref %func *] i.b) i.b)
    ==
  ::
  ++  to-si
    |=  [base=@ n=@]
    ^-  @s
    =.  n  (mod n (bex base))
    =/  sign=?  (lth n (bex (dec base)))
    %+  new:si  sign
    ?:  sign  n
    (sub (bex base) n)
  ::
  ++  en-si
    |=  [base=@ s=@s]
    ^-  @
    ?:  (syn:si s)
      +:(old:si s)
    (sub (bex base) +:(old:si s))
  ::
  ++  sat
    |=  [size=@ s=@s mode=?(%u %s)]
    ^-  @
    =,  si
    =;  sat-s=@s
      (en-si size sat-s)
    ?:  =(%u mode)
      ?:  =(--1 (cmp s (sum (new & (bex size)) -1)))
        (sum (new & (bex size)) -1)
      ?:  =(-1 (cmp s --0))
        --0
      s
    ?:  =(--1 (cmp s (sum (new & (bex (dec size))) -1)))
      (sum (new & (bex (dec size))) -1)
    ?:  =(-1 (cmp s (new | (bex (dec size)))))
      (new | (bex (dec size)))
    s
  ::
  ++  coin-to-val
    |=  c=coin-wasm
    ^-  val
    ?:  ?=(%ref -.c)
      c
    ?-  -.c
      ?(%i32 %f32)  (mod +.c ^~((bex 32)))
      ?(%i64 %f64)  (mod +.c ^~((bex 64)))
      %v128         (mod +.c ^~((bex 128)))
    ==
  ::
  ++  val-to-coin
    |=  [v=val ex=coin-wasm]
    ^-  coin-wasm
    ?@  v
      ?<  ?=(%ref -.ex)
      ;;  coin-wasm    ::  the compiler isn't convinced that the
      [-.ex v]         ::  expression below is a coin-wasm, why? :(
    ?>  ?=(%ref -.ex)
    ?>  =(+<.v +<.ex)  ::  assert: same reftypes
    v
  ::  ++snug: unitized ++snag
  ::
  ++  snug
    |*  [a=@ b=(list)]
    |-  ^-  (unit _?>(?=(^ b) i.b))
    ?~  b  ~
    ?:  =(0 a)  `i.b
    $(b t.b, a (dec a))
  ::  ++shot: replace existing item in a list
  ::
  ++  shot
    |*  [a=(list) b=@ c=*]
    ^+  a
    ?>  (lth b (lent a))
    (snap a b c)
  ::  ++buy: resolve import. If shop is empty, build
  ::  a request, otherwise typecheck ans push values on the stack
  ::
  ++  buy
    |=  [l=local-state req=[[mod=cord name=cord] =request] type=(list valtype)]
    ^-  local-state
    ?~  shop.store.l
      =,  store.l
      l(br.stack [%bloq req module mem tables globals])
    =/  valid-types=?
      =/  res=(list coin-wasm)  p.i.shop.store.l
      |-  ^-  ?
      ?:  &(?=(~ res) ?=(~ type))  &
      ?>  &(?=(^ res) ?=(^ type))
      ?&  =(-.i.res i.type)
          $(res t.res, type t.type)
      ==
    ?>  valid-types
    %=    l
        va.stack
      %+  weld
        %-  flop
        (turn p.i.shop.store.l coin-to-val)
      va.stack.l
    ::
        store
      =,  store.l
      [t.shop q.i.shop]
    ==
  ::
  ::  |grab: import-related utils. Gates return either a local
  ::  instance of an object (sometimes with its index)
  ::  or its external reference
  ::
  ++  grab
    |%
    ++  func
      |=  [id=@ st=store]
      ^-  (each function [[mod=cord name=cord] type-id=@])
      =,  import-section.module.st
      =+  imp=(snug id funcs)
      ?^  imp  [%| u.imp]
      :-  %&
      (snag (sub id (lent funcs)) function-section.module.st)
    ::
    ++  table
      |=  [id=@ st=store]
      ^-  %+  each  (pair @ (list $>(%ref coin-wasm)))
          [[mod=cord name=cord] t=^table]
      =,  import-section.module.st
      =+  imp=(snug id tables)
      ?^  imp  [%| u.imp]
      :-  %&
      =+  idx=(sub id (lent tables))
      :-  idx
      (snag idx tables.st)
    ::
    ++  memo
      |=  [id=@ st=store]
      ^-  %+  each  [buffer=@ n-pages=@]
          [[mod=cord name=cord] l=limits]
      =,  import-section.module.st
      =+  imp=(snug id memos)
      ?^  imp  [%| u.imp]
      [%& (need mem.st)]
    ::
    ++  glob
      |=  [id=@ st=store]
      ^-  %+  each  (pair @ coin-wasm)
          [[mod=cord name=cord] v=valtype m=?(%con %var)]
      =,  import-section.module.st
      =+  imp=(snug id globs)
      ?^  imp  [%| u.imp]
      :-  %&
      =+  idx=(sub id (lent globs))
      :-  idx
      (snag idx globals.st)
    ::
    --
  ::
  ++  mem-store
    |=  [index=@ size=@ content=@ buffer=@ n-pages=@]
    ^-  (unit [buffer=@ n-pages=@])
    ?.  (lte (add index size) (mul n-pages page-size))
      ~
    `[(sew 3 [index size content] buffer) n-pages]
  ::
  ++  mem-load
    |=  [index=@ size=@ buffer=@ n-pages=@]
    ^-  (unit @)
    ?.  (lte (add index size) (mul n-pages page-size))
      ~
    `(cut 3 [index size] buffer)
  ::
  ::  |kind: instruction classes
  ::
  ++  kind
    |%
    +$  nullary  $?  %unreachable
                    %nop
                    %return
                    %drop
                ==
    ::
    +$  ref  ?(%ref-null %ref-is-null %ref-func)
    +$  get  ?(%global-get %local-get)
    +$  set  ?(%global-set %local-set %local-tee)
    +$  branch  ?(%br %br-if %br-table)
    +$  table  $?
                %table-get
                %table-set
                %table-init
                %elem-drop
                %table-copy
                %table-grow
                %table-size
                %table-fill
              ==
    ::
    +$  memo  $?
                %memory-size
                %memory-grow
                %memory-init
                %data-drop
                %memory-copy
                %memory-fill
              ==
    ::
    +$  unary-num
      $?
        ::  integer
        ::
        %clz  %ctz  %popcnt
        ::  float
        ::
        %abs  %neg  %sqrt  %ceil  %floor  %trunc
        %nearest
        ::  int test
        ::
        %eqz
        ::  convert
        ::
        %wrap  %extend  %trunc  %convert  %demote
        %promote  %reinterpret
      ==
    ::
    +$  binary-num
      $?
        ::  Operations
        ::
        %add  %sub  %mul  %div  %rem  %and  %or  %xor
        %shl  %shr  %rotl  %rotr
        ::  float only
        ::
        %min  %max  %copysign
        ::  Comparisons
        ::
        %eq  %ne  %lt  %gt  %le  %ge
      ==
    ::
    --
  ::  +fetch-gate: turn $instruction to a gate to transform
  ::  the local state
  ::
  ++  fetch-gate
    |=  i=$<(?(%call %loop %call-indirect %block %if) instruction)
    ^-  $-(local-state local-state)
    ?-    -.i
        %vec          (simd +.i)
        nullary:kind  (null:fetch i)
        ref:kind      (ref:fetch i)
        %load         (load:fetch i)
        %store        (store:fetch i)
        %const        (const:fetch i)
        get:kind      (get:fetch i)
        set:kind      (set:fetch i)
        branch:kind   (branch:fetch i)
        table:kind    (table:fetch i)
        memo:kind     (memo:fetch i)
        %select       select:fetch
        %dbug         (dbug:fetch i)
    ::
        unary-num:kind
      |=  l=local-state
      ^-  local-state
      ?>  ?=([a=@ rest=*] va.stack.l)
      =,  va.stack.l
      =+  val=((unar:fetch i) a)
      ?~  val  l(br.stack [%trap ~])
      l(va.stack [u.val rest])
    ::
        binary-num:kind
      |=  l=local-state
      ^-  local-state
      ?>  ?=([b=@ a=@ rest=*] va.stack.l)
      =,  va.stack.l
      =+  val=((bina:fetch i) a b)
      ?~  val  l(br.stack [%trap ~])
      l(va.stack [u.val rest])
    ::
    ==
  ::
  ::  |fetch: core with instruction definitions
  ::
  ++  fetch
    |%
    ++  dbug
      =-  |=  i=instruction
          ((~(got by m) ;;(@tas +<.i)) i)
      ^~
      ^=  m
      ^-  (map @tas $-(instruction $-(local-state local-state)))
      |^
      %-  my
      :~
        print-tee+print-tee
      ==
      ::
      ++  print-tee
        |=  i=instruction
        ?>  ?=([%dbug %print-tee a=term] i)
        |=  l=local-state
        ^-  local-state
        ~&  [a.i ;;(@ux -.va.stack.l)]
        l
      ::
      --
    ::
    ++  select
      |=  l=local-state
      ^-  local-state
      ?>  ?=([which=@ val2=* val1=* rest=*] va.stack.l)
      =,  va.stack.l
      %=    l
          va.stack
        [?.(=(0 which) val1 val2) rest]
      ==
    ::
    ++  null
      =-  |=  i=instruction
          (~(got by m) ;;(@tas -.i))
      ^~
      ^=  m
      ^-  (map @tas $-(local-state local-state))
      |^
      %-  my
      :~
        unreachable+unreachable
        nop+nop
        return+return
        drop+drop
      ==
      ::
      ++  unreachable
        |=  l=local-state
        ^-  local-state
        l(br.stack [%trap ~])
      ::
      ++  nop  |=(local-state +<)
      ++  return
        |=  l=local-state
        ^-  local-state
        l(br.stack [%retr ~])
      ::
      ++  drop
        |=  l=local-state
        ^-  local-state
        l(va.stack +.va.stack.l)
      ::
      --
    ::
    ++  ref
      =-  |=  i=instruction
          ?>  ?=(ref:kind -.i)
          ^-  $-(local-state local-state)
          ((~(got by m) ;;(@tas -.i)) i)
      ^~
      ^=  m
      ^-  (map @tas $-(instruction $-(local-state local-state)))
      |^
      %-  my
      :~
        ref-null+ref-null
        ref-is-null+ref-is-null
        ref-func+ref-func
      ==
      ::
      ++  ref-null
        |=  i=instruction
        ?>  ?=(%ref-null -.i)
        |=  l=local-state
        ^-  local-state
        %=    l
            va.stack
          :_  va.stack.l
          :-  %ref
          ?-  t.i
            %extn  [%extn ~]
            %func  [%func ~]
          ==
        ==
      ::
      ++  ref-is-null
        |=  *
        |=  l=local-state
        ^-  local-state
        ?>  ?=([ref=[%ref *] rest=*] va.stack.l)
        =,  va.stack.l
        =/  out=@
          ?@(+>.ref 1 0)
        l(va.stack [out rest])
      ::
      ++  ref-func
        |=  i=instruction
        ?>  ?=(%ref-func -.i)
        |=  l=local-state
        ^-  local-state
        l(va.stack [[%ref %func ~ func-id.i] va.stack.l])
      ::
      --
    ::
    ++  load
      |=  i=instruction
      ?>  ?=(%load -.i)
      |=  l=local-state
      ^-  local-state
      ?>  ?=([addr=@ rest=*] va.stack.l)
      =,  va.stack.l
      =/  index=@  (add addr offset.m.i)
      =+  mem=(memo:grab 0 store.l)
      ?:  ?=(%| -.mem)
        %^  buy  l(va.stack rest)
          :*  -.p.mem
              %memo
              (change ~[%i32] ~[addr])
              i
          ==
        ~[type.i]
      =;  loaded=(unit @)
        ?~  loaded  l(br.stack [%trap ~])
        l(va.stack [u.loaded rest])
      ?~  n.i
        ?-    type.i
            ?(%i32 %f32)
          (mem-load index 4 p.mem)
        ::
            ?(%i64 %f64)
          (mem-load index 8 p.mem)
        ==
      ?>  ?=(^ mode.i)
      ?-    u.mode.i
          %u
        (mem-load index (div u.n.i 8) p.mem)
      ::
          %s
        %-  bind  :_  (cury en-si ?+(type.i !! %i32 32, %i64 64))
        %-  bind  :_  (cury to-si u.n.i)
        (mem-load index (div u.n.i 8) p.mem)
      ==
    ::
    ++  store
      |=  i=instruction
      ?>  ?=(%store -.i)
      |=  l=local-state
      ^-  local-state
      ?>  ?=([content=@ addr=@ rest=*] va.stack.l)
      =,  va.stack.l
      =+  memo=(memo:grab 0 store.l)
      ?:  ?=(%| -.memo)
        %^  buy  l(va.stack rest)
          :*  -.p.memo
              %memo
              (change ~[%i32 type.i] ~[addr content])
              i
          ==
        ~
      =/  index=@  (add addr offset.m.i)
      =;  [size=@ to-put=@]
        =+  stored=(mem-store index size to-put p.memo)
        ?~  stored  l(br.stack [%trap ~])
        %=    l
            va.stack  rest
        ::
            mem.store
          `u.stored
        ==
      ?~  n.i
        :_  content
        ?-(type.i ?(%i32 %f32) 4, ?(%i64 %f64) 8)
      [(div u.n.i 8) (mod content (bex u.n.i))]
    ::
    ++  const
      |=  i=instruction
      ?>  ?=(%const -.i)
      |=  l=local-state
      ^-  local-state
      l(va.stack [(coin-to-val p.i) va.stack.l])
    ::
    ++  get
      |=  i=instruction
      ?>  ?=(get:kind -.i)
      |=  l=local-state
      ^-  local-state
      ?:  ?=(%local-get -.i)
        l(va.stack [(snag index.i locals.l) va.stack.l])
      =+  glob=(glob:grab index.i store.l)
      ?:  ?=(%| -.glob)
        %^  buy  l
          [-.p.glob %glob ~ i]
        ~[v.p.glob]
      l(va.stack [(coin-to-val q.p.glob) va.stack.l])
    ::
    ++  set
      |=  i=instruction
      ?>  ?=(set:kind -.i)
      |=  l=local-state
      ^-  local-state
      ?>  ?=([a=* rest=*] va.stack.l)
      =,  va.stack.l
      ?-    -.i
          %local-set
        l(locals (snap locals.l index.i a), va.stack rest)
      ::
          %local-tee
        l(locals (snap locals.l index.i a))
      ::
          %global-set
        =+  glob=(glob:grab index.i store.l)
        ?:  ?=(%| -.glob)
          %^  buy  l(va.stack rest)
            [-.p.glob %glob (change ~[v.p.glob] ~[a]) i]
          ~
        %=    l
            va.stack  rest
            globals.store
          %^  snap  globals.store.l  p.p.glob
          (val-to-coin a q.p.glob)
        ==
      ::
      ==
    ::
    ++  branch
      |=  i=instruction
      ?>  ?=(branch:kind -.i)
      |=  l=local-state
      ^-  local-state
      ?-    -.i
          %br  l(br.stack [%targ label.i])
          %br-if
        ?>  ?=([a=@ rest=*] va.stack.l)
        =,  va.stack.l
        ?.  =(a 0)
          l(br.stack [%targ label.i], va.stack rest)
        l(va.stack rest)
      ::
          %br-table
        ?>  ?=([a=@ rest=*] va.stack.l)
        =,  va.stack.l
        ?:  (gte a (lent label-vec.i))
          l(va.stack rest, br.stack [%targ label-default.i])
        l(va.stack rest, br.stack [%targ (snag a label-vec.i)])
      ==
    ::
    ++  table
      =-  |=  i=instruction
          ?>  ?=(table:kind -.i)
          ^-  $-(local-state local-state)
          ((~(got by m) ;;(@tas -.i)) i)
      ^~
      ^=  m
      ^-  (map @tas $-(instruction $-(local-state local-state)))
      |^
      %-  my
      :~
        table-get+table-get
        table-set+table-set
        table-init+table-init
        elem-drop+elem-drop
        table-copy+table-copy
        table-grow+table-grow
        table-size+table-size
        table-fill+table-fill
      ==
      ::
      ++  table-get
        |=  i=instruction
        ?>  ?=(%table-get -.i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([a=@ rest=*] va.stack.l)
        =,  va.stack.l
        =+  tab=(table:grab tab-id.i store.l)
        ?:  ?=(%| -.tab)
          %^  buy  l(va.stack rest)
            [-.p.tab %tabl (change ~[%i32] ~[a]) i]
          ~[p.t.p.tab]
        l(va.stack [(snag a q.p.tab) rest])
      ::
      ++  table-set
        |=  i=instruction
        ?>  ?=(%table-set -.i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([ref=* a=@ rest=*] va.stack.l)
        ?<  ?=(@ ref.va.stack.l)
        =,  va.stack.l
        =+  tab=(table:grab tab-id.i store.l)
        ?:  ?=(%| -.tab)
          %^  buy  l(va.stack rest)
            [-.p.tab %tabl (change ~[%i32 p.t.p.tab] ~[a ref]) i]
          ~
        %=    l
            va.stack  rest
        ::
            tables.store
          %^  shot  tables.store.l  p.p.tab
          (shot q.p.tab a ref)
        ==
      ::
      ++  table-init
        |=  i=instruction
        ?>  ?=(%table-init -.i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([n=@ s=@ d=@ rest=*] va.stack.l)
        =,  va.stack.l
        =+  tab=(table:grab tab-id.i store.l)
        ?:  ?=(%| -.tab)
          %^  buy  l(va.stack rest)
            [-.p.tab %tabl (change ~[%i32 %i32 %i32] ~[d s n]) i]
          ~
        =+  elem=(snag elem-id.i elem-section.module.store.l)
        |-  ^-  local-state
        ?:  =(n 0)  l
        =/  ref=$>(%ref coin-wasm)
          =+  op=(snag s i.elem)
          ?:  ?=(%ref-null -.op)
            :-  %ref
            ?-  t.op
              %extn  [%extn ~]
              %func  [%func ~]
            ==
          [%ref %func ~ func-id.op]
        =+  [rest d s n]=[rest d s n]  ::  save before changing l
        =>  .(l `local-state`l)
        =.  l
          %-  (table-set [%table-set tab-id.i])
          l(va.stack [ref d rest])
        ?^  br.stack.l  l
        ?.  ?&  (lth +(d) ^~((bex 32)))
                (lth +(s) ^~((bex 32)))
            ==
          l(br.stack [%trap ~])
        $(va.stack.l [(dec n) +(s) +(d) va.stack.l])
      ::
      ++  elem-drop
        |=  i=instruction
        ?>  ?=(%elem-drop -.i)
        |=  l=local-state
        ^-  local-state
        =,  module.store.l
        =.  elem-section.module.store.l
          %^  shot  elem-section  elem-id.i
          =+  elem=(snag elem-id.i elem-section)
          ^-  ^elem
          [t.elem ~ m.elem]
        l
      ::  Here I express %table-copy expression in
      ::  exactly the same way it is defined in the spec
      ::  in order to propagate %bloq's correctly, since
      ::  only one of two tables might be local, or both,
      ::  or neither
      ::
      ++  table-copy
        |=  i=instruction
        ?>  ?=(%table-copy -.i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([n=@ s=@ d=@ rest=*] va.stack.l)
        =,  va.stack.l
        ?:  =(n 0)  l
        =+  [d s n rest]=[d s n rest]
        =>  .(va.stack.l `(pole val)`va.stack.l)
        =.  l
          ?:  (lte d s)
            =.  l
              %.  l(va.stack [s d rest])
              ;~  chap
                (table-get [%table-get tab-id-y.i])
                (table-set [%table-set tab-id-x.i])
              ==
            ?.  ?&  (lth +(d) ^~((bex 32)))
                    (lth +(s) ^~((bex 32)))
                ==
              l(br.stack [%trap ~])
            l(va.stack [+(s) +(d) va.stack.l])
          ?.  ?&  (lth (dec (add d n)) ^~((bex 32)))
                  (lth (dec (add s n)) ^~((bex 32)))
              ==
            l(br.stack [%trap ~])
          =.  l
            %.  l(va.stack [(dec (add s n)) (dec (add d n)) rest])
            ;~  chap
                (table-get [%table-get tab-id-y.i])
                (table-set [%table-set tab-id-x.i])
              ==
          l(va.stack [s d va.stack.l])
        ?^  br.stack.l  l
        $(va.stack.l [(dec n) va.stack.l])
      ::
      ++  table-grow
        |=  i=instruction
        ?>  ?=(%table-grow -.i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([n=@ val=* rest=*] va.stack.l)
        ?<  ?=(@ val.va.stack.l)
        =,  va.stack.l
        =+  tab=(table:grab tab-id.i store.l)
        ?:  ?=(%| -.tab)
          %^  buy  l(va.stack rest)
            [-.p.tab %tabl (change ~[p.t.p.tab %i32] ~[val n]) i]
          ~[%i32]
        ?.  %+  lte-lim  (add n (lent q.p.tab))
            q:(snag p.p.tab table-section.module.store.l)
          l(va.stack [^~((en-si 32 -1)) rest])
        %=    l
            va.stack  [(lent q.p.tab) rest]
        ::
            tables.store
          %^  shot  tables.store.l  p.p.tab
          (weld q.p.tab (reap n val))
        ==
      ::
      ++  table-size
        |=  i=instruction
        ?>  ?=(%table-size -.i)
        |=  l=local-state
        ^-  local-state
        =+  tab=(table:grab tab-id.i store.l)
        ?:  ?=(%| -.tab)
          %^  buy  l
            [-.p.tab %tabl ~ i]
          ~[%i32]
        l(va.stack [(lent q.p.tab) va.stack.l])
      ::
      ++  table-fill
        |=  i=instruction
        ?>  ?=(%table-fill -.i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([n=@ val=* i=@ rest=*] va.stack.l)
        ?<  ?=(@ val.va.stack.l)
        =,  va.stack.l
        =+  tab=(table:grab tab-id.^i store.l)
        ?:  ?=(%| -.tab)
          %^  buy  l(va.stack rest)
            [-.p.tab %tabl (change ~[%i32 p.t.p.tab %i32] ~[i val n]) ^i]
          ~
        %=    l
            va.stack  rest
        ::
            tables.store
            %^  shot  tables.store.l  p.p.tab
            (place q.p.tab i (reap n val))
        ==
      ::
      --
    ++  memo
      =-  |=  i=instruction
          ?>  ?=(memo:kind -.i)
          ^-  $-(local-state local-state)
          ((~(got by m) ;;(@tas -.i)) i)
      ^~
      ^=  m
      ^-  (map @tas $-(instruction $-(local-state local-state)))
      |^
      %-  my
      :~
        memory-size+memory-size
        memory-grow+memory-grow
        memory-init+memory-init
        data-drop+data-drop
        memory-copy+memory-copy
        memory-fill+memory-fill
      ==
      ::
      ++  memory-size
        |=  i=instruction
        ?>  ?=(%memory-size -.i)
        |=  l=local-state
        ^-  local-state
        =+  memo=(memo:grab 0 store.l)
        ?:  ?=(%| -.memo)
          %^  buy  l
            [-.p.memo %memo ~ i]
          ~[%i32]
        l(va.stack [n-pages.p.memo va.stack.l])
      ::
      ++  memory-grow
        |=  i=instruction
        ?>  ?=(%memory-grow -.i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([a=@ rest=*] va.stack.l)
        =,  va.stack.l
        =+  memo=(memo:grab 0 store.l)
        ::  imported memory
        ::
        ?:  ?=(%| -.memo)
          %^  buy  l(va.stack rest)
            [-.p.memo %memo (change ~[%i32] ~[a]) i]
          ~[%i32]
        ::  local memory
        ::
        =/  memory-section  memory-section.module.store.l
        =/  limits  -.memory-section
        ?.  (lte-lim (add n-pages.p.memo a) limits)
          l(va.stack [^~((en-si 32 -1)) rest])
        %=  l
          va.stack  [n-pages.p.memo rest]
          mem.store  `[buffer.p.memo (add n-pages.p.memo a)]
        ==
      ::
      ++  memory-init
        |=  i=instruction
        ?>  ?=(%memory-init -.i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([n=@ s=@ d=@ rest=*] va.stack.l)
        =,  va.stack.l
        =+  memo=(memo:grab 0 store.l)
        ?:  ?=(%| -.memo)
          %^  buy  l(va.stack rest)
            [-.p.memo %memo (change ~[%i32 %i32 %i32] ~[d s n]) i]
          ~
        =/  data-bytes=octs
          =+  data=(snag x.i data-section.module.store.l)
          ?:(?=(%acti -.data) b.data b.data)
        ?.  ?&  (lte (add s n) p.data-bytes)
                (lte (add d n) (mul page-size n-pages.p.memo))
            ==
          l(br.stack [%trap ~])
        %=    l
            va.stack  rest
        ::
            mem.store
          :-  ~
          :_  n-pages.p.memo
          (sew 3 [d n (rsh [3 s] q.data-bytes)] buffer.p.memo)
        ==
      ::
      ++  data-drop
        |=  i=instruction
        ?>  ?=(%data-drop -.i)
        |=  l=local-state
        ^-  local-state
        =,  module.store.l
        =.  data-section.module.store.l
          %^  shot  data-section  x.i
          =+  data=(snag x.i data-section)
          ?-  -.data
            %acti  data(b *octs)
            %pass  data(b *octs)
          ==
        l
      ::
      ++  memory-copy
        |=  i=instruction
        ?>  ?=(%memory-copy -.i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([n=@ s=@ d=@ rest=*] va.stack.l)
        =,  va.stack.l
        =+  memo=(memo:grab 0 store.l)
        ?:  ?=(%| -.memo)
          %^  buy  l(va.stack rest)
            [-.p.memo %memo (change ~[%i32 %i32 %i32] ~[d s n]) i]
          ~
        ?.  ?&  (lte (add s n) (mul page-size n-pages.p.memo))
                (lte (add d n) (mul page-size n-pages.p.memo))
            ==
          l(br.stack [%trap ~])
        %=    l
            va.stack  rest
        ::
            mem.store
          :-  ~
          :_  n-pages.p.memo
          (sew 3 [d n (rsh [3 s] buffer.p.memo)] buffer.p.memo)
        ==
      ::
      ++  memory-fill
        |=  i=instruction
        ?>  ?=(%memory-fill -.i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([n=@ val=@ d=@ rest=*] va.stack.l)
        =,  va.stack.l
        =+  memo=(memo:grab 0 store.l)
        ?:  ?=(%| -.memo)
          %^  buy  l(va.stack rest)
            [-.p.memo %memo (change ~[%i32 %i32 %i32] ~[d val n]) i]
          ~
        ?.  (lte (add d n) (mul page-size n-pages.p.memo))
          l(br.stack [%trap ~])
        %=    l
            va.stack  rest
        ::
            mem.store
          :-  ~
          :_  n-pages.p.memo
          (sew 3 [d n (fil 3 n val)] buffer.p.memo)
        ==
      ::
      --
    ::
    ::  +unar: unary gate fetcher
    ::
    ++  unar
      =-  |=  i=instruction
          ?>  ?=(unary-num:kind -.i)
          ^-  $-(@ (unit @))
          ~+
          ((~(got by m) ;;(@tas -.i)) i)
      ^~
      ^=  m
      ^-  (map @tas $-(instruction $-(@ (unit @))))
      |^
      %-  my
      :~
        clz+clz
        ctz+ctz
        popcnt+popcnt
        abs+abs
        neg+neg
        sqrt+sqrt
        ceil+ceil
        floor+floor
        trunc+trunc
        nearest+nearest
        eqz+eqz
        wrap+wrap
        extend+extend
        trunc+trunc
        convert+convert
        demote+demote
        promote+promote
        reinterpret+reinterpret
      ==
      ::
      ++  clz
        |=  i=instruction
        %-  mayb
        ?>  ?=(%clz -.i)
        =/  base=@  ?-(type.i %i32 32, %i64 64)
        |=  v=@
        (sub base (xeb v))
      ::
      ++  ctz
        |=  i=instruction
        %-  mayb
        ?>  ?=(%ctz -.i)
        =/  counter-max=@  ?-(type.i %i32 32, %i64 64)
        |=  v=@
        ^-  @
        =+  counter=0
        |-  ^-  @
        ?:  =(counter counter-max)  counter
        ?:  =(1 (mod v 2))  counter
        $(v (div v 2), counter +(counter))
      ::
      ++  popcnt
        |=  *
        %-  mayb
        |=  v=@
        ^-  @
        =+  counter=0
        |-  ^-  @
        ?:  =(v 0)  counter
        $(v (div v 2), counter (add counter (mod v 2)))
      ::
      ++  abs
        |=  i=instruction
        %-  mayb
        ?>  ?=(%abs -.i)
        ?.  ?=(?(%f32 %f64) type.i)
          =+  size=(lane-size type.i)
          |=  v=@
          =/  v-signed=@s  (to-si size v)
          ?.  =(-1 (cmp:si v-signed --0))  v
          (mod (en-si size (pro:si -1 v-signed)) (bex size))
        |=  v=@
        ^-  @
        =/  sign=?
          ?-  type.i
            %f32  (sig:rs v)
            %f64  (sig:rd v)
          ==
        ?:  sign  v
        %+  sub  v
        ?-  type.i
          %f32  ^~((bex 31))
          %f64  ^~((bex 63))
        ==
      ::
      ++  neg
        |=  i=instruction
        %-  mayb
        ?>  ?=(%neg -.i)
        =+  size=(lane-size type.i)
        |=  v=@
        ^-  @
        %+  ~(sum fo (bex size))  v
        (bex (dec size))
      ::
      ++  sqrt
        |=  i=instruction
        %-  mayb
        ?>  ?=(%sqrt -.i)
        |=  v=@
        ^-  @
        ?-  type.i
          %f32  (sqt:rs v)
          %f64  (sqt:rd v)
        ==
      ::
      ++  ceil
        |=  i=instruction
        %-  mayb
        ?>  ?=(%ceil -.i)
        =+  ^=  r
            ?-  type.i
              %f32  ~(. rs %u)
              %f64  ~(. rd %u)
            ==
        =/  neg-one=@   ?-(type.i %f32 .-1, %f64 .~-1)
        =/  neg-zero=@  ?-(type.i %f32 .-0, %f64 .~-0)
        |=  v=@
        ^-  @
        =/  int=(unit @s)  (toi:r v)
        ?~  int  v
        ?:  &((gth:r v neg-one) (lth:r v `@`0))  neg-zero
        ?:  =(--0 u.int)  v
        (san:r u.int)
      ::
      ++  floor
        |=  i=instruction
        %-  mayb
        ?>  ?=(%floor -.i)
        =+  ^=  r
            ?-  type.i
              %f32  ~(. rs %d)
              %f64  ~(. rd %d)
            ==
        =/  one=@   ?-(type.i %f32 .1, %f64 .~1)
        |=  v=@
        ^-  @
        =/  int=(unit @s)  (toi:r v)
        ?~  int  v
        ?:  &((gth:r v `@`0) (lth:r v one))  0
        ?:  =(--0 u.int)  v
        (san:r u.int)
      ::
      ++  trunc
        |=  i=instruction
        ?>  ?=(%trunc -.i)
        ?~  mode.i
          ::  no conversion
          ::
          ?>  ?=(?(%f32 %f64) type.i)
          =+  ^=  r
              ?-  type.i
                %f32  ~(. rs %z)
                %f64  ~(. rd %z)
              ==
          =/  one=@       ?-(type.i %f32 .1, %f64 .~1)
          =/  neg-zero=@  ?-(type.i %f32 .-0, %f64 .~-0)
          =/  neg-one=@   ?-(type.i %f32 .-1, %f64 .~-1)
          %-  mayb
          |=  v=@
          ^-  @
          =/  int=(unit @s)  (toi:r v)
          ?~  int  v
          ?:  &((lth:r v one) (gth:r v `@`0))  0
          ?:  &((lth:r v `@`0) (gth:r v neg-one))  neg-zero
          ?:  =(--0 u.int)  v
          (san:r u.int)
        ::  conversion
        ::
        ?>  ?=(?(%i32 %i64) type.i)
        ?>  ?=(^ mode.i)
        ?>  ?=(^ source-type.i)
        =+  ^=  r
            ?-  u.source-type.i
              %f32  ~(. rs %z)
              %f64  ~(. rd %z)
            ==
        =/  one=@       ?-(u.source-type.i %f32 .1, %f64 .~1)
        =/  neg-zero=@  ?-(u.source-type.i %f32 .-0, %f64 .~-0)
        =/  neg-one=@   ?-(u.source-type.i %f32 .-1, %f64 .~-1)
        =/  base=@      ?-(type.i %i32 32, %i64 64)
        =/  [below=@s above=@s]
          =,  si
          ?-    u.mode.i
              %s
            :-  (sum (new | (bex (dec base))) -1)
            (new & (bex (dec base)))
          ::
              %u
            :-  -1
            (new & (bex base))
          ==
        =/  [lower-int=@ upper-int=@]
          =,  si
          ?-    u.mode.i
              %u
            [0 (dec (bex base))]
          ::
              %s
            :-  (en-si base (new | (bex (dec base))))
            (en-si base (new & (dec (bex (dec base)))))
          ==
        |=  v=@
        ^-  (unit @)
        =/  int=(unit @s)  (toi:r v)
        ?~  int
          ?.  sat.i  ~
          =/  =fn  (sea:r v)
          ?:  ?=(%n -.fn)  ``@`0
          ?:  ?=(%i -.fn)
            ?:  s.fn
              `upper-int
            `lower-int
          ~
        ?.  =((cmp:si u.int below) --1)
          ?.  sat.i  ~
          `lower-int
        ?.  =((cmp:si above u.int) --1)
          ?.  sat.i  ~
          `upper-int
        ?-    u.mode.i
          %u  `(abs:si u.int)
          %s  `(en-si base u.int)
        ==
      ::
      ++  nearest
        |=  i=instruction
        %-  mayb
        ?>  ?=(%nearest -.i)
        =+  ^=  r
            ?-  type.i
              %f32  ~(. rs %n)
              %f64  ~(. rd %n)
            ==
        =/  neg-zero=@  ?-(type.i %f32 .-0, %f64 .~-0)
        |=  v=@
        ^-  @
        =/  int=(unit @s)  (toi:r v)
        ?~  int  v
        ?:  =(--0 u.int)
          ?:  |((gth:r v `@`0) =(v 0))  0
          neg-zero
        (san:r u.int)
      ::
      ++  eqz
        |=  *
        %-  mayb
        |=  v=@
        ^-  @
        ?:(=(v 0) 1 0)
      ::
      ++  wrap
        |=  *
        %-  mayb
        |=  v=@
        ^-  @
        (mod v ^~((bex 32)))
      ::
      ++  extend
        |=  i=instruction
        %-  mayb
        ?>  ?=(%extend -.i)
        =/  base=@  ?-(type.i %i32 32, %i64 64)
        |=  v=@
        ^-  @
        ?-  mode.i
          %u  v
          %s  (en-si base (to-si source-size.i v))
        ==
      ::
      ++  convert
        |=  i=instruction
        %-  mayb
        ?>  ?=(%convert -.i)
        =+  ^=  r
            ?-  type.i
              %f32  rs
              %f64  rd
            ==
        =/  base=@  ?-(source-type.i %i32 32, %i64 64)
        |=  v=@
        ^-  @
        ?:  ?=(%u mode.i)
          (sun:r v)
        (san:r (to-si base v))
      ::
      ++  demote
        |=  *
        %-  mayb
        |=  v=@
        ^-  @
        (bit:rs (sea:rd v))
      ::
      ++  promote
        |=  *
        %-  mayb
        |=  v=@
        ^-  @
        (bit:rd (sea:rs v))
      ::
      ++  reinterpret
        |=  *
        %-  mayb
        |=(v=@ v)
      ::
      --
    ::  +bina: binary gate fetcher.
    ::  Attention, arithmetic gates are shadowed here, but not elsewhere
    ::
    ++  bina
      =-  |=  i=instruction
          ?>  ?=(binary-num:kind -.i)
          ^-  $-([@ @] (unit @))
          ~+
          ((~(got by m) ;;(@tas -.i)) i)
      ^~
      ^=  m
      ^-  (map @tas $-(instruction $-([@ @] (unit @))))
      |^
      %-  my
      :~
        add+add
        sub+sub
        mul+mul
        div+div
        rem+rem
        and+and
        or+or
        xor+xor
        shl+shl
        shr+shr
        rotl+rotl
        rotr+rotr
        min+min
        max+max
        copysign+copysign
        eq+eq
        ne+ne
        lt+lt
        gt+gt
        le+le
        ge+ge
      ==
      ::
      ++  add
        |=  i=instruction
        %-  mayb
        ?>  ?=(%add -.i)
        =/  modulo=@  (bex (lane-size type.i))
        |=  [v=@ w=@]
        ^-  @
        ?+  type.i  (~(sum fo modulo) v w)
          %f32  (add:rs v w)
          %f64  (add:rd v w)
        ==
      ::
      ++  sub
        |=  i=instruction
        %-  mayb
        ?>  ?=(%sub -.i)
        =/  modulo=@  (bex (lane-size type.i))
        |=  [v=@ w=@]
        ^-  @
        ?+  type.i  (~(dif fo modulo) v w)
          %f32  (sub:rs v w)
          %f64  (sub:rd v w)
        ==
      ::
      ++  mul
        |=  i=instruction
        %-  mayb
        ?>  ?=(%mul -.i)
        =/  base=@  (bex (lane-size type.i))
        |=  [v=@ w=@]
        ^-  @
        ?+  type.i  (~(pro fo base) v w)
          %f32  (mul:rs v w)
          %f64  (mul:rd v w)
        ==
      ::
      ++  div
        |=  i=instruction
        ?>  ?=(%div -.i)
        ?-    type.i
            %f32  (mayb div:rs)
            %f64  (mayb div:rd)
        ::
            *
          =/  mode=?(%u %s)  (fall mode.i %u)
          ?-    mode
              %u
            |=  [v=@ w=@]
            ^-  (unit @)
            ?:  =(0 w)  ~
            `(^div v w)
          ::
              %s
            =/  size=@  (lane-size type.i)
            =/  ill=@s  (new:si & (bex (dec size)))  ::  unrepresentable
            |=  [v=@ w=@]
            ^-  (unit @)
            ?:  =(0 w)  ~
            %-  bind  :_  (cury en-si size)
            %-  (flit |=(=@s !=(s ill)))
            %+  fra:si
              (to-si size v)
            (to-si size w)
          ==
        ::
        ==
      ::
      ++  rem
        |=  i=instruction
        ?>  ?=(%rem -.i)
        ?:  =(%u mode.i)
          |=  [v=@ w=@]
          ^-  (unit @)
          ?:  =(0 w)  ~
          `(mod v w)
        =/  base=@  ?-(type.i %i32 32, %i64 64)
        |=  [v=@ w=@]
        ^-  (unit @)
        ?:  =(0 w)  ~
        :-  ~
        %+  en-si  base
        %+  rem:si
          (to-si base v)
        (to-si base w)
      ::
      ++  and
        |=  *
        (mayb dis)  ::  ATTENTION! loobean disjunction is boolean conjunction
      ::
      ++  or
        |=  *
        (mayb con)  ::  ATTENTION! loobean conjunction is boolean disjunction
      ::
      ++  xor
        |=  *
        (mayb mix)
      ::
      ++  shl
        |=  i=instruction
        %-  mayb
        ?>  ?=(%shl -.i)
        =/  base=@   (lane-size type.i)
        |=  [v=@ w=@]
        ^-  @
        (mod (lsh [0 (mod w base)] v) (bex base))
      ::
      ++  shr
        |=  i=instruction
        %-  mayb
        ?>  ?=(%shr -.i)
        =/  base=@   (lane-size type.i)
        =/  negat=@  (bex (dec base))
        =/  negat-dec=@  (dec negat)
        |=  [v=@ w=@]
        ^-  @
        ?-    mode.i
            %u  (rsh [0 (mod w base)] v)
            %s
          ?:  (lth v negat)
            (rsh [0 (mod w base)] v)
          ;:  ^add
            negat
            (^sub negat-dec (dec (bex (^sub (dec base) (mod w base)))))
            (rsh [0 (mod w base)] (^sub v negat))
          ==
        ==
      ::
      ++  rotl
        |=  i=instruction
        %-  mayb
        ?>  ?=(%rotl -.i)
        =/  base=@  ?-(type.i %i32 32, %i64 64)
        =/  n=@  ?-(type.i %i32 5, %i64 6)
        |=  [v=@ w=@]
        ^-  @
        (~(rol fe n) 0 (mod w base) v)
      ::
      ++  rotr
        |=  i=instruction
        %-  mayb
        ?>  ?=(%rotr -.i)
        =/  base=@  ?-(type.i %i32 32, %i64 64)
        =/  n=@  ?-(type.i %i32 5, %i64 6)
        |=  [v=@ w=@]
        ^-  @
        (~(ror fe n) 0 (mod w base) v)
      ::
      ++  min
        |=  i=instruction
        %-  mayb
        ?>  ?=(%min -.i)
        =+  ^=  r
            ?-  type.i
              %f32  rs
              %f64  rd
            ==
        |=  [v=@ w=@]
        ^-  @
        =/  [fn-1=fn fn-2=fn]  [(sea:r v) (sea:r w)]
        ?:  =(%n -.fn-1)  v
        ?:  =(%n -.fn-2)  w
        ?:  ?&  ?=(%f -.fn-1)
                ?=(%f -.fn-2)
                =(0 a.fn-1)
                =(0 a.fn-2)
            ==
          (^max v w)  ::  return zero with negative sign if present
        ?:((lth:r v w) v w)
      ::
      ++  max
        |=  i=instruction
        %-  mayb
        ?>  ?=(%max -.i)
        =+  ^=  r
            ?-  type.i
              %f32  rs
              %f64  rd
            ==
        |=  [v=@ w=@]
        ^-  @
        =/  [fn-1=fn fn-2=fn]  [(sea:r v) (sea:r w)]
        ?:  =(%n -.fn-1)  v
        ?:  =(%n -.fn-2)  w
        ?:  ?&  ?=(%f -.fn-1)
                ?=(%f -.fn-2)
                =(0 a.fn-1)
                =(0 a.fn-2)
            ==
          (^min v w)  ::  return zero with positive sign if present
        ?:((gth:r v w) v w)
      ::
      ++  copysign
        |=  i=instruction
        %-  mayb
        ?>  ?=(%copysign -.i)
        =+  ^=  r
            ?-  type.i
              %f32  rs
              %f64  rd
            ==
        =/  sign=@   ?-(type.i %f32 (bex 31), %f64 (bex 63))
        =/  modul=@  ?-(type.i %f32 (bex 32), %f64 (bex 64))
        =/  wid=@    ?-(type.i %f32 31, %f64 63)
        |=  [v=@ w=@]
        ^-  @
        ?:  =((rsh [0 wid] v) (rsh [0 wid] w))  v
        (~(sum fo modul) v sign)
      ::
      ++  eq
        |=  i=instruction
        %-  mayb
        ?>  ?=(%eq -.i)
        ?.  ?=(?(%f32 %f64) type.i)
          |=  [v=@ w=@]
          ^-  @
          ?:(=(v w) 1 0)
        =+  ^=  r
            ?-  type.i
              %f32  rs
              %f64  rd
            ==
        |=  [v=@ w=@]
        ^-  @
        ?:  (equ:r v w)  1
        0
      ::
      ++  ne
        |=  i=instruction
        %-  mayb
        ?>  ?=(%ne -.i)
        ?.  ?=(?(%f32 %f64) type.i)
          |=  [v=@ w=@]
          ^-  @
          ?:(!=(v w) 1 0)
        =+  ^=  r
            ?-  type.i
              %f32  rs
              %f64  rd
            ==
        |=  [v=@ w=@]
        ^-  @
        ?:  !(equ:r v w)  1
        0
      ::
      ++  lt
        |=  i=instruction
        %-  mayb
        ?>  ?=(%lt -.i)
        =/  mode=?(%s %u)  (fall mode.i %u)
        =/  negat=@  ?+(type.i 0 %i32 (bex 31), %i64 (bex 63))
        |=  [v=@ w=@]
        ^-  @
        ?-    type.i
            %f32  ?:((lth:rs v w) 1 0)
            %f64  ?:((lth:rd v w) 1 0)
        ::
            *
          ?-    mode
              %u  ?:((lth v w) 1 0)
              %s
            ::  if both are positive or both a negative,
            ::  then comparison is simple
            ::
            ?:  ?|  &((lth v negat) (lth w negat))
                    &((gte v negat) (gte w negat))
                ==
              ?:((lth v w) 1 0)
            ::  otherwise v and w have different signs,
            ::  check which is negative
            ::
            ?:((gth v w) 1 0)
          ==
        ==
      ::
      ++  gt
        |=  i=instruction
        %-  mayb
        ?>  ?=(%gt -.i)
        |=  [v=@ w=@]
        ^-  @
        ?-    type.i
            %f32  ?:((gth:rs v w) 1 0)
            %f64  ?:((gth:rd v w) 1 0)
        ::
            *
          %-  need
          %.  [w v]
          (lt ;;(instruction [%lt +.i]))
        ==
      ::
      ++  le
        |=  i=instruction
        %-  mayb
        ?>  ?=(%le -.i)
        |=  [v=@ w=@]
        ^-  @
        ?-    type.i
            %f32  ?:((lte:rs v w) 1 0)
            %f64  ?:((lte:rd v w) 1 0)
        ::
            *
          ?:  =(v w)  1
          %-  need
          %.  [v w]
          (lt ;;(instruction [%lt +.i]))
        ==
      ::
      ++  ge
        |=  i=instruction
        %-  mayb
        ?>  ?=(%ge -.i)
        |=  [v=@ w=@]
        ^-  @
        ?-    type.i
            %f32  ?:((gte:rs v w) 1 0)
            %f64  ?:((gte:rd v w) 1 0)
        ::
            *
          %-  need
          %.  [w v]
          (le ;;(instruction [%le +.i]))
        ==
      ::
      --
    --
  ::
  ++  simd
    =<  fetch-vec
    |%
    ::  ++rope: ++rip but with leading zeros.
    ::  Takes bloq size, number of blocks and
    ::  an atom to dissasemble
    ::
    ++  rope
      |=  [b=bloq s=step a=@]
      ^-  (list @)
      ?:  =(s 0)  ~
      :-  (end b a)
      $(a (rsh b a), s (dec s))
    ::
    ++  fetch-vec
      |=  i=instr-vec
      ^-  $-(local-state local-state)
      ?+    -.i  (plain i)
          %load        (load i)
          %load-lane   (load-lane i)
          %store       (store i)
          %store-lane  (store-lane i)
      ::
          %const    (const i)
          %shuffle  (shuffle i)
          %extract  (extract i)
          %replace  (replace i)
      ::
      ==
    ::
    ++  load
      |=  i=instr-vec
      ?>  ?=(%load -.i)
      |=  l=local-state
      ^-  local-state
      ?>  ?=([addr=@ rest=*] va.stack.l)
      =,  va.stack.l
      =/  index=@  (add addr offset.m.i)
      =+  mem=(memo:grab 0 store.l)
      ?:  ?=(%| -.mem)
        %^  buy  l(va.stack rest)
          :*  -.p.mem
              %memo
              (change ~[%i32] ~[addr])
              [%vec i]
          ==
        ~[%v128]
      ?~  kind.i
        =+  loaded=(mem-load index 16 p.mem)
        ?~  loaded  l(br.stack [%trap ~])
        l(va.stack [u.loaded rest])
      ?-    q.u.kind.i
          [%extend p=?(%s %u)]
        =;  loaded=(unit @)
          ?~  loaded  l(br.stack [%trap ~])
          l(va.stack [u.loaded rest])
        =+  bloq=(xeb (dec p.u.kind.i))
        =+  get=(mem-load index 8 p.mem)
        ?~  get  ~
        =/  lanes=(list @)
          (rope bloq (div 64 p.u.kind.i) u.get)
        :-  ~
        %+  rep  +(bloq)
        ?:  ?=(%u p.q.u.kind.i)
          lanes
        %+  turn  lanes
        %+  cork  (cury to-si p.u.kind.i)
        (cury en-si (mul 2 p.u.kind.i))
      ::
          %zero
        =+  get=(mem-load index (div p.u.kind.i 8) p.mem)
        ?~  get  l(br.stack [%trap ~])
        l(va.stack [u.get rest])
      ::
          %splat
        =;  loaded=(unit @)
          ?~  loaded  l(br.stack [%trap ~])
          l(va.stack [u.loaded rest])
        =+  lane=(mem-load index (div p.u.kind.i 8) p.mem)
        ?~  lane  ~
        `(fil (xeb (dec p.u.kind.i)) (div 128 p.u.kind.i) u.lane)
      ::
      ==
    ::
    ++  load-lane
      |=  i=instr-vec
      ?>  ?=(%load-lane -.i)
      |=  l=local-state
      ^-  local-state
      ?>  ?=([vec=@ addr=@ rest=*] va.stack.l)
      =,  va.stack.l
      =/  index=@  (add addr offset.m.i)
      =+  mem=(memo:grab 0 store.l)
      ?:  ?=(%| -.mem)
        %^  buy  l(va.stack rest)
          :*  -.p.mem
              %memo
              (change ~[%i32 %v128] ~[addr vec])
              [%vec i]
          ==
        ~[%v128]
      =+  lane=(mem-load index (div p.i 8) p.mem)
      ?~  lane  l(br.stack [%trap ~])
      %=    l
          va.stack
        [(sew (xeb (dec p.i)) [l.i 1 u.lane] vec) rest]
      ==
    ::
    ++  store
      |=  i=instr-vec
      ?>  ?=(%store -.i)
      |=  l=local-state
      ^-  local-state
      ?>  ?=([vec=@ addr=@ rest=*] va.stack.l)
      =,  va.stack.l
      =+  memo=(memo:grab 0 store.l)
      ?:  ?=(%| -.memo)
        %^  buy  l(va.stack rest)
          :*  -.p.memo
              %memo
              (change ~[%i32 %v128] ~[addr vec])
              [%vec i]
          ==
        ~
      =/  index=@  (add addr offset.m.i)
      =+  mem-stored=(mem-store index 16 vec p.memo)
      ?~  mem-stored  l(br.stack [%trap ~])
      %=    l
          va.stack  rest
      ::
          mem.store
        `u.mem-stored
      ==
    ::
    ++  store-lane
      |=  i=instr-vec
      ?>  ?=(%store-lane -.i)
      |=  l=local-state
      ^-  local-state
      ?>  ?=([vec=@ addr=@ rest=*] va.stack.l)
      =,  va.stack.l
      =+  memo=(memo:grab 0 store.l)
      ?:  ?=(%| -.memo)
        %^  buy  l(va.stack rest)
          :*  -.p.memo
              %memo
              (change ~[%i32 %v128] ~[addr vec])
              [%vec i]
          ==
        ~
      =/  index=@  (add addr offset.m.i)
      =+  lane=(cut (xeb (dec p.i)) [l.i 1] vec)
      =+  mem-stored=(mem-store index (div p.i 8) lane p.memo)
      ?~  mem-stored  l(br.stack [%trap ~])
      %=    l
          va.stack  rest
      ::
          mem.store
        `u.mem-stored
      ==
    ::
    ++  const
      |=  i=instr-vec
      ?>  ?=(%const -.i)
      |=  l=local-state
      ^-  local-state
      l(va.stack [(coin-to-val p.i) va.stack.l])
    ::
    ++  shuffle
      |=  i=instr-vec
      ?>  ?=(%shuffle -.i)
      |=  l=local-state
      ^-  local-state
      ?>  ?=([c2=@ c1=@ rest=*] va.stack.l)
      =,  va.stack.l
      =/  seq=(list @)
        (weld (rope 3 16 c1) (rope 3 16 c2))
      %=    l
          va.stack
        :_  rest
        %+  rep  3
        %+  turn  lane-ids.i
        (curr snag seq)
      ==
    ::
    ++  extract
      |=  i=instr-vec
      ?>  ?=(%extract -.i)
      |=  l=local-state
      ^-  local-state
      ?>  ?=([vec=@ rest=*] va.stack.l)
      =,  va.stack.l
      =+  size=(lane-size p.i)
      =+  lane=(cut (xeb (dec size)) [l.i 1] vec)
      =;  to-put=@
        l(va.stack [to-put rest])
      ?:  ?=(%u mode.i)  lane
      (en-si 32 (to-si size lane))
    ::
    ++  replace
      |=  i=instr-vec
      ?>  ?=(%replace -.i)
      |=  l=local-state
      ^-  local-state
      ?>  ?=([lane=@ vec=@ rest=*] va.stack.l)
      =,  va.stack.l
      %=    l
          va.stack
        :_  rest
        (sew (xeb (dec (lane-size p.i))) [l.i 1 lane] vec)
      ==
    ::
    ++  plain
      |=  i=instr-vec
      ^-  $-(local-state local-state)
      ~+
      |^
      ?+    -.i  !!
          vec-unary:kind
        |=  l=local-state
        ^-  local-state
        ?>  ?=([a=@ rest=*] va.stack.l)
        =,  va.stack.l
        =+  val=((vec-unar i) a)
        ?~  val  l(br.stack [%trap ~])
        l(va.stack [val rest])
      ::
          vec-binary:kind
        |=  l=local-state
        ^-  local-state
        ?>  ?=([b=@ a=@ rest=*] va.stack.l)
        =,  va.stack.l
        =+  val=((vec-bina i) a b)
        ?~  val  l(br.stack [%trap ~])
        l(va.stack [val rest])
      ::
          lane-wise-unary:kind
        =/  op=$-(@ (unit @))  (get-op-unar i)
        =/  size=@  (get-size i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([vec=@ rest=*] va.stack.l)
        =,  va.stack.l
        =;  val=(unit @)
          ?~  val  l(br.stack [%trap ~])
          l(va.stack [u.val rest])
        ;<  =(list @)  _biff
          (torn (rope (xeb (dec size)) (div 128 size) vec) op)
        `(rep (xeb (dec size)) list)
      ::
          lane-wise-binary:kind
        =/  op=$-([@ @] (unit @))  (get-op-bina i)
        =/  size=@  (get-size i)
        |=  l=local-state
        ^-  local-state
        ?>  ?=([vec2=@ vec1=@ rest=*] va.stack.l)
        =,  va.stack.l
        =;  val=(unit @)
          ?~  val  l(br.stack [%trap ~])
          l(va.stack [u.val rest])
        ;<  =(list @)  _biff
          %-  torn  :_  op
          %+  fuse  (rope (xeb (dec size)) (div 128 size) vec1)
          (rope (xeb (dec size)) (div 128 size) vec2)
        `(rep (xeb (dec size)) list)
      ::
          %bitselect
        |=  l=local-state
        ^-  local-state
        ?>  ?=([vec3=@ vec2=@ vec1=@ rest=*] va.stack.l)
        =,  va.stack.l
        %=    l
          va.stack
          :_  rest
          ::  ++con is ior, ++dis is iand
          ::
          (con (dis vec1 vec3) (dis vec2 (not 7 1 vec3)))
        ==
      ==
      ::  |kind: instruction classes for SIMD instructions
      ::
      ++  kind
        |%
        +$  vec-unary
          $?  %splat  %not  %any-true  %all-true
              %bitmask  %extadd  %extend  %convert
              %demote  %promote
          ==
        ::
        +$  vec-binary
          $?  %swizzle  %and  %andnot  %or  %xor
              %narrow  %shl  %shr  %extmul  %dot
          ==
        ::
        +$  lane-wise-unary
          $?
            %abs  %neg  %popcnt  %ceil  %floor  %trunc
            %nearest  %sqrt
          ==
        ::
        +$  lane-wise-binary
          $?  %eq  %ne  %lt  %gt  %le  %ge  %add  %sub
              %min  %max  %avgr  %q15mul-r-sat  %mul  %div
              %pmin  %pmax
          ==
        --
      ::
      ++  vec-unar
        |:  i=`$>(vec-unary:kind instr-vec)`[%not ~]
        ^-  $-(@ @)
        ?-    -.i
            %splat
          =+  size=(lane-size p.i)
          =+  bloq=(xeb (dec size))
          =+  number=(div 128 size)
          |=  a=@
          ^-  @
          (fil bloq number a)
        ::
            %not
          |=  a=@
          ^-  @
          (not 7 1 a)
        ::
            %any-true
          |=  a=@
          ^-  @
          ?.(=(0 a) 1 0)
        ::
            %all-true
          =+  size=(lane-size p.i)
          =+  bloq=(xeb (dec size))
          =+  n=(div 128 size)
          |=  a=@
          ^-  @
          ?:  (lien (rope bloq n a) (curr test 0))
            0
          1
        ::
            %bitmask
          =+  size=(lane-size p.i)
          =+  bloq=(xeb (dec size))
          =+  n=(div 128 size)
          |=  a=@
          %+  rep  0
          %-  weld  :_  (reap (sub 32 n) `@`0)
          %+  turn
            (turn (rope bloq n a) (cury to-si size))
          |=  a=@s
          ^-  @
          ?:  =(-1 (cmp:si a --0))
            1
          0
        ::
            %extadd
          =+  half-size=(div (lane-size p.i) 2)
          =+  half-bloq=(xeb (dec half-size))
          =+  n=(div 128 half-size)
          |=  a=@
          =/  lanes-extend=(list @)
            =+  lanes=(rope half-bloq n a)
            ?:  ?=(%u mode.i)  lanes
            %+  turn  lanes
            %+  cork  (cury to-si half-size)
            (cury en-si (mul 2 half-size))
          %+  rep  +(half-bloq)
          %-  turn  :_  ~(sum fo (bex (mul 2 half-size)))
          %+  fuse  (scag (div n 2) lanes-extend)
          (slag (div n 2) lanes-extend)
        ::
            %extend
          =+  half-size=(div (lane-size p.i) 2)
          =+  half-bloq=(xeb (dec half-size))
          =+  n=(div 128 half-size)
          |=  a=@
          =;  lanes-extend=(list @)
            (rep +(half-bloq) lanes-extend)
          =+  lanes=(rope half-bloq n a)
          =.  lanes
            ?:  =(%low half.i)
              (scag (div n 2) lanes)
            (slag (div n 2) lanes)
          ?:  =(%u mode.i)  lanes
          %+  turn  lanes
          %+  cork  (cury to-si half-size)
          (cury en-si (mul 2 half-size))
        ::
            %convert
          =+  size=(lane-size p.i)
          =+  bloq=(xeb (dec size))
          =+  ^=  r
            ?-  p.i
              %f32  rs
              %f64  rd
            ==
          |=  a=@
          =+  lanes=(rope 5 4 a)
          =?  lanes  =(%f64 p.i)  (scag 2 lanes)
          %+  rep  bloq
          %+  turn  lanes
          ?:  ?=(%u mode.i)  sun:r
          (cork san:r (cury to-si size))
        ::
            %demote
          |=  a=@
          =+  lanes=(rope 6 2 a)
          %+  rep  5
          %-  weld  :_  (reap 2 `@`0)
          (turn lanes (corl bit:rs sea:rd))
        ::
            %promote
          |=  a=@
          =+  half-lanes=(rope 5 2 a)
          %+  rep  5
          (turn half-lanes (corl bit:rd sea:rs))
        ::
        ==
      ::
      ++  vec-bina
        |:  i=`$>(vec-binary:kind instr-vec)`[%and ~]
        ^-  $-([@ @] @)
        ?-    -.i
            %swizzle
          |=  [a=@ b=@]
          ^-  @
          %+  rep  3
          %+  turn  (rope 3 16 b)
          %+  corl  (curr fall 0)
          (curr snug (rope 3 16 a))
        ::
            %and
          dis
        ::
            %andnot
          |=  [a=@ b=@]
          ^-  @
          (dis a (not 7 1 b))
        ::
            %or
          con
        ::
            %xor
          mix
        ::
            %narrow
          =+  double-size=(mul 2 (lane-size p.i))
          =+  double-bloq=(xeb (dec double-size))
          =+  n=(div 128 double-size)
          |=  [a=@ b=@]
          ^-  @
          %+  rep  (dec double-bloq)
          %+  turn
            %+  weld  (rope double-bloq n a)
            (rope double-bloq n b)
          %+  cork  (cury to-si double-size)
          (curr (cury sat (div double-size 2)) mode.i)
        ::
            %shl
          =+  size=(lane-size p.i)
          =+  bloq=(xeb (dec size))
          =+  n=(div 128 size)
          =+  shl=(sure (bina:fetch [%shl p.i]))
          |=  [vec=@ j=@]
          ^-  @
          %+  rep  bloq
          %+  turn
            (rope bloq n vec)
          (curr shl j)
        ::
            %shr
          =+  size=(lane-size p.i)
          =+  bloq=(xeb (dec size))
          =+  n=(div 128 size)
          =+  shr=(sure (bina:fetch [%shr p.i mode.i]))
          |=  [vec=@ j=@]
          ^-  @
          %+  rep  bloq
          %+  turn
            (rope bloq n vec)
          (curr shr j)
        ::
            %extmul
          =+  half-size=(div (lane-size p.i) 2)
          =+  half-bloq=(xeb (dec half-size))
          =+  n=(div 128 half-size)
          =+  mode=mode.i
          |=  [a=@ b=@]
          =/  half-lanes-a=(list @)
            =-  ?:  =(%u mode)  -
                %+  turn  -
                %+  cork  (cury to-si half-size)
                (cury en-si (mul 2 half-size))
            %.  [(div n 2) (rope half-bloq n a)]
            ?:  =(%low half.i)  scag
            slag
          =/  half-lanes-b=(list @)
            =-  ?:  =(%u mode)  -
                %+  turn  -
                %+  cork  (cury to-si half-size)
                (cury en-si (mul 2 half-size))
            %.  [(div n 2) (rope half-bloq n b)]
            ?:  =(%low half.i)  scag
            slag
          %+  rep  +(half-bloq)
          %+  turn
            (fuse half-lanes-a half-lanes-b)
          ~(pro fo (mul 2 half-size))
        ::
            %dot
          |=  [a=@ b=@]
          =/  lanes-a=(list @)
            %+  turn  (rope 4 8 a)
            %+  cork  (cury to-si 16)
            (cury en-si 32)
          =/  lanes-b=(list @)
            %+  turn  (rope 4 8 b)
            %+  cork  (cury to-si 16)
            (cury en-si 32)
          =/  i1-i2=(list @)
            (turn (fuse lanes-a lanes-b) ~(pro fo 32))
          %+  rep  5
          %+  turn
            (fuse (scag 4 i1-i2) (slag 4 i1-i2))
          ~(sum fo ^~((bex 32)))
        ::
        ==
      ::
      ++  get-op-unar
        |:  i=`$>(lane-wise-unary:kind instr-vec)`[%popcnt ~]
        ^-  $-(@ (unit @))
        ?-    -.i
            %abs
          (unar:fetch [%abs p.i])
        ::
            %neg
          (unar:fetch [%neg p.i])
        ::
            %popcnt
          (unar:fetch [%neg %i8])
        ::
            %ceil
          (unar:fetch [%ceil p.i])
        ::
            %floor
          (unar:fetch [%floor p.i])
        ::
            %trunc
          =/  mode=(unit ?(%s %u))
            ?:  ?=(?(%f32 %f64) p.i)  ~
            `mode.i
          (unar:fetch [%trunc p.i `from.i mode &])
        ::
            %nearest
          (unar:fetch [%nearest p.i])
        ::
            %sqrt
          (unar:fetch [%sqrt p.i])
        ::
        ==
      ::
      ++  get-op-bina
        |:  i=`$>(lane-wise-binary:kind instr-vec)`[%q15mul-r-sat ~]
        ^-  $-([@ @] (unit @))
        ?-    -.i
            %eq
          (bina:fetch [%eq p.i])
        ::
            %ne
          (bina:fetch [%ne p.i])
        ::
            %lt
          (bina:fetch [%lt p.i `mode.i])
        ::
            %gt
          (bina:fetch [%gt p.i `mode.i])
        ::
            %le
          (bina:fetch [%le p.i `mode.i])
        ::
            %ge
          (bina:fetch [%ge p.i `mode.i])
        ::
            %add
          ?~  sat.i
            (bina:fetch [%add p.i])
          =+  size=(lane-size p.i)
          %-  mayb
          |=  [a=@ b=@]
          ^-  @
          %+  sat  size
          :_  u.sat.i
          =,  si
          (sum (to-si size a) (to-si size b))
        ::
            %sub
          ?~  sat.i
            (bina:fetch [%sub p.i])
          =+  size=(lane-size p.i)
          %-  mayb
          |=  [a=@ b=@]
          ^-  @
          %+  sat  size
          :_  u.sat.i
          =,  si
          (dif (to-si size a) (to-si size b))
        ::
            %min
          ?:  ?=(?(%f32 %f64) p.i)
            (bina:fetch [%min p.i])
          ?:  =(%u mode.i)  (mayb min)
          =+  size=(lane-size p.i)
          %-  mayb
          |=  [a=@ b=@]
          ?:  =(--1 (cmp:si (to-si size a) (to-si size b)))
            b
          a
        ::
            %max
          ?:  ?=(?(%f32 %f64) p.i)
            (bina:fetch [%max p.i])
          ?:  =(%u mode.i)  (mayb max)
          =+  size=(lane-size p.i)
          %-  mayb
          |=  [a=@ b=@]
          ?:  =(-1 (cmp:si (to-si size a) (to-si size b)))
            b
          a
        ::
            %avgr
          %-  mayb
          |=  [a=@ b=@]
          ^-  @
          (div :(add a b 1) 2)
        ::
            %q15mul-r-sat
          %-  mayb
          =+  sign-bit=(bex 15)
          =+  base=(bex 16)
          =+  size=16
          |=  [a=@ b=@]
          ^-  @
          =/  sign=?  &
          =?  .  (gte a sign-bit)
            .(a (~(sum fo base) (not 4 1 a) 1), sign !sign)
          =?  .  (gte b sign-bit)
            .(b (~(sum fo base) (not 4 1 b) 1), sign !sign)
          %+  sat  size
          :_  %s
          %+  new:si  sign
          (rsh [0 15] (add (mul a b) ^~((bex 14))))
        ::
            %mul
          (bina:fetch [%mul p.i])
        ::
            %div
          (bina:fetch [%div p.i ~])
        ::
            %pmin
          =+  flt=(sure (bina:fetch [%lt p.i ~]))
          %-  mayb
          |=  [a=@ b=@]
          ^-  @
          ?:  =(1 (flt b a))  b
          a
        ::
            %pmax
          =+  fgt=(sure (bina:fetch [%gt p.i ~]))
          %-  mayb
          |=  [a=@ b=@]
          ^-  @
          ?:  =(1 (fgt b a))  b
          a
        ::
        ==
      ::  ++get-size: get size of a line-wise simd instruction.
      ::  simd instructions that we care about either
      ::  have the size at +2, or +6, or we set the size
      ::  manually in the case of %q15mul-r-sat and others
      ::
      ++  get-size
        |=  i=^
        ^-  @
        ?+    +.i  !!
            ~
          ?+  -.i  !!
            %q15mul-r-sat  16
            %popcnt  8
          ==
        ::
            lane-type
          (lane-size +.i)
        ::
            [p=lane-type *]
          (lane-size p.i)
        ==
      --
    ::
    --
  --
--  ::  |op-def