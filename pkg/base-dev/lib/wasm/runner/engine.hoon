::  Web Assembly AST interpreter
::
/+  wasm-runner-op-def
=>  wasm-runner-op-def
~%  %wasm-engine-v0  +  ~
|%
++  engine
  ::
  =,  engine-sur
  =,  op-def
  |%
  ::  +get-types: turn a (list coin-wasm) into a list of types of coins
  ::
  ++  get-types
    |=  a=(list coin-wasm)
    ^-  (list valtype)
    %+  turn  a
    |=  c=coin-wasm
    ^-  valtype
    ?.  ?=([%ref r=*] c)
      -.c
    -.r.c
  ::  +mint:  get stack values from valtypes, used for locals
  ::
  ++  mint
    |=  a=(list valtype)
    ^-  (list val)
    ?~  a  ~
    :_  $(a t.a)
    ^-  val
    ?-  i.a
      ?(num-type vec-type)  *@
      ref-type  :-  %ref
                ?-  i.a
                  %extn  [%extn ~]
                  %func  [%func ~]
    ==          ==
  ::  +make-export-map: turns export-section into a map [name=cord =export-desc]
  ::
  ++  make-export-map
    |=  =export-section
    =|  out=(map cord export-desc)
    |-  ^+  out
    ?~  export-section  out
    =,  i.export-section
    %=  $
      out  (~(put by out) name export-desc)
      export-section  t.export-section
    ==
  ::  +find-func-id: find func-id from a name of an exported function
  ::
  ++  find-func-id
    |=  [name=cord =module]
    ^-  @
    =,  module
    =/  =export-desc
      (~(got by (make-export-map export-section)) name)
    ?>  ?=(%func -.export-desc)
    i.export-desc
  ::
  ++  conv
    |=  [m=^module sh=shop]
    ^-  store
    =|  st=store
    =.  shop.st  sh
    |^
    =.  module.st
      =,  m
      :*
        type-section
        (import-upd import-section)
        (fuse function-section code-section)
        table-section
        memory-section
        global-section
        export-section
        start-section
        elem-section
        data-section
      ==
    st
    ::
    ++  import-upd
      |=  i=^import-section
      =|  out=import-section
      |-  ^-  import-section
      ?~  i
        =,  out
        %_  out
          funcs   (flop funcs)
          tables  (flop tables)
          memos   (flop memos)
          globs   (flop globs)
        ==
      =.  out
        =,  i.i
        =,  out
        ?-  -.desc.i.i
          %func  out(funcs [[[mod name] type-id.desc] funcs])
          %tabl  out(tables [[[mod name] t.desc] tables])
          %memo  out(memos [[[mod name] l.desc] memos])
          %glob  out(globs [[[mod name] +.desc] globs])
        ==
      $(i t.i)
    ::
    --
  ::  +prep: instantiate Wasm module. Returns global state
  ::  after the validation and import handling,
  ::  instantiation of table, globals and the membuffer,
  ::  and running the start function
  ::
  ::  Attention: it takes a parsed module, but the store
  ::  will contain engine version, with locals added and
  ::  some superfluous sections removed
  ::
  ++  prep
    |=  [m=^module sh=shop]
    ^-  result
    (instantiate (conv m sh))
  ::
  ++  instantiate
    |=  st=store
    ^-  result
    |^
    ;<  [* st=store]  _wasm-bind  (init-globals st)  =>  +(st st)
    ;<  [* st=store]  _wasm-bind  (init-table st)    =>  +(st st)
    ;<  [* st=store]  _wasm-bind  (init-elems st)    =>  +(st st)
    ;<  [* st=store]  _wasm-bind  (init-mem st)      =>  +(st st)
    ;<  [* st=store]  _wasm-bind  (init-data st)     =>  +(st st)
    ;<  [* st=store]  _wasm-bind  (start st)         =>  +(st st)
    [%0 ~ st]
    ::
    ++  init-globals
      |=  st=store
      =/  m=module  module.st
      |-  ^-  result
      ?~  global-section.m
        [%0 ~ st(globals (flop globals.st))]
      =*  glob  i.global-section.m
      ::  Const globals
      ::
      ?:  ?=([%const p=$<(?(%v128 %ref) coin-wasm)] i.glob)
        %=  $
          global-section.m  t.global-section.m
          globals.st  [p.i.glob globals.st]
        ==
      ?:  ?=([%vec %const p=$>(%v128 coin-wasm)] i.glob)
        %=  $
          global-section.m  t.global-section.m
          globals.st  [p.i.glob globals.st]
        ==
      ?:  ?=([%ref-null t=ref-type] i.glob)
        =/  null-ref=coin-wasm
          ?:  ?=(%func t.i.glob)  [%ref %func ~]
          [%ref %extn ~]
        %=  $
          global-section.m  t.global-section.m
          globals.st  [null-ref globals.st]
        ==
      ?:  ?=([%ref-func func-id=@] i.glob)
        %=  $
          global-section.m  t.global-section.m
          globals.st  [[%ref %func `func-id.i.glob] globals.st]
        ==
      ::  Imported globals. We assume here that %global-get
      ::  would not affect module store
      ::
      ?^  shop.st
        %=  $
          global-section.m  t.global-section.m
          globals.st  [-.p.i.shop.st globals.st]
          shop.st  t.shop.st
        ==
      :+  %1
        :-  -:(snag index.i.glob globs.import-section.module.st)
        [%glob ~ i.glob]
      [*module ~ ~ ~]
    ::
    ++  init-table
      |=  st=store
      =/  m=module  module.st
      |-  ^-  result
      :+  %0  ~
      |-  ^-  store
      ?~  table-section.m
        st(tables (flop tables.st))
      =*  tab  i.table-section.m
      %=    $
          table-section.m  t.table-section.m
      ::
          tables.st
        :_  tables.st
        (reap (lim-min q.tab) [%ref %func ~])
      ==
    ::
    ++  init-elems
      |=  st=store
      =/  m=module  module.st
      |-  ^-  result
      :+  %0  ~
      |-  ^-  store
      ?~  elem-section.m  st
      =*  elem  i.elem-section.m
      ?.  ?=(%acti -.m.elem)
        $(elem-section.m t.elem-section.m)
      ::  Assert: only %func ref tables can be
      ::  initialized with an element segment
      ::
      ?>  ?=(%func t.elem)
      ::  Assert: i32 value in the offset
      ::  (it theoretically can be a %global-get of import, revisit later?)
      ::
      =/  offset=(unit @)
        ?:  ?=([%const %i32 n=@] off.m.elem)  `n.p.off.m.elem
        ?.  ?=(%global-get -.off.m.elem)  ~
        =+  glob=(glob:grab index.off.m.elem st)
        ?:  ?=(%| -.glob)  ~
        =/  coin=coin-wasm  q.p.glob
        ?:  ?=(%i32 -.coin)  `+.coin
        ~
      ?>  ?=(^ offset)
      =/  tab-loc-id=@
        %+  sub  tab.m.elem                     ::  Assert: elems are instantiated locally
        (lent tables.import-section.module.st)  ::  (to revisit?)
      %=    $
          elem-section.m  t.elem-section.m
      ::
          tables.st
        %^  shot  tables.st  tab-loc-id
        %^  place  (snag tab-loc-id tables.st)  ::  table to change
          u.offset                              ::  offset
        %+  turn  i.elem
        |=  in=instruction
        ^-  $>(%ref coin-wasm)
        ?>  ?=([%ref-func @] in)  ::  Assert: %func refs only
        [%ref %func `func-id.in]
      ::
      ==
    ::
    ++  init-mem
      |=  st=store
      =/  m=module  module.st
      |-  ^-  result
      :+  %0  ~
      ?~  memory-section.m  st
      ?>  ?=(@ t.memory-section.m)  ::  Assert: single memory
      =*  mem  i.memory-section.m
      st(mem `[0 (lim-min mem)])
    ::
    ++  init-data
      |=  st=store
      =/  m=module  module.st
      =+  id=`@`0
      |-  ^-  result
      ?~  data-section.m  [%0 ~ st]
      =*  data  i.data-section.m
      ?.  ?=(%acti -.data)
        $(data-section.m t.data-section.m, id +(id))
      ::  Assert: const i32 value as offset
      ::  (it theoretically can be a %global-get of import, revisit later?)
      ::
      =/  offset=(unit @)
        ?:  ?=([%const %i32 n=@] off.data)  `n.p.off.data
        ?.  ?=(%global-get -.off.data)  ~
        =+  glob=(glob:grab index.off.data st)
        ?:  ?=(%| -.glob)  ~
        =/  coin=coin-wasm  q.p.glob
        ?:  ?=(%i32 -.coin)  `+.coin
        ~
      ?>  ?=(^ offset)
      =/  l=local-state
        [[~ ~[-.b.data 0 u.offset]] ~ st]
      =.  l  ((fetch-gate [%memory-init id %0]) l)
      ?^  br.stack.l
        ?+  br.stack.l  !!
          [%bloq p=*]  [%1 p.br.stack.l]
          [%trap ~]    [%2 store.l]
        ==
      %=    $
          id  +(id)
          data-section.m  t.data-section.m
          st  store.l
      ==
    ::
    ++  start
      |=  st=store
      ^-  result
      =/  m=module  module.st
      ?~  start-section.m  [%0 ~ st]
      =/  [=stack * st-out=store]
        (call u.start-section.m [[~ ~] ~ st])
      ?+  br.stack  !!
        ~            [%0 ~ st-out]
        [%bloq p=*]  [%1 p.br.stack]
        [%trap ~]    [%2 st-out]
      ==
    --
  ::
  ++  wasm-need
    |=  a=result
    ^-  (quip coin-wasm store)
    ?>  ?=(%0 -.a)
    +.a
  ::
  ++  wasm-bind
    |=  [a=result b=$-((quip coin-wasm store) result)]
    ^-  result
    ?.  ?=(%0 -.a)  a
    (b +.a)
  ::
  ::  +invoke: call function by name, to call from the outside
  ::
  ++  invoke
    |=  [name=cord in=(list coin-wasm) st=store]
    ^-  result
    =/  id=@  (find-func-id name module.st)
    (invoke-id id in st)
  ::  ++invoke-id: call function by id, to call from the outside
  ::
  ++  invoke-id
    |=  [id=@ in=(list coin-wasm) st=store]
    ^-  result
    =/  id-local=@
      (sub id (lent funcs.import-section.module.st))
    ::  Type check for the input values
    ::
    =,  module.st
    =/  =func-type
      =/  func  (func:grab id st)
      ?:  ?=(%& -.func)  (snag type-id.p.func type-section)
      (snag type-id.p.func type-section)
    ?>  =(params.func-type (get-types in))
    =/  [stack-out=stack * st-out=store]
      %+  call  id
      ^-  local-state
      :+  stack=[~ (turn (flop in) coin-to-val)]
        locals=~
      store=st
    ?+  br.stack-out  !!
      ~  :+  %0
          (change results.func-type (flop va.stack-out))
        st-out
      [%bloq p=*]  [%1 p.br.stack-out]
      [%trap ~]    [%2 st-out]
    ==
  ::  +call: call function by id, inside caller
  ::
  ++  call
    |=  [id=@ l=local-state]
    ^-  local-state
    =,  module.store.l
    =+  f=(func:grab id store.l)  ::  (each function [[mod=cord name=cord] type-id=@])
    =/  type-id=@  =>(f ?:(?=(%& -) type-id.p type-id.p))
    =/  =func-type  (snag type-id type-section)
    ::  import case
    ::
    ?:  ?=(%| -.f)
      %^  buy  l(va.stack (slag (lent params.func-type) va.stack.l))
        :+  -.p.f  %func
        %+  change  params.func-type
        %-  flop
        (scag (lent params.func-type) va.stack.l)
      results.func-type
    ::  local case
    ::  take input values
    ::
    =/  input-values=(pole val)
      %-  flop
      (scag (lent params.func-type) va.stack.l)
    ::  save the rest of the stack and our locals
    ::
    =/  rest-vals=(pole val)
      (slag (lent params.func-type) va.stack.l)
    =/  our-locs=(list val)  locals.l
    ::  update local state
    ::
    =.  l
      %+  eval  expression.p.f
      ^-  local-state
      :+  stack=[~ ~]
        locals=(weld input-values (mint locals.p.f))
      store=store.l
    ::  If trap or bloq: forward
    ::
    ?:  ?=([?(%trap %bloq) *] br.stack.l)  l
    ::  Assert: no branch or branch with label 0 or return
    ::
    ?>  ?|  ?=(~ br.stack.l)
            ?=([%retr ~] br.stack.l)
            ?=([%targ %0] br.stack.l)
        ==
    ::  If return or targeted, take appropriate amount of vals from the stack
    ::
    =?  va.stack.l  ?|  ?=([%retr ~] br.stack.l)
                        ?=([%targ %0] br.stack.l)
                    ==
      (scag (lent results.func-type) va.stack.l)
    ::  Push returned values on stack, bring back locals, empty out br
    ::
    %=  l
      va.stack  (weld va.stack.l rest-vals)
      locals    our-locs
      br.stack  ~
    ==
  ::  +eval: evaluate an expression, mutating local state
  ::
  ++  eval
    |=  [e=expression l=local-state]
    ^-  local-state
    ?:  |(=(~ e) !=(~ br.stack.l))  ::  if navigating branch
      l                             ::  jump to the end of expression
    $(l (apply -.e l), e +.e)
  ::  +dec-br: "decrement" branch. Preserve absolute branching
  ::  coordinates (trap, return, block), and safely decrement
  ::  relative target index. Used for code flow navigation
  ::
  ++  dec-br
    |=  br=branch
    ^-  branch
    ?+  br  br
      [%targ %0]   ~
      [%targ i=@]  [%targ (dec i.br)]
    ==
  ::  +apply: apply an instruction, mutating local state
  ::  Only recursive control instructions are listed here. The rest
  ::  are returned by fetch-gate:op-def as gates $-(local-state local-state)
  ::  and applied to `l`
  ::
  ++  apply
    |=  [i=instruction l=local-state]
    ^-  local-state
    !.
    ?+    i  ((fetch-gate i) l)
        [%call func-id=@]
      (call func-id.i l)
    ::
        [%block *]  ::  [%block type=block-type body=expression]
      =/  n-params=@
        %-  lent
        ?^  type.i  params.type.i
        params:(snag type.i type-section.module.store.l)
      ::  save the current frame
      ::
      =/  rest-vals=(pole val)  (slag n-params va.stack.l)
      ::  execute the block
      ::
      =.  l  (eval body.i l(va.stack (scag n-params va.stack.l)))
      ::  If the block was targeted, pop appropriate amount of vals
      ::
      =?  va.stack.l  ?=([%targ %0] br.stack.l)
        =;  n-results=@
          (scag n-results va.stack.l)
        %-  lent
        ?^  type.i  results.type.i
        results:(snag type.i type-section.module.store.l)
      ::  Exit the block, navigate branch
      ::
      l(va.stack (weld va.stack.l rest-vals), br.stack (dec-br br.stack.l))
    ::
        [%loop *]  ::  [%loop type=block-type body=expression]
      |-  ^-  local-state  ::  not strictly necessary, but prevents from matching `i` again
      =/  n-params=@
        %-  lent
        ?^  type.i  params.type.i
        params:(snag type.i type-section.module.store.l)
      ::  save the current frame
      ::
      =/  rest-vals=(pole val)  (slag n-params va.stack.l)
      ::  execute the block
      ::
      =.  l  (eval body.i l(va.stack (scag n-params va.stack.l)))
      ::  If the loop was targeted, pop appropriate amount of vals,
      ::  push them on the stack, clear branching signal and jump to start
      ::
      ?:  ?=([%targ %0] br.stack.l)
        %=  $
          va.stack.l  (weld (scag n-params va.stack.l) rest-vals)
          br.stack.l  ~
        ==
      ::  Exit the block, navigate branch
      ::
      l(va.stack (weld va.stack.l rest-vals), br.stack (dec-br br.stack.l))
    ::
        [%call-indirect type-id=@ table-id=@]
      =,  module.store.l
      ?>  ?=([ref-id=@ rest=*] va.stack.l)
      =,  va.stack.l
      =+  tab=(table:grab table-id.i store.l)  ::  (each (list $>(%ref coin-wasm)) [[mod=cord name=cord] t=table])
      ::  import table
      ::
      ?:  ?=(%| -.tab)
        =/  =func-type  (snag type-id.i type-section)
        =/  input=(list coin-wasm)
          %+  change  params.func-type
          (flop (scag (lent params.func-type) rest))
        %^  buy  l(va.stack (slag (lent params.func-type) rest))
          [-.p.tab %tabl (weld input (change ~[%i32] ~[ref-id])) i]
        results.func-type
      ::  local table
      ::
      ::  Type check of reference
      ::
      =/  type-in-instr=func-type  (snag type-id.i type-section)
      =+  ref=(snag ref-id q.p.tab)
      =/  type-of-ref=(unit func-type)
        ?+    ref  ~
            [%ref %func p=[~ @]]
          =;  =func-type
            `func-type
          =/  func  (func:grab u.p.ref store.l)
          ?:  ?=(%& -.func)  (snag type-id.p.func type-section)
          (snag type-id.p.func type-section)
        ::
            [%ref %extn p=^]
          `type.u.p.ref
        ==
      ?~  type-of-ref  l(br.stack [%trap ~])
      ?.  =(type-in-instr u.type-of-ref)
        l(br.stack [%trap ~])
      ::  local func reference
      ::
      ?:  ?=([%ref %func p=[~ @]] ref)
        (call u.p.ref l(va.stack rest))
      ::  external func reference
      ::
      ?.  ?=([%ref %extn p=^] ref)
        l(br.stack [%trap ~])
      %^  buy  l(va.stack (slag (lent params.type.u.p.ref) rest))
        :-  -.u.p.ref
        =*  params  params.type.u.p.ref
        [%func (change params (flop (scag (lent params) rest)))]
      results.type.u.p.ref
    ::
        [%if *]  ::  [%if type=block-type branch-true=expression branch-false=expression]
      ?>  ?=([f=@ rest=*] va.stack.l)
      =,  va.stack.l
      ?.  =(0 f)
        $(i [%block type branch-true]:i, va.stack.l rest)
      $(i [%block type branch-false]:i, va.stack.l rest)
    ::
    ==
  --
--  ::  |engine