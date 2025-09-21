/+  wasm-parser
=>  wasm-parser
~%  %validator-v0  +  ~
|%
++  validator
  =,  wasm-sur
  |%
  +$  glob-type  [v=valtype m=?(%con %var)]
  +$  glob-types  (list glob-type)
  ++  output
    |%
    +$  import
      $:
        funcs=(list func-type)  ::  flopped
        tables=(list table)  ::  flopped
        memo=(unit limits)
        globs=glob-types  ::  flopped
      ==
    --
  ::
  +$  store  import:output  ::  right order
  ::
  ++  result-form
    |$  [mold]
    (each mold cord)
  ::
  ++  result
    |*  m2=mold
    |%
    +$  form  $+(form (result-form m2))
    ++  bind
      |*  m1=mold
      |=  [a=(result-form m1) b=$-(m1 form)]
      ^-  form
      ?:  ?=(%| -.a)  a
      (b p.a)
    ::
    --
  ::
  ++  snug
    |=  where=cord
    |*  [a=@ b=(list)]
    =/  r  (result _?>(?=(^ b) i.b))
    |-  ^-  form:r
    ?~  b  |+(cat 3 'index error in ' where)
    ?:  =(a 0)  &+i.b
    $(a (dec a), b t.b)
  ::
  ++  validate-module
    |=  m=module
    =/  r  (result ,~)
    ^-  form:r
    ;<  import-out=import:output  bind:r  (v-import-section m)
    =/  n-funcs-import=@  (lent funcs.import-out)
    ;<  functypes=(list func-type)  bind:r
      (v-function-section m funcs.import-out)
    ;<  tables=(list table)  bind:r  (v-table-section m tables.import-out)
    ;<  memo=(unit limits)  bind:r  (v-memory-section m memo.import-out)
    =/  n-funcs=@  (lent functypes)
    =/  n-tables=@  (lent tables)
    ;<  =glob-types  bind:r
      (v-global-section m globs.import-out n-funcs)
    =/  =store  [functypes tables memo glob-types]
    ;<  ~  bind:r
      %:  v-export-section
        m
        n-funcs
        n-tables
        ?^(memo 1 0)
        (lent glob-types)
      ==
    ;<  ~  bind:r  (v-start-section m functypes)
    ;<  ~  bind:r  (v-elem-section m n-tables functypes store)
    ;<  datacnt=(unit @)  bind:r  (v-datacnt-section m)
    ;<  ~  bind:r  (v-code-section m n-funcs-import store)
    (v-data-section m datacnt store)
  ::
  ++  v-import-section
    |=  m=module
    =|  out=import:output
    =/  num-types=@  (lent type-section.m)
    =/  r  (result import:output)
    |-  ^-  form:r
    ?~  import-section.m
      &+out
    ?-    -.desc.i.import-section.m
        %func
      =/  idx=@  type-id.desc.i.import-section.m
      ;<  type=func-type  bind:r  ((snug 'import functype') idx type-section.m)
      %=  $
        import-section.m  t.import-section.m
        funcs.out  [type funcs.out]
      ==
    ::
        %tabl
      ?.  (validate-limits q.t.desc.i.import-section.m)
        |+'invalid limits import table'
      =/  =table  t.desc.i.import-section.m
      $(import-section.m t.import-section.m, tables.out [table tables.out])
    ::
        %memo
      ?.  (validate-limits l.desc.i.import-section.m)
        |+'invalid limits import memo'
      ?^  memo.out  |+'multiple memos'
      %=  $
        import-section.m  t.import-section.m
        memo.out  `l.desc.i.import-section.m
      ==
    ::
        %glob
      %=  $
        import-section.m  t.import-section.m
        globs.out  [+.desc.i.import-section.m globs.out]
      ==
    ==
  ::
  ++  validate-limits
    |=  l=limits
    ^-  ?
    ?-  -.l
      %flor  &
      %ceil  (gte q.l p.l)
    ==
  ::
  ++  v-function-section
    |=  [m=module functypes-import=(list func-type)]
    =/  functypes=(list func-type)  functypes-import
    =/  r  (result (list func-type))
    |-  ^-  form:r
    ?~  function-section.m  &+(flop functypes)
    =/  idx=@  i.function-section.m
    ;<  type=func-type  bind:r  ((snug 'local functype') idx type-section.m)
    %=  $
      function-section.m  t.function-section.m
      functypes  [type functypes]
    ==
  ::
  ++  v-table-section
    |=  [m=module tables=(list table)]
    =/  r  (result (list table))
    ^-  form:r
    ?~  table-section.m  &+(flop tables)
    ?.  (validate-limits q.i.table-section.m)  |+'invalid limits local table'
    $(table-section.m t.table-section.m, tables [i.table-section.m tables])
  ::
  ++  v-memory-section
    |=  [m=module memo=(unit limits)]
    =/  r  (result (unit limits))
    ^-  form:r
    =/  len-memos=@  (lent memory-section.m)
    ?:  (gth len-memos 1)  |+'multiple memos'
    ?:  &(?=(^ memo) (gth len-memos 0))  |+'multiple memos'
    ?^  memo  &+memo
    ?:  =(len-memos 0)  &+~
    =/  lim=limits  -.memory-section.m
    ?.  (validate-limits lim)  |+'invalid limits local memory'
    ?:  &(?=(%ceil -.lim) (gth q.lim (bex 16)))  |+'mem limit too big'
    &+`-.memory-section.m
  ::
  ++  v-global-section
    |=  [m=module gt=glob-types n-funcs=@]
    =/  n-glob-import=@  (lent gt)
    =/  r  (result glob-types)
    |-  ^-  form:r
    ?~  global-section.m  &+(flop gt)
    =/  glob  i.global-section.m
    ?-    -.i.glob
        %const
      ?.  =(v.glob -.p.i.glob)  |+'global type mismatch'
      $(global-section.m t.global-section.m, gt [[v m]:glob gt])
    ::
        %vec
      ?.  ?=(%v128 v.glob)  |+'global type mismatch'
      $(global-section.m t.global-section.m, gt [[v m]:glob gt])
    ::
        %ref-null
      $(global-section.m t.global-section.m, gt [[v m]:glob gt])
    ::
        %ref-func
      ?:  (gte func-id.i.glob n-funcs)  |+'invalid funcref'
      $(global-section.m t.global-section.m, gt [[v m]:glob gt])
    ::
        %global-get
      ?:  (gte index.i.glob n-glob-import)
        |+'non-import or nonexisting const global initializer'
      $(global-section.m t.global-section.m, gt [[v m]:glob gt])
    ==
  ::
  ++  v-export-section
    |=  [m=module n-funcs=@ n-tables=@ n-memos=@ n-globs=@]
    =|  names=(set cord)
    =/  r  (result ,~)
    |-  ^-  form:r
    ?~  export-section.m  &+~
    =/  exp  i.export-section.m
    ?:  (~(has in names) name.exp)  |+'name duplicate'
    =;  [i=@ num=@]
      ?:  (gte i num)  |+'invalid export index'
      $(export-section.m t.export-section.m, names (~(put in names) name.exp))
    ?-  -.export-desc.exp
      %func  [i.export-desc.exp n-funcs]
      %tabl  [i.export-desc.exp n-tables]
      %memo  [i.export-desc.exp n-memos]
      %glob  [i.export-desc.exp n-globs]
    ==
  ::
  ++  v-start-section
    |=  [m=module functypes=(list func-type)]
    =/  r  (result ,~)
    ^-  form:r
    ?~  start-section.m  &+~
    =/  func-idx=@  u.start-section.m
    ;<  type=func-type  bind:r  ((snug 'start section') func-idx functypes)
    ?.  ?=([~ ~] type)
      |+'non-void start function'
    &+~
  ::
  ++  v-elem-section
    ::  elems are additionaly restricted by the parser: offset
    ::  expression is limited to a single const instruction,
    ::  and init expression are limited to a single %ref* instruction
    ::
    |=  [m=module n-tables=@ functypes=(list func-type) =store]
    =/  r  (result ,~)
    ^-  form:r
    ?~  elem-section.m  &+~
    =/  elem  i.elem-section.m
    ;<  ~  bind:r
      =/  r  (result ,~)
      |-  ^-  form:r
      ?~  i.elem  &+~
      ?:  ?=(%ref-null -.i.i.elem)
        ?:  =(t.elem t.i.i.elem)  $(i.elem t.i.elem)
        |+'%ref-null type mismatch in element'
      ?.  ?=(%func t.elem)  |+'%ref-null type mismatch in element'
      =/  idx=@  func-id.i.i.elem
      ;<  *  bind:r  ((snug 'elem section funcref') idx functypes)
      $(i.elem t.i.elem)
    ?.  ?=(%acti -.m.elem)  $(elem-section.m t.elem-section.m)
    ?:  (gte tab.m.elem n-tables)  |+'element index error'
    :: ?.  ?=(%i32 -.p.off.m.elem)  |+'type error in element offset'
    ?:  ?=(?(%ref-null %ref-func %vec) -.off.m.elem)
      |+'type error in element offset'
    ?:  ?=(%const -.off.m.elem)
      ?.  ?=(%i32 -.p.off.m.elem)  |+'type error in element offset'
      $(elem-section.m t.elem-section.m)
    ::  %global-get
    ;<  glob=glob-type  bind:r
      ((snug 'global section') index.off.m.elem globs.store)
    ?.  ?=(%i32 v.glob)  |+'type error in element offset'
    $(elem-section.m t.elem-section.m)
  ::
  ++  v-datacnt-section
    |=  m=module
    =/  r  (result (unit @))
    ^-  form:r
    &+datacnt-section.m
  ::
  ++  v-code-section
    |=  $:  m=module
            n-funcs-import=@
            =store
        ==
    ?.  =((lent code-section.m) (lent function-section.m))
      |+'mismatching lengths of function and code sections'
    =/  idx=@  n-funcs-import
    =/  r  (result ,~)
    |-  ^-  form:r
    ?~  code-section.m  &+~
    ;<  type=func-type  bind:r  ((snug 'code section') idx funcs.store)
    ;<  ~  bind:r  (validate-code idx i.code-section.m type m store)
    $(idx +(idx), code-section.m t.code-section.m)
  ::
  ++  v-data-section
    ::  data section is additionaly restrained by the parser:
    ::  offset expression may only be a single const instruction
    ::
    |=  [m=module datacnt=(unit @) =store]
    =/  r  (result ,~)
    ^-  form:r
    ?:  &(?=(^ datacnt) !=(u.datacnt (lent data-section.m)))
      |+'wrong datacnt'
    |-  ^-  form:r
    ?~  data-section.m
      &+~
    =/  data  i.data-section.m
    ?:  ?=(%pass -.data)
      $(data-section.m t.data-section.m)
    ?~  memo.store  |+'no memory to copy data to'
    ?:  ?=(%const -.off.data)
      ?.  ?=(%i32 -.p.off.data)  |+'type error in data offset'
      $(data-section.m t.data-section.m)
    ?:  ?=(?(%ref-null %ref-func %vec) -.off.data)
      |+'type error in data offset'
    ::  global-get
    ;<  glob=glob-type  bind:r
      ((snug 'global-section') index.off.data globs.store)
    ?.  ?=(%i32 v.glob)  |+'type error in data offset'
    $(data-section.m t.data-section.m)
  ::
  ++  validate-code
    |=  $:  idx=@
            =code
            type=func-type
            =module
            =store
        ==
    =/  r  (result ,~)
    ^-  form:r
    =/  locals  (weld params.type locals.code)
    =/  stack=(list valtype)  ~
    =/  frames=(list (list valtype))  ~[results.type]
    =/  res
      %:  validate-expr
        expression.code
        module
        store
        locals
        stack
        frames
      ==
    ?-  -.res
      %&  res
      %|  |+(rap 3 'func ' (scot %ud idx) ': ' p.res ~)
    ==
  ::
  ++  validate-expr
    |=  $:  expr=expression
            =module
            =store
            $=  args
            $:  locals=(list valtype)
                stack=(list valtype)
                frames=(list (list valtype))
        ==  ==
    =/  r  (result ,~)
    ^-  form:r
    ?~  expr
      ?.  =(-.frames.args (flop stack.args))
        ~&  [frames.args (flop stack.args)]
        |+'type error in result'
      &+~
    =/  instr  i.expr
    ::  stack-polymorphic instructions (unconditional control transfer)
    ::
    ?:  ?=(%unreachable -.instr)  &+~
    ?:  ?=(%br -.instr)
      ;<  results=(list valtype)  bind:r
        ((snug 'br frames') label.instr frames.args)
      ?.  =(results (flop (scag (lent results) stack.args)))  |+'br type error'
      &+~
    ?:  ?=(%br-table -.instr)
      =/  labels=(list @)  [label-default label-vec]:instr
      ?.  =(%i32 -.stack.args)  |+'br-table index type error'
      =.  stack.args  +.stack.args
      |-  ^-  form:r
      ?~  labels  &+~
      ;<  results=(list valtype)  bind:r
        ((snug 'br-table frames') i.labels frames.args)
      ?.  =(results (flop (scag (lent results) stack.args)))
        |+'br-table type error'
      $(labels t.labels)
    ?:  ?=(%return -.instr)
      ?:  =(~ frames.args)  |+'no frames'
      =/  results=(list valtype)  (rear frames.args)
      ?.  =(results (flop (scag (lent results) stack.args)))
        |+'return type error'
      &+~
    ;<  [stack1=_stack.args]  bind:r
      (validate-instr instr module store args)
    $(expr t.expr, stack.args stack1)
  ::
  ++  validate-instr
    |=  $:  $=  instr
            $~  [%nop ~]
            $<(?(%unreachable %br %br-table %return) instruction)
        ::
            =module
            =store
            locals=(list valtype)
            stack=(pole valtype)
            frames=(list (list valtype))
        ==
    =/  r  (result _stack)
    ^-  form:r
    ::  value-polymorphic instructions
    ::
    ?:  ?=(%drop -.instr)
      ?~  stack  |+'drop empty'
      &+[+.stack]
    ?:  ?=(%select -.instr)
      ?~  +.instr
        ?.  &(?=([%i32 t1=* t2=* *] stack) =(t1.stack t2.stack))
          |+'select type error'
        &+[+>.stack]
      ?.  ?&  ?=([%i32 t1=* t2=* *] stack)
              =(t1.stack t2.stack)
              =(t1.stack u.instr)
          ==
        |+'select type error'
      &+[+>.stack]
    ::  block instructions
    ::
    ?:  ?=(%block -.instr)
      ;<  type=func-type  bind:r
        ?@  type.instr  ((snug 'type idx in block') type.instr funcs.store)
        &+type.instr
      =/  n-params=@  (lent params.type)
      ?.  =(params.type (flop (scag n-params stack)))
        |+'block params mismatch'
      ;<  ~  bind:r
        %:  validate-expr
          body.instr
          module
          store
          locals
          (flop params.type)
          [results.type frames]
        ==
      &+(weld (flop results.type) (slag n-params stack))
    ?:  ?=(%loop -.instr)
      ;<  type=func-type  bind:r
        ?@  type.instr  ((snug 'type idx in loop') type.instr funcs.store)
        &+type.instr
      =/  n-params=@  (lent params.type)
      ?.  =(params.type (flop (scag (lent params.type) stack)))
        |+'loop params mismatch'
      ;<  ~  bind:r
        %:  validate-expr
          body.instr
          module
          store
          locals
          (flop params.type)
          [params.type frames]
        ==
      &+(weld (flop results.type) (slag n-params stack))
    ?:  ?=(%if -.instr)
      ;<  type=func-type  bind:r
        ?@  type.instr  ((snug 'type idx in loop') type.instr funcs.store)
        &+type.instr
      =/  n-params=@  (lent params.type)
      ?.  =(%i32 -.stack)  |+'if no flag'
      =.  stack  +.stack
      ?.  =(params.type (flop (scag n-params stack)))
        |+'if params mismatch'
      ;<  ~  bind:r
        %:  validate-expr
          branch-true.instr
          module
          store
          locals
          (flop params.type)
          [results.type frames]
        ==
      ;<  ~  bind:r
        %:  validate-expr
          branch-false.instr
          module
          store
          locals
          (flop params.type)
          [results.type frames]
        ==
      &+(weld (flop results.type) (slag n-params stack))
    ::  some instructions that are handled separately from the rest
    ::  for no good reason (except for %ref-is-null, it's kinda
    ::  polymorphic, and br-if depends on frames)
    ::
    ?:  ?=(%br-if -.instr)
      ;<  results=(list valtype)  bind:r
        ((snug 'br-if frames') label.instr frames)
      ?:  =(~ stack)  |+'br-if no cond'
      ?.  =(%i32 -.stack)  |+'br-if cond type mismatch'
      =.  stack  +.stack
      ?.  =(results (flop (scag (lent results) stack)))
        |+'br-if type error'
      &+stack
    ?:  ?=(%call -.instr)
      ;<  type=func-type  bind:r
        ((snug 'call idx') func-id.instr funcs.store)
      =/  n-params=@  (lent params.type)
      ?.  =(params.type (flop (scag n-params stack)))
        |+'call params mismatch'
      &+(weld (flop results.type) (slag n-params stack))
    ?:  ?=(%call-indirect -.instr)
      ;<  type=func-type  bind:r
        ((snug 'call-indirect idx') type-id.instr type-section.module)
      =/  n-params=@  (lent params.type)
      ?:  =(~ stack)  |+'call-indirect stack empty'
      ?.  =(%i32 -.stack)  |+'call-indirect idx type error'
      =.  stack  +.stack
      ?.  =(params.type (flop (scag n-params stack)))
        |+'call-indirect params mismatch'
      &+(weld (flop results.type) (slag n-params stack))
    ?:  ?=(%ref-is-null -.instr)
      ?:  =(~ stack)  |+'ref-is-null empty stack'
      ?.  ?=(ref-type -.stack)  |+'ref-is-null type mismatch'
      &+[%i32 +.stack]
    ::  the rest
    ::
    ;<  type=func-type  bind:r  (get-type instr module store locals)
    =/  n-params=@  (lent params.type)
    ?.   =(params.type (flop (scag n-params stack)))
      |+(crip "type mismatch {<instr>}")
    &+(weld (flop results.type) (slag n-params stack))
  ::
  ++  get-type
    |=  $:  $=  instr
            $~  [%nop ~]
            $<  $?  %unreachable
                    %br
                    %br-table
                    %return
                    %drop
                    %select
                    %block
                    %loop
                    %if
                    %br-if
                    %call
                    %call-indirect
                    %ref-is-null
                ==
            instruction
        ::
            =module
            =store
            locals=(list valtype)
        ==
    ~+
    =/  r  (result func-type)
    ^-  form:r
    ?-    -.instr
        %vec     (get-type-vec +.instr module store)
        %dbug    ~|(%dbug !!)
        %const   &+[~ ~[(from-coin p.instr)]]
        %eqz     &+[~[type.instr] ~[%i32]]
        %clz     &+[~[type] ~[type]]:instr
        %ctz     &+[~[type] ~[type]]:instr
        %popcnt  &+?>(?=(valtype type.instr) [~[type] ~[type]]:instr)
        %abs     &+?>(?=(valtype type.instr) [~[type] ~[type]]:instr)
        %neg     &+?>(?=(valtype type.instr) [~[type] ~[type]]:instr)
        %ceil    &+[~[type] ~[type]]:instr
        %floor   &+[~[type] ~[type]]:instr
        %trunc
      ?~  source-type.instr
        &+[~[type] ~[type]]:instr
      &+[~[u.source-type] ~[type]]:instr
    ::
        %nearest      &+[~[type] ~[type]]:instr
        %sqrt         &+[~[type] ~[type]]:instr
        %wrap         &+[~[%i64] ~[%i32]]
        %extend       &+[~[source-type] ~[type]]:instr
        %convert      &+[~[source-type] ~[type]]:instr
        %demote       &+[~[%f64] ~[%f32]]
        %promote      &+[~[%f32] ~[%f64]]
        %reinterpret  &+[~[source-type] ~[type]]:instr
    ::
        %eq    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %ne    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %lt    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %gt    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %le    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %ge    &+?>(?=(valtype type.instr) [~[type type] ~[%i32]]:instr)
        %add   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %sub   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %mul   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %div   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %rem   &+[~[type type] ~[type]]:instr
        %and   &+[~[type type] ~[type]]:instr
        %or    &+[~[type type] ~[type]]:instr
        %xor   &+[~[type type] ~[type]]:instr
        %shl   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %shr   &+?>(?=(valtype type.instr) [~[type type] ~[type]]:instr)
        %rotl  &+[~[type type] ~[type]]:instr
        %rotr  &+[~[type type] ~[type]]:instr
        %min   &+[~[type type] ~[type]]:instr
        %max   &+[~[type type] ~[type]]:instr
        %copysign  &+[~[type type] ~[type]]:instr
        %nop  &+[~ ~]
    ::
        %ref-null  &+[~ ~[t.instr]]
        %ref-func
      ?.  (lth func-id.instr (lent funcs.store))  |+'ref func idx error'
      &+[~ ~[%func]]
    ::
        %local-get
      ;<  type=valtype  bind:r  ((snug 'local get') index.instr locals)
      &+[~ ~[type]]
    ::
        %local-set
      ;<  type=valtype  bind:r  ((snug 'local set') index.instr locals)
      &+[~[type] ~]
    ::
        %local-tee
      ;<  type=valtype  bind:r  ((snug 'local tee') index.instr locals)
      &+[~[type] ~[type]]
    ::
        %global-get
      ;<  type=glob-type  bind:r  ((snug 'global get') index.instr globs.store)
      &+[~ ~[v.type]]
    ::
        %global-set
      ;<  type=glob-type  bind:r  ((snug 'global set') index.instr globs.store)
      ?:  ?=(%con m.type)  |+'constant global set'
      &+[~[v.type] ~]
    ::
        %table-get
      ;<  =table  bind:r  ((snug 'table get') tab-id.instr tables.store)
      &+[~[%i32] ~[p.table]]
    ::
        %table-set
      ;<  =table  bind:r  ((snug 'table get') tab-id.instr tables.store)
      &+[~[%i32 p.table] ~]
    ::
        %table-init
      ;<  =table  bind:r  ((snug 'table init table') tab-id.instr tables.store)
      ;<  =elem  bind:r
        ((snug 'table init elem') elem-id.instr elem-section.module)
      ?.  =(t.elem p.table)  |+'table init type mismatch'
      &+[~[%i32 %i32 %i32] ~]
    ::
        %elem-drop
      ;<  *  bind:r
        ((snug 'elem drop') elem-id.instr elem-section.module)
      &+[~ ~]
    ::
        %table-copy
      ;<  tab-x=table  bind:r
        ((snug 'table copy') tab-id-x.instr tables.store)
      ;<  tab-y=table  bind:r
        ((snug 'table copy') tab-id-y.instr tables.store)
      ?.  =(p.tab-x p.tab-y)  |+'table copy type mismatch'
      &+[~[%i32 %i32 %i32] ~]
    ::
        %table-grow
      ;<  =table  bind:r  ((snug 'table grow') tab-id.instr tables.store)
      &+[~[p.table %i32] ~[%i32]]
    ::
        %table-size
      ;<  =table  bind:r  ((snug 'table grow') tab-id.instr tables.store)
      &+[~ ~[%i32]]
    ::
        %table-fill
      ;<  =table  bind:r  ((snug 'table grow') tab-id.instr tables.store)
      &+[~[%i32 p.table %i32] ~]
    ::
        %load
      ?~  memo.store  |+'load no memo'
      =/  misaligned=?
        %+  gth  (bex align.m.instr)
        ?~  n.instr
          (byte-width type.instr)
        (div u.n.instr 8)
      ?:  misaligned
        ~&  instr
        |+'load misaligned'
      &+[~[%i32] ~[type.instr]]
    ::
        %store
      ?~  memo.store  |+'store no memo'
      =/  misaligned=?
        %+  gth  (bex align.m.instr)
        ?~  n.instr
          (byte-width type.instr)
        (div u.n.instr 8)
      ?:  misaligned  |+'store misaligned'
      &+[~[%i32 type.instr] ~]
    ::
        %memory-size
      ?~  memo.store  |+'size no memo'
      &+[~ ~[%i32]]
    ::
        %memory-grow
      ?~  memo.store  |+'grow no memo'
      &+[~[%i32] ~[%i32]]
    ::
        %memory-init
      ?~  memo.store  |+'init no memo'
      ;<  *  bind:r  ((snug 'memo init elem') x.instr elem-section.module)
      &+[~[%i32 %i32 %i32] ~]
    ::
        %data-drop
      ;<  *  bind:r  ((snug 'memo init elem') x.instr elem-section.module)
      &+[~ ~]
    ::
        %memory-copy
      ?~  memo.store  |+'copy no memo'
      &+[~[%i32 %i32 %i32] ~]
    ::
        %memory-fill
      ?~  memo.store  |+'fill no memo'
      &+[~[%i32 %i32 %i32] ~]
    ::
    ==
  ::
  ++  get-type-vec
    |=  [instr=instr-vec =module =store]
    =/  r  (result func-type)
    ^-  form:r
    ?-    -.instr
        %load
      ?~  memo.store  |+'load no memo'
      ?~  kind.instr
        ?:  (gth (bex align.m.instr) 16)
          |+'load misaligned'
        &+[~[%i32] ~[%v128]]
      ?-    q.u.kind.instr
          ?(%splat %zero)
        ?:  (gth (bex align.m.instr) (div p.u.kind.instr 8))
          |+'load misaligned'
        &+[~[%i32] ~[%v128]]
      ::
          [%extend sign=?(%s %u)]
        ?:  (gth (bex align.m.instr) 8)
          |+'load misaligned'
        &+[~[%i32] ~[%v128]]
      ==
    ::
        %load-lane
      ?~  memo.store  |+'load no memo'
      ?.  (lth l.instr (div 128 p.instr))  |+'load bad lane'
      ?:  (gth (bex align.m.instr) (div p.instr 8))
        |+'load misaligned'
      &+[~[%i32 %v128] ~[%v128]]
    ::
        %store
      ?~  memo.store  |+'store no memo'
      ?:  (gth (bex align.m.instr) 16)
        |+'store misaligned'
      &+[~[%i32 %v128] ~]
    ::
        %store-lane
      ?~  memo.store  |+'store no memo'
      ?.  (lth l.instr (div 128 p.instr))  |+'load bad lane'
      ?:  (gth (bex align.m.instr) (div p.instr 8))
        |+'store misaligned'
      &+[~[%i32 %v128] ~]
    ::
        %const  &+[~ ~[%v128]]
        %shuffle
      |-  ^-  form:r
      ?~  lane-ids.instr  &+[~[%v128 %v128] ~[%v128]]
      ?.  (lth i.lane-ids.instr 32)  |+'shuffle bad lane'
      $(lane-ids.instr t.lane-ids.instr)
    ::
        %extract
      ?.  (lth l.instr (dim-lane p.instr))  |+'extract bad lane'
      &+[~[%v128] ~[(unpack p.instr)]]
    ::
        %replace
      ?.  (lth l.instr (dim-lane p.instr))  |+'extract bad lane'
      &+[~[%v128 (unpack p.instr)] ~[%v128]]
    ::
        %swizzle  &+[~[%v128 %v128] ~[%v128]]
        %splat    &+[~[(unpack p.instr)] ~[%v128]]
    ::
        ?(%eq %ne %lt %gt %le %ge)  &+[~[%v128 %v128] ~[%v128]]  ::  vrelop
    ::
        %not
      &+[~[%v128] ~[%v128]]  ::  vvunop
    ::
        ?(%and %andnot %or %xor)
      &+[~[%v128 %v128] ~[%v128]]  ::  vvbinop
    ::
        %bitselect
      &+[~[%v128 %v128 %v128] ~[%v128]]  ::  vvterop
    ::
        %any-true
      &+[~[%v128] ~[%i32]]  ::  vvtestop
    ::
        ?(%abs %neg %popcnt %sqrt %ceil %floor %trunc %nearest)
      &+[~[%v128] ~[%v128]]  ::  vunop
    ::
        %all-true  &+[~[%v128] ~[%i32]]
        %bitmask   &+[~[%v128] ~[%i32]]
        %narrow  &+[~[%v128 %v128] ~[%v128]]
        ?(%shl %shr)  &+[~[%v128 %i32] ~[%v128]]
        $?  %add
            %sub
            %min
            %max
            %avgr
            %q15mul-r-sat
            %mul
            %div
            %pmin
            %pmax
        ==
      &+[~[%v128 %v128] ~[%v128]]
    ::
        %extadd  &+[~[%v128] ~[%v128]]
        %extmul  &+[~[%v128 %v128] ~[%v128]]
        %dot  &+[~[%v128 %v128] ~[%v128]]
    ::
        $?  %extend
            %convert
            %demote
            %promote
        ==
      &+[~[%v128] ~[%v128]]  ::  vcvtop, trunc_sat is covered by vunop
    ==
  ::
  ++  from-coin
    |=  coin=coin-wasm
    ^-  valtype
    ?-  -.coin
      valtype  -.coin
      %ref     +<.coin
    ==
  ::
  ++  byte-width
    |=  v=?(num-type vec-type)
    ^-  @
    ?-  v
      ?(%i32 %f32)  4
      ?(%i64 %f64)  8
      %v128         16
    ==
  ::
  ++  dim-lane
    |=  l=lane-type
    ^-  @
    ?-  l
      %i8           16
      %i16          8
      ?(%i32 %f32)  4
      ?(%i64 %f64)  2
    ==
  ::
  ++  unpack
    |=  l=lane-type
    ^-  num-type
    ?-  l
      num-type  l
      ?(%i8 %i16)  %i32
    ==
  --  ::  |validator
--