/+  wasm-runner-engine
=>  wasm-runner-engine
~%  %monad-v0  +  ~
=,  lia-sur
=*  cw  coin-wasm:wasm-sur
|%
::
+$  run-input
  (each (script-raw-form (list lia-value)) (list lia-value))
::  ++run-once: extract. Type polymorphic.
::
++  run-once
  ~/  %run-once-v0
  |*  type=mold
  =/  m  (script type)
  ~%  %run-once-inner-v0  +>+  ~
  |=  [[binary=octs imp=import] hint=term script-in=form:m]
  ::
  :: ~&  !.(call+!=(call))                 ::  [9 20 0 63]      kick
  :: ~&  !.(memread+!=(memread))           ::  [9 3.006 0 63]   kick
  :: ~&  !.(memwrite+!=(memwrite))         ::  [9 750 0 63]     kick
  :: ~&  !.(call-ext+!=(call-ext))         ::  [9 1.498 0 63]   kick
  :: ~&  !.(global-set+!=(global-set))     ::  [9 92 0 63]      kick
  :: ~&  !.(global-get+!=(global-get))     ::  [9 186 0 63]     kick
  :: ~&  !.(memory-size+!=(memory-size))   ::  [9 748 0 63]
  :: ~&  !.(memory-grow+!=(memory-grow))   ::  [9 12.021 0 63]  kick
  :: ~&  !.(runnable+!=(runnable))         ::  [9 3.004 0 63]
  :: ~&  'from runnable:'
  :: ~&  !.(try-m+!=(try):runnable)        ::  [9 21 0 1]  kick x2
  :: ~&  !.(catch-m+!=(catch):runnable)    ::  [9 4 0 1]   kick x2
  :: ~&  !.(return-m+!=(return):runnable)  ::  [9 20 0 1]  kick
  ::
  ^-  yield:m
  =,  engine-sur
  =>  [- [[binary=binary imp=imp] script-in=script-in] +>]  ::  remove hint
  =/  ast  (main:parser binary)
  =/  valid  (validate-module:validator ast)
  ?.  ?=(%& -.valid)
    ~&  valid
    !!
  =/  sat=lia-state  [(conv:engine ast ~) ~ imp]
  |^  ^-  yield:m
  -:(;<(* try:m init script-in) sat)  ::  ((init >> script-in) sat)
  ::
  ++  init
    =/  m  (script ,~)
    ^-  form:m
    |=  sat=lia-state
    ^-  output:m
    =/  engine-res=result:engine
      (instantiate:engine p.sat)
    ?:  ?=(%0 -.engine-res)  [0+~ sat(p st.engine-res)]
    ?:  ?=(%2 -.engine-res)  [2+~ sat(p st.engine-res)]
    ::  engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
    ::
    ?>  ?=(%func -.request.engine-res)
    =/  sat-blocked=lia-state  [[~ +>.engine-res] q.sat r.sat]  ::  Wasm blocked on import
    =/  import-arrow
      (~(got by imp) mod.engine-res name.engine-res)
    =^  import-yil=(script-yield (list cw))  sat-blocked
      ((import-arrow args.request.engine-res) sat-blocked)
    ?.  ?=(%0 -.import-yil)  [import-yil sat-blocked]
    $(shop.p.sat (snoc shop.p.sat p.import-yil +.p.sat-blocked))
  --
::  ++run: extend & extract. Type monomorphic.
::
++  run
  ~/  %run-v0
  |=  [input=run-input =seed hint=term]
  ::
  =,  engine-sur
  =/  m  runnable
  =>  [- [input=input seed=seed] +>]  ::  remove hint
  ^-  [yield:m _seed]
  =.  seed
    ?-    -.input
        %&
      ::  jet matching hack
      ::  past.seed >> p.input
      ::
      seed(past ((try:m) past.seed =>(p.input |=(* +>))))
    ::
        %|
      seed(shop (snoc shop.seed p.input))
    ==
  =/  ast  (main:parser module.seed)
  =/  valid  (validate-module:validator ast)
  ?>  ?=(%& -.valid)
  =/  sat=lia-state  [(conv:engine ast ~) shop.seed import.seed]
  |^  ^-  [yield:m _seed]
  =/  yil=yield:m  -:(;<(* try:m init past.seed) sat)  ::  ((init >> past.seed) sat)
  [yil seed]
  ::
  ++  init
    =/  m  (script ,~)
    ^-  form:m
    |=  sat=lia-state
    ^-  output:m
    =/  engine-res=result:engine
      (instantiate:engine p.sat)
    ?:  ?=(%0 -.engine-res)  [0+~ sat(p st.engine-res)]
    ?:  ?=(%2 -.engine-res)  [2+~ sat(p st.engine-res)]
    ::  engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
    ::
    ?>  ?=(%func -.request.engine-res)
    =/  sat-blocked=lia-state  [[~ +>.engine-res] q.sat r.sat]  ::  Wasm blocked on import
    =/  import-arrow
      (~(got by import.seed) mod.engine-res name.engine-res)
    =^  import-yil=(script-yield (list cw))  sat-blocked
      ((import-arrow args.request.engine-res) sat-blocked)
    ?.  ?=(%0 -.import-yil)  [import-yil sat-blocked]
    $(shop.p.sat (snoc shop.p.sat p.import-yil +.p.sat-blocked))
  --
::
::  Basic Lia ops (Kleisli arrows)
::
++  call
  |=  [name=cord args=(list @)]
  :: =*  ctx  +>
  :: =*  sam  +<
  =/  m  (script (list @))
  ^-  form:m
  |=  sat=lia-state
  :: ~&  !.(call-ctx+!=(ctx))
  :: ~&  !.(call-sam+!=(sam))
  =,  module.p.sat
  =/  id=@  (find-func-id:engine name module.p.sat)
  =/  id-local=@
    (sub id (lent funcs.import-section))
  =/  =func-type
    (snag type-id:(snag id-local function-section) type-section)
  ?>  =((lent params.func-type) (lent args))
  =/  engine-res=result:engine
    (invoke:engine name (types-atoms-to-coins params.func-type args) p.sat)
  ?:  ?=(%0 -.engine-res)
    [0+(turn out.engine-res cw-to-atom) sat(p st.engine-res)]
  ?:  ?=(%2 -.engine-res)
    [2+~ sat(p st.engine-res)]
  ::  engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
  ::
  ?>  ?=(%func -.request.engine-res)
  =/  sat-blocked=lia-state  [[~ +>.engine-res] q.sat r.sat]  ::  Wasm blocked on import
  =/  import-arrow
    ~|  "couldn't find import {<mod.engine-res>}/{<name.engine-res>}"
    (~(got by r.sat) mod.engine-res name.engine-res)
  =^  import-yil=(script-yield (list cw))  sat-blocked
    ((import-arrow args.request.engine-res) sat-blocked)
  ?.  ?=(%0 -.import-yil)  [import-yil sat-blocked]
  $(shop.p.sat (snoc shop.p.sat p.import-yil +.p.sat-blocked))
::
++  call-1
  |=  [name=cord args=(list @)]
  =/  m  (script @)
  ^-  form:m
  ;<  out=(list @)  try:m  (call name args)
  ?>  =(1 (lent out))
  (return:m -.out)
::
++  memread
  |=  [ptr=@ len=@]
  :: =*  ctx  +>
  =/  m  (script octs)
  ^-  form:m
  |=  sat=lia-state
  :: ~&  !.(memread-ctx+!=(ctx))
  =.  ptr  (mod ptr ^~((bex 32)))
  ?~  mem.p.sat  [2+~ sat]
  =,  u.mem.p.sat
  ?:  (gth (add ptr len) (mul n-pages page-size))
    [2+~ sat]
  [0+[len (cut 3 [ptr len] buffer)] sat]
::
++  memwrite
  |=  [ptr=@ len=@ src=@]
  :: =*  ctx  +>
  =/  m  (script ,~)
  ^-  form:m
  |=  sat=lia-state
  :: ~&  !.(memwrite-ctx+!=(ctx))
  =.  ptr  (mod ptr ^~((bex 32)))
  ?~  mem.p.sat  [2+~ sat]
  =,  u.mem.p.sat
  ?:  (gth (add ptr len) (mul n-pages page-size))
    [2+~ sat]
  :-  0+~
  sat(buffer.u.mem.p (sew 3 [ptr len src] buffer))
::
++  call-ext
  |=  [name=term args=(list lia-value)]
  =/  m  (script (list lia-value))
  ^-  form:m
  |=  sat=lia-state
  ?~  q.sat
    [1+[name args] sat]
  [0+i.q.sat sat(q t.q.sat)]
::
++  global-set
  |=  [name=cord value=@]
  =/  m  (script ,~)
  ^-  form:m
  |=  sat=lia-state
  =/  exports  export-section.module.p.sat
  |-  ^-  output:m
  ?~  exports
    ~|("exported global not found" !!)
  ?.  =(name name.i.exports)
    $(exports t.exports)
  =/  =export-desc  export-desc.i.exports
  =+  glob=(glob:grab:op-def i.export-desc p.sat)
  ?:  ?=(%| -.glob)
    ~|("non-local global set" !!)
  =.  globals.p.sat
    (snap globals.p.sat p.p.glob (val-to-coin:op-def value q.p.glob))
  [0+~ sat]
::
++  global-get
  |=  name=cord
  =/  m  (script @)
  ^-  form:m
  |=  sat=lia-state
  =/  exports  export-section.module.p.sat
  |-  ^-  output:m
  ?~  exports
    ~|("exported global not found" !!)
  ?.  =(name name.i.exports)
    $(exports t.exports)
  =/  =export-desc  export-desc.i.exports
  =+  glob=(glob:grab:op-def i.export-desc p.sat)
  ?:  ?=(%| -.glob)
    ~|("non-local global get" !!)
  [0+;;(@ (coin-to-val:op-def q.p.glob)) sat]
::
++  memory-size
  =/  m  (script @)
  ^-  form:m
  |=  sat=lia-state
  ?~  mem.p.sat
    ~|("no memory" !!)
  [0+n-pages.u.mem.p.sat sat]
::
++  memory-grow  ::  returns old size in pages
  |=  delta=@
  =/  m  (script @)
  ^-  form:m
  |=  sat=lia-state
  ?~  mem.p.sat
    ~|("no memory" !!)
  :-  0+n-pages.u.mem.p.sat
  sat(n-pages.u.mem.p (add delta n-pages.u.mem.p.sat))
::
::  misc
::
++  runnable  (script (list lia-value))
++  seed-init
  |=  [module=octs =import]
  ^-  seed
  =/  m  (script (list lia-value))
  :*  module
      (return:m ~)
      ~
      import
  ==
++  cw-to-atom
  |=  cw=coin-wasm:wasm-sur
  ^-  @
  ?<  ?=(%ref -.cw)
  +.cw
::
++  types-atoms-to-coins
  |=  [a=(list valtype:wasm-sur) b=(list @)]
  ^-  (list coin-wasm:wasm-sur)
  ?:  &(?=(~ a) ?=(~ b))  ~
  ?>  &(?=(^ a) ?=(^ b))
  :_  $(a t.a, b t.b)
  ?<  ?=(ref-type:wasm-sur i.a)
  ^-  cw
  ?-  i.a
    %i32   [i.a i.b]
    %i64   [i.a i.b]
    %f32   [i.a i.b]
    %f64   [i.a i.b]
    %v128  [i.a i.b]
  ==
::
++  page-size  ^~((bex 16))
::
++  yield-need
  |*  a=(script-yield)
  ?>  ?=(%0 -.a)
  p.a
::
--  ::  |script-lib