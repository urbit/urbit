/+  wasm-runner-engine
=>  wasm-runner-engine
~%  %monad-v0  +  ~
=,  lia-sur
=*  cw  coin-wasm:wasm-sur
|%
::
+$  run-input
  (each (script-raw-form (list lia-value) vase) (list lia-value))
::  ++run-once: extract. Type polymorphic.
::
++  run-once
  ~/  %run-once-v0
  |*  [type-yield=mold type-acc=mold]
  =/  m  (script type-yield type-acc)
  ~%  %run-once-inner-v0  +>+  ~
  |=  [[binary=octs imp=(import type-acc)] hint=term script-in=form:m]
  ::
  :: ~&  !.(arrows+!=(arrows))                                         ::  [9 1502 0 63]
  :: ~&  'from arrows:'
  :: ~&  !.(call+!=(call):(arrows))                                    ::  [9 20 0 1] kick x1
  :: ~&  !.(memread+!=(memread):(arrows))                              ::  [9 383 0 1] kick x1
  :: ~&  !.(memwrite+!=(memwrite):(arrows))                            ::  [9 94 0 1] kick x1
  :: ~&  !.(call-ext+!=(call-ext):(arrows))                            ::  [9 375 0 1] kick x1
  :: ~&  !.(global-set+!=(global-set):(arrows))                        ::  [9 4 0 1]  kick x1
  :: ~&  !.(global-get+!=(global-get):(arrows))                        ::  [9 22 0 1] kick x1
  :: ~&  !.(memory-size+!=(memory-size):(arrows))                      ::  [9 186 0 1]
  :: ~&  !.(memory-grow+!=(memory-grow):(arrows))                      ::  [9 381 0 1] kick x1
  :: ~&  !.(get-acc+!=(get-acc):(arrows))                              ::  [9 374 0 1]
  :: ~&  !.(set-acc+!=(set-acc):(arrows))                              ::  [9 92 0 1] kick x1
  :: ~&  !.(get-all-local-globals+!=(get-all-local-globals):(arrows))  ::  [9 43 0 1]
  :: ~&  !.(set-all-local-globals+!=(set-all-local-globals):(arrows))  ::  [9 380 0 1] kick x1
  :: ~&  ''
  :: ~&  !.(runnable+!=(runnable))                 ::  [9 374 0 63]
  :: ~&  'from runnable:'    
  :: ~&  !.(try-m+!=(try):runnable)                ::  [9 43 0 1]  kick x2
  :: ~&  !.(catch-m+!=(catch):runnable)            ::  [9 4 0 1]   kick x2
  :: ~&  !.(return-m+!=(return):runnable)          ::  [9 20 0 1]  kick
  :: ~&  !.(fail-m+!=(fail):runnable)              ::  [9 47 0 1]  kick
  ::
  ^-  [yield:m type-acc]
  =,  engine-sur
  =>  [- [[binary=binary imp=imp] script-in=script-in] +>]  ::  remove hint
  =/  ast  (main:parser binary)
  =/  valid  (validate-module:validator ast)
  ?.  ?=(%& -.valid)
    ~&  valid
    !!
  =/  sat=(lia-state type-acc)  [(conv:engine ast ~) ~ imp]
  |^  ^-  [yield:m type-acc]
  =^  yil  sat  (;<(* try:m init script-in) sat)  ::  ((init >> script-in) sat)
  [yil p.r.sat]
  ::
  ++  init
    =/  m  (script ,~ type-acc)
    ^-  form:m
    |=  sat=(lia-state type-acc)
    ^-  output:m
    =/  engine-res=result:engine
      (instantiate:engine p.sat)
    ?:  ?=(%0 -.engine-res)  [0+~ sat(p st.engine-res)]
    ?:  ?=(%2 -.engine-res)  [2+~ sat(p st.engine-res)]
    ::  engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
    ::
    ?>  ?=(%func -.request.engine-res)
    =/  sat-blocked=(lia-state type-acc)  [[~ +>.engine-res] q.sat r.sat]  ::  Wasm blocked on import
    =/  import-arrow
      (~(got by q.r.sat-blocked) mod.engine-res name.engine-res)
    =^  import-yil=(script-yield (list cw))  sat-blocked
      ((import-arrow args.request.engine-res) sat-blocked)
    ?.  ?=(%0 -.import-yil)  [import-yil sat-blocked]
    %=  $
      shop.p.sat  (snoc shop.p.sat p.import-yil +.p.sat-blocked)
      p.r.sat  p.r.sat-blocked
    ==
  --
::  ++run: extend & extract. Type monomorphic.
::
++  run
  ~/  %run-v1
  |=  [input=run-input =seed hint=*]
  ::
  =,  engine-sur
  =/  m  runnable
  =>  [- [input=input seed=seed] +>]  ::  remove hint
  ^-  [[yield:m vase] _seed]
  =.  seed
    ?-    -.input
        %&
      ::  jet matching hack
      ::  past.seed >> p.input
      ::
      !.
      seed(past ((try:m) past.seed =>(p.input |=(* +>))))
    ::
        %|
      seed(shop [p.input shop.seed])
    ==
  =/  ast  (main:parser module.seed)
  =/  valid  (validate-module:validator ast)
  ?>  ?=(%& -.valid)
  =/  sat=(lia-state vase)  [(conv:engine ast ~) (flop shop.seed) import.seed]
  |^  ^-  [[yield:m vase] _seed]
  =^  yil=yield:m  sat  (;<(* try:m init past.seed) sat)  ::  ((init >> past.seed) sat)
  [[yil p.r.sat] seed]
  ::
  ++  init
    =/  m  (script ,~ vase)
    ^-  form:m
    |=  sat=(lia-state vase)
    ^-  output:m
    =/  engine-res=result:engine
      (instantiate:engine p.sat)
    ?:  ?=(%0 -.engine-res)  [0+~ sat(p st.engine-res)]
    ?:  ?=(%2 -.engine-res)  [2+~ sat(p st.engine-res)]
    ::  engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
    ::
    ?>  ?=(%func -.request.engine-res)
    =/  sat-blocked=(lia-state vase)  [[~ +>.engine-res] q.sat r.sat]  ::  Wasm blocked on import
    =/  import-arrow
      (~(got by q.r.sat-blocked) mod.engine-res name.engine-res)
    =^  import-yil=(script-yield (list cw))  sat-blocked
      ((import-arrow args.request.engine-res) sat-blocked)
    ?.  ?=(%0 -.import-yil)  [import-yil sat-blocked]
    %=  $
      shop.p.sat  (snoc shop.p.sat p.import-yil +.p.sat-blocked)
      p.r.sat  p.r.sat-blocked
    ==
  --
::
::  Basic Lia ops (Kleisli arrows)
::
++  arrows
  :: =*  ctx  .
  |*  m-acc=mold
  ^?  !.  |%
  ++  m-sat  (lia-state m-acc)
  ++  call
    |=  [name=cord args=(list @)]
    :: =*  ctx  +>
    :: =*  sam  +<
    =/  m  (script (list @) m-acc)
    ^-  form:m
    |=  sat=m-sat
    :: ~&  !.(arrow-ctx+!=(ctx))
    :: ~&  !.(call-sam+!=(sam))
    =,  module.p.sat
    =/  id=@  (find-func-id:engine name module.p.sat)
    =/  =func-type
      =/  func  (func:grab:op-def id p.sat)
      ?:  ?=(%& -.func)  (snag type-id.p.func type-section)
      (snag type-id.p.func type-section)
    ?>  =((lent params.func-type) (lent args))
    =/  engine-res=result:engine
      (invoke-id:engine id (types-atoms-to-coins params.func-type args) p.sat)
    ?:  ?=(%0 -.engine-res)
      [0+(turn out.engine-res cw-to-atom) sat(p st.engine-res)]
    ?:  ?=(%2 -.engine-res)
      [2+~ sat(p st.engine-res)]
    ::  engine-res = [%1 [[mod=cord name=cord] =request] module mem tables globals]
    ::
    ?>  ?=(%func -.request.engine-res)
    =/  sat-blocked=m-sat  [[~ +>.engine-res] q.sat r.sat]  ::  Wasm blocked on import
    =/  import-arrow
      ~|  "couldn't find import {<mod.engine-res>}/{<name.engine-res>}"
      (~(got by q.r.sat-blocked) mod.engine-res name.engine-res)
    =^  import-yil=(script-yield (list cw))  sat-blocked
      ((import-arrow args.request.engine-res) sat-blocked)
    ?.  ?=(%0 -.import-yil)  [import-yil sat-blocked]
    %=  $
      shop.p.sat  (snoc shop.p.sat p.import-yil +.p.sat-blocked)
      p.r.sat  p.r.sat-blocked
    ==
  ::
  ++  call-1
    |=  [name=cord args=(list @)]
    =/  m  (script @ m-acc)
    ^-  form:m
    ;<  out=(list @)  try:m  (call name args)
    ?>  =(1 (lent out))
    (return:m -.out)
  ::
  ++  memread
    |=  [ptr=@ len=@]
    :: =*  ctx  +>
    =/  m  (script octs m-acc)
    ^-  form:m
    |=  sat=m-sat
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
    =/  m  (script ,~ m-acc)
    ^-  form:m
    |=  sat=m-sat
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
    =/  m  (script (list lia-value) m-acc)
    ^-  form:m
    |=  sat=m-sat
    ?~  q.sat
      [1+[name args] sat]
    [0+i.q.sat sat(q t.q.sat)]
  ::
  ++  global-set
    |=  [name=cord value=@]
    =/  m  (script ,~ m-acc)
    ^-  form:m
    |=  sat=m-sat
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
    =/  glob-type=global:wasm-sur  (snag p.p.glob global-section.module.p.sat)
    ?>  ?=(%var m.glob-type)
    =.  globals.p.sat
      (snap globals.p.sat p.p.glob (val-to-coin:op-def value q.p.glob))
    [0+~ sat]
  ::
  ++  global-get
    |=  name=cord
    =/  m  (script @ m-acc)
    ^-  form:m
    |=  sat=m-sat
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
    =/  m  (script @ m-acc)
    ^-  form:m
    |=  sat=m-sat
    :: ~&  !.(monad-ctx+!=(ctx))
    ?~  mem.p.sat
      ~|("no memory" !!)
    [0+n-pages.u.mem.p.sat sat]
  ::
  ++  memory-grow  ::  returns old size in pages
    |=  delta=@
    =/  m  (script @ m-acc)
    ^-  form:m
    |=  sat=m-sat
    ?~  mem.p.sat
      ~|("no memory" !!)
    :-  0+n-pages.u.mem.p.sat
    sat(n-pages.u.mem.p (add delta n-pages.u.mem.p.sat))
  ::
  ++  get-acc
    =/  m  (script m-acc m-acc)
    ^-  form:m
    |=  sat=m-sat
    [0+p.r.sat sat]
  ::
  ++  set-acc
    |=  acc=m-acc
    =/  m  (script ,~ m-acc)
    ^-  form:m
    |=  sat=m-sat
    [0+~ sat(p.r acc)]
  ::
  ++  get-all-local-globals
    =/  m  (script (list @) m-acc)
    ^-  form:m
    |=  sat=m-sat
    [0+(turn globals.p.sat (cork coin-to-val:op-def @)) sat]
  :: ::
  ++  set-all-local-globals
    |=  vals=(list @)
    =/  m  (script ,~ m-acc)
    ^-  form:m
    |=  sat=m-sat
    :-  0+~
    %=    sat
        globals.p
      (types-atoms-to-coins (turn globals.p.sat valtype-from-coin) vals)
    ==
  --
::
::  misc
::
++  runnable  (script (list lia-value) vase)
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
++  valtype-from-coin
  |=  =cw
  ^-  valtype:wasm-sur
  ?-  -.cw
    ?(%i32 %i64 %f32 %f64 %v128)  -.cw
    %ref  +<.cw
  ==
++  page-size  ^~((bex 16))
::
++  yield-need
  |*  a=(script-yield *)
  ?>  ?=(%0 -.a)
  p.a
::
--  ::  |script-lib