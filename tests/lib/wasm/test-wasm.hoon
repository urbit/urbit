/+  *test
/+  *wasm-runner-engine
/*  div-table  %wasm  /tests/lib/wasm/del-table/wasm
/*  fac-loop   %wasm  /tests/lib/wasm/fac-br/wasm
/*  fib-rust   %wasm  /tests/lib/wasm/fib/wasm
/*  fac-if     %wasm  /tests/lib/wasm/fac/wasm
/*  two-func   %wasm  /tests/lib/wasm/two-functions/wasm
/*  flopper    %wasm  /tests/lib/wasm/flopper/wasm
/*  import     %wasm  /tests/lib/wasm/import/wasm
/*  printf     %wasm  /tests/lib/wasm/printf/wasm
/*  br-table   %wasm  /tests/lib/wasm/br-table1/wasm
::
|%
++  test-table
  ::  Test table section and indirect calls
  ::
  %+  expect-eq
    !>  `(list coin-wasm:wasm-sur)`~[[type=%f32 n=.5.5]]
    !>
    =<  -  %-  wasm-need:engine
    %^  invoke:engine  'testdivtable'  ~[[%f32 .11] [%f32 .2]]
    +:(wasm-need:engine (prep:engine (main:parser div-table) ~))
::
++  test-loop
  ::  Test correct loop execution
  ::
  %+  expect-eq
    !>  `(list coin-wasm:wasm-sur)`~[[type=%i32 n=362.880]]
    !>
    =<  -  %-  wasm-need:engine
    %^  invoke:engine  'factorial'  ~[[%i32 9]]
    +:(wasm-need:engine (prep:engine (main:parser fac-loop) ~))
::
++  test-rust
  ::  Test a module obtained from wasm-pack utility in Rust
  ::
  %+  expect-eq
    !>  `(list coin-wasm:wasm-sur)`~[[type=%i32 n=102.334.155]]
    !>
    =<  -  %-  wasm-need:engine
    %^  invoke:engine  'fib'  ~[[%i32 40]]
    +:(wasm-need:engine (prep:engine (main:parser fib-rust) ~))
::
++  test-if
  ::  Test if branching
  ::
  %+  expect-eq
    !>  `(list coin-wasm:wasm-sur)`~[[type=%f64 n=.~362880]]
    !>
    =<  -  %-  wasm-need:engine
    %^  invoke:engine  'fac'  ~[[%f64 .~9]]
    +:(wasm-need:engine (prep:engine (main:parser fac-if) ~))
::
++  test-two-functions
  ::  Test nested function calls
  ::
  %+  expect-eq
    !>  `(list coin-wasm:wasm-sur)`~[[type=%i32 n=43]]
    !>
    =<  -  %-  wasm-need:engine
    %^  invoke:engine  'addTwo'  ~[[%i32 21] [%i32 21]]
    +:(wasm-need:engine (prep:engine (main:parser two-func) ~))
::
::
++  test-flopper
  ::  Test a function that flops a string
  ::
  %+  expect-eq
    !>  `tape`(flop (gulf 'a' 'z'))
    !>
    =/  string-in=tape  (gulf 'a' 'z')
    =+  st=+:(wasm-need:engine (prep:engine (main:parser flopper) ~))
    =^  out=(list coin-wasm:wasm-sur)  st
      %-  wasm-need:engine
      %^  invoke:engine  '__wbindgen_add_to_stack_pointer'
      ~[[%i32 (en-si:op-def 32 -16)]]  st
    =/  retptr=@  ?>(?=([[%i32 n=@] ~] out) n.i.out)
    =^  out=(list coin-wasm:wasm-sur)  st
      %-  wasm-need:engine
      %^  invoke:engine  '__wbindgen_malloc'
      ~[[%i32 (lent string-in)] [%i32 1]]  st
    =/  ptr0=@  ?>(?=([[%i32 n=@] ~] out) n.i.out)
    =/  len0=@  (lent string-in)
    ?>  ?=(^ mem.st)
    =.  buffer.u.mem.st
      (sew bloq=3 [ptr0 size=len0 (crip string-in)] buffer.u.mem.st)
    =>  .(st `store:engine-sur`st)
    =.  st
      =<  +  %-  wasm-need:engine
      %^  invoke:engine  'process'
      ~[[%i32 retptr] [%i32 ptr0] [%i32 len0]]  st
    ?>  ?=(^ mem.st)
    =/  r0=@  (cut 3 [retptr 4] buffer.u.mem.st)
    =/  r1=@  (cut 3 [(add retptr 4) 4] buffer.u.mem.st)
    =/  string-out=tape
      %-  trip
      (cut 3 [r0 r1] buffer.u.mem.st)
    string-out
  ::
++  test-import
  %+  expect-eq
    !>  `(list coin-wasm:wasm-sur)`~[[type=%i32 n=43]]
    !>
    |^
    ~!  import
    =+  st=+:(wasm-need:engine (prep:engine (main:parser import) ~))
    =<  -
    |-  ^-  (quip coin-wasm:wasm-sur store:engine-sur)
    =+  r=(invoke:engine 'succ' ~ st)
    ?-  -.r
      %0  +.r
      %1  $(shop.st [(resolve +.r) shop.st])
      %2  !!
    ==
    ::
    ++  resolve
      |=  $:  [[mod=cord name=cord] =request:engine-sur]
              =module:engine-sur
              mem=(unit [buffer=@ n-pages=@])
              tables=(list (list $>(%ref coin-wasm:wasm-sur)))
              globals=(list coin-wasm:wasm-sur)
          ==
      ^-  item:engine-sur
      ?>  =(['./import_test_bg.js' '__wbg_getn_e182583a43d51902'] [mod name])
      ?>  ?=(%func -.request)
      ?>  ?=(~ args.request)
      [[%i32 42]~ [module mem tables globals]]
    ::
    --
::
++  test-import-2
  %+  expect-eq
    !>  `(list coin-wasm:wasm-sur)`~[[type=%i32 n=84]]
    !>
    |^
    =+  st=+:(wasm-need:engine (prep:engine (main:parser import) ~))
    =<  -
    |-  ^-  (quip coin-wasm:wasm-sur store:engine-sur)
    =+  r=(invoke:engine 'add' ~ st)
    ?-  -.r
      %0  +.r
      %1  $(shop.st [(resolve +.r) shop.st])
      %2  !!
    ==
    ::
    ++  resolve
      |=  $:  [[mod=cord name=cord] =request:engine-sur]
              =module:engine-sur
              mem=(unit [buffer=@ n-pages=@])
              tables=(list (list $>(%ref coin-wasm:wasm-sur)))
              globals=(list coin-wasm:wasm-sur)
          ==
      ^-  item:engine-sur
      ?>  =(['./import_test_bg.js' '__wbg_getn_e182583a43d51902'] [mod name])
      ?>  ?=(%func -.request)
      ?>  ?=(~ args.request)
      [[%i32 42]~ [module mem tables globals]]
    ::
    --
::
++  test-hi
  %+  expect-eq
    !>  `(list coin-wasm:wasm-sur)`~
    !>
    |^
    =+  st=+:(wasm-need:engine (prep:engine (main:parser printf) ~))
    =<  -
    |-  ^-  (quip coin-wasm:wasm-sur store:engine-sur)
    =+  r=(invoke:engine 'writeHi' ~ st)
    ?-  -.r
      %0  +.r
      %1  $(shop.st [(resolve +.r) shop.st])
      %2  !!
    ==
    ::
    ++  resolve
      |=  $:  [[mod=cord name=cord] =request:engine-sur]
              =module:engine-sur
              mem=(unit [buffer=@ n-pages=@])
              tables=(list (list $>(%ref coin-wasm:wasm-sur)))
              globals=(list coin-wasm:wasm-sur)
          ==
      ^-  item:engine-sur
      ?>  =(['console' 'log'] [mod name])
      ?>  ?=(%func -.request)
      =/  args-pole=(pole coin-wasm:wasm-sur)  args.request
      ?>  ?=([[%i32 off=@] [%i32 len=@] ~] args-pole)
      ?>  ?=(^ mem)
      =,  args-pole
      ~&  >>  `cord`(cut 3 [off len] buffer.u.mem)
      [~ [module mem tables globals]]
    ::
    --
::
++  test-validator
  %+  expect-eq
    !>  [%& ~]
    !>
    (validate-module:validator (main:parser br-table))
--