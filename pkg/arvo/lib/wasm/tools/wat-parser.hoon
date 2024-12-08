::  An example of using urwasm in production: parsing of
::  .wat format
::
/+  *runner-engine
/*  bin  %wasm  /lib/tools/wat2wasm/wasm
::
|=  string-in=tape
^-  module:wasm-sur
::  check text correctness to avoid having to call realloc
::
?>  (levy string-in (curr lte 0x7f))
~&  'parse wat2wasm module'
=+  m=~>(%bout (main:parser bin))
~&  'instantiate'
=+  st=+:(wasm-need:engine (prep:engine m ~))
=^  out=(list coin-wasm:wasm-sur)  st
  %-  wasm-need:engine
  ~&  'invoke:engine add-to-stack-pointer'
  %^  invoke:engine  '__wbindgen_add_to_stack_pointer'
  ~[[%i32 (en-si:op-def 32 -16)]]  st
=/  retptr=@  ?>(?=([[%i32 n=@] ~] out) n.i.out)
=^  out=(list coin-wasm:wasm-sur)  st
  %-  wasm-need:engine
  ~&  'invoke:engine malloc'
  %^  invoke:engine  '__wbindgen_malloc'
  ~[[%i32 (lent string-in)] [%i32 1]]  st
=/  ptr0=@  ?>(?=([[%i32 n=@] ~] out) n.i.out)
=/  len0=@  (lent string-in)
?>  ?=(^ mem.st)
=.  buffer.u.mem.st
  ~&  'memory write'
  (sew bloq=3 [ptr0 size=len0 (crip string-in)] buffer.u.mem.st)
=>  .(st `store:engine-sur`st)
=.  st
  =<  +  %-  wasm-need:engine
  ~&  'invoke:engine process'
  ~>  %bout
  %^  invoke:engine  'process'
  ~[[%i32 retptr] [%i32 ptr0] [%i32 len0]]  st
?>  ?=(^ mem.st)
~&  'memory read'
=/  r0=@  (cut 3 [retptr 4] buffer.u.mem.st)
=/  r1=@  (cut 3 [(add retptr 4) 4] buffer.u.mem.st)
=/  module-out=octs
  ~&  'memory read'
  [r1 (cut 3 [r0 r1] buffer.u.mem.st)]
~&  '.wasm -> noun parsing'
(main:parser module-out)