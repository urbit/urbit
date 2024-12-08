::  An example of using urwasm in production: parsing of
::  .wat format
::
/+  wasm=wasm-lia
/*  bin  %wasm  /lib/wasm/tools/wat2wasm/wasm
::
=/  lv  lia-value:lia-sur:wasm
|=  string-in=tape
|^  ^-  octs
?>  (levy string-in (curr lte 0x7f))
=;  out=(list lv)
  ?>  ?=([[%octs *] ~] out)
  +.i.out
%-  yield-need:wasm
%^  (run-once:wasm (list lv))  [bin ~]  %$
=,  wasm
=/  m  runnable
;<  retptr=@  try:m  (call-1 '__wbindgen_add_to_stack_pointer' (i32neg 16) ~)
=/  len0=@  (lent string-in)
;<  ptr0=@    try:m  (call-1 '__wbindgen_malloc' len0 1 ~)
;<  ~         try:m  (memwrite ptr0 len0 (crip string-in))
;<  *         try:m  (call 'process' retptr ptr0 len0 ~)
;<  r0=octs   try:m  (memread retptr 4)
;<  r1=octs   try:m  (memread (add retptr 4) 4)
;<  r2=octs   try:m  (memread q.r0 q.r1)
(return:m octs+r2 ~)
::
++  i32neg  ^~((cury sub (bex 32)))
--