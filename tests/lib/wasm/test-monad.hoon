/+  *test
/+  wasm=wasm-lia
/*  import-vec  %wasm  /tests/lib/wasm/import-vec/wasm
/*  fac-loop    %wasm  /tests/lib/wasm/fac-br/wasm
/*  flopper     %wasm  /tests/lib/wasm/flopper/wasm
/*  fac-if      %wasm  /tests/lib/wasm/fac/wasm
::
=/  flag=@tas  %$
=/  lv  lia-value:lia-sur:wasm
=/  cw  coin-wasm:wasm-sur:wasm
|%
++  test-import-vec
  %+  expect-eq
    !>  `(list lv)`~[octs+[5 'olleh']]
    !>
    |^
    %-  yield-need:wasm  =<  -
    %^  (run-once:wasm (list lv) vase)  [import-vec import]  flag
    =/  m  runnable:wasm
    =/  arr  (arrows:wasm vase)
    =,  arr
    ;<  retptr=@  try:m  (call-1 '__wbindgen_add_to_stack_pointer' (i32neg 16) ~)
    ;<  *         try:m  (call 'process' retptr ~)
    ;<  r0=octs   try:m  (memread retptr 4)
    ;<  r1=octs   try:m  (memread (add retptr 4) 4)
    ;<  r2=octs   try:m  (memread q.r0 q.r1)
    (return:m octs+r2 ~)
    ::
    ++  i32neg  ^~((cury sub (bex 32)))
    ++  import
      ^-  (import:lia-sur:wasm vase)
      =/  m  (script:lia-sur:wasm (list cw) vase)
      :-  *vase
      %-  malt
      ^-  %-  list
          %+  pair  (pair cord cord)
          $-((list cw) form:m)
      :~
        :-  ['./len_bg.js' '__wbg_getvec_ab3ebae2a99ce16c']
        |=  args=(pole cw)
        ?>  ?=([[%i32 arg0=@] ~] args)
        =/  arg0=@  arg0.args
        =/  arr  (arrows:wasm vase)
        =,  arr
        ;<  ptr1=@  try:m  (call-1 '__wbindgen_malloc' 5 1 ~)
        ;<  *       try:m  (memwrite ptr1 5 'hello')
        ;<  *       try:m  (memwrite arg0 4 ptr1)
        ;<  *       try:m  (memwrite (add arg0 4) 4 5)
        (return:m ~)
      ::
      ==
    --
::
++  test-simple
  %+  expect-eq
    !>  `(list lv)`~[i32+362.880]
    !>
    %-  yield-need:wasm  =<  -
    %^  (run-once:wasm (list lv) vase)  [fac-loop *vase^~]  flag
    =/  m  runnable:wasm
    =/  arr  (arrows:wasm vase)
    =,  arr
    ;<  out=@  try:m  (call-1 'factorial' 9 ~)
    (return:m i32+out ~)
::
++  test-flop
  %+  expect-eq
    !>  `(list lv)`~[octs+[5 'olleh']]
    !>
    |^
    %-  yield-need:wasm  =<  -
    %^  (run-once:wasm (list lv) vase)  [flopper *vase^~]  flag
    =/  m  runnable:wasm
    =/  arr  (arrows:wasm vase)
    =,  arr
    ;<  retptr=@  try:m  (call-1 '__wbindgen_add_to_stack_pointer' (i32neg 16) ~)
    =/  src=@  'hello'
    =/  len0=@  (met 3 src)
    ;<  ptr0=@    try:m  (call-1 '__wbindgen_malloc' len0 1 ~)
    ;<  *         try:m  (memwrite ptr0 len0 src)
    ;<  *         try:m  (call 'process' retptr ptr0 len0 ~)
    ;<  r0=octs   try:m  (memread retptr 4)
    ;<  r1=octs   try:m  (memread (add 4 retptr) 4)
    ;<  r2=octs   try:m  (memread q.r0 q.r1)
    (return:m octs+r2 ~)
    ::
    ++  i32neg  ^~((cury sub (bex 32)))
    --
::
++  test-1
  %+  expect-eq
    !>  `(list lv)`~[i32+42]
    !>
    %-  yield-need:wasm  =<  -
    %^  (run-once:wasm (list lv) vase)  [flopper *vase^~]  flag
    =/  m  runnable:wasm
    =/  arr  (arrows:wasm vase)
    =,  arr
    ;<  ptr0=@  try:m  (call-1 '__wbindgen_malloc' 5 1 ~)
    (return:m i32+42 ~)
::
++  test-2
  %+  expect-eq
    !>  `(list lv)`~[f64+.~362880]
    !>
    %-  yield-need:wasm  =<  -
    %^  (run-once:wasm (list lv) vase)  [fac-if *vase^~]  flag
    =/  m  runnable:wasm
    =/  arr  (arrows:wasm vase)
    =,  arr
    ;<  out=@  try:m  (call-1 'fac' .~9 ~)
    (return:m f64+out ~)
::
++  test-import-vec-acc
  %+  expect-eq
    !>  [~[octs+[5 'olleh']] 42]
    !>
    |^
    =;  res
      [(yield-need:wasm -.res) +.res]
    %^  (run-once:wasm (list lv) @)  [import-vec import]  flag
    =/  m  (script:lia-sur:wasm (list lv) @)
    =/  arr  (arrows:wasm @)
    =,  arr
    ;<  retptr=@  try:m  (call-1 '__wbindgen_add_to_stack_pointer' (i32neg 16) ~)
    ;<  *         try:m  (call 'process' retptr ~)
    ;<  r0=octs   try:m  (memread retptr 4)
    ;<  r1=octs   try:m  (memread (add retptr 4) 4)
    ;<  r2=octs   try:m  (memread q.r0 q.r1)
    ;<  a=@       try:m  get-acc
    ~&  a
    (return:m octs+r2 ~)
    ::
    ++  i32neg  ^~((cury sub (bex 32)))
    ++  import
      ^-  (import:lia-sur:wasm @)
      =/  m  (script:lia-sur:wasm (list cw) @)
      :-  0
      %-  malt
      ^-  %-  list
          %+  pair  (pair cord cord)
          $-((list cw) form:m)
      :~
        :-  ['./len_bg.js' '__wbg_getvec_ab3ebae2a99ce16c']
        |=  args=(pole cw)
        ?>  ?=([[%i32 arg0=@] ~] args)
        =/  arg0=@  arg0.args
        =/  arr  (arrows:wasm @)
        =,  arr
        ;<  ptr1=@  try:m  (call-1 '__wbindgen_malloc' 5 1 ~)
        ;<  *       try:m  (memwrite ptr1 5 'hello')
        ;<  *       try:m  (memwrite arg0 4 ptr1)
        ;<  *       try:m  (memwrite (add arg0 4) 4 5)
        ;<  *       try:m  (set-acc 42)
        (return:m ~)
      ::
      ==
    --
--