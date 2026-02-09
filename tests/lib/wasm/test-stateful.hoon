/+  *test
/+  wasm-lib=wasm-lia
/*  parser-bin  %wasm  /tests/lib/wasm/wat2wasm/wasm
::
=/  lv  lia-value:lia-sur:wasm-lib
=/  cw  coin-wasm:wasm-sur:wasm-lib
=/  import  import:lia-sur:wasm-lib
=/  script  script:lia-sur:wasm-lib
=/  arr  ^?((arrows:wasm-lib vase))
=/  seed  seed:lia-sur:wasm-lib
=/  wasm  ^?(wasm-lib)
::
=>  |%
    ++  i8neg   ^~((cury sub (bex 8)))
    ++  i16neg  ^~((cury sub (bex 16)))
    ++  i32neg  ^~((cury sub (bex 32)))
    ++  i64neg  ^~((cury sub (bex 64)))
    --
=/  parser
  |=  string-in=tape
  ^-  octs
  ?>  (levy string-in (curr lte 0x7f))
  =;  out=(list lv)
    ?>  ?=([[%octs *] ~] out)
    +.i.out
  %-  yield-need:wasm  =<  -
  %^  (run-once:wasm (list lv) vase)  [parser-bin *vase^~]  %$
  =,  arr
  =/  m  runnable:wasm
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
=/  bin-continue
  %-  parser
  """
  (module
    (import "env" "print-i32" (func $print-i32 (param i32)))
    (func
      (export "print")
      (param i32)
    ;;
      (call $print-i32 (local.get 0))
    )
  )
  """
::
=/  bin-empty  (parser "(module)")
=/  bin-import-ext
  %-  parser
  """
  (module
  ;;
    (import "env" "read-from-world" (func $read-from-world (result i32 i64)))
  ;;
    (func (export "foo") (result i32 i64) (call $read-from-world))
  )
  """
::
=/  bin-import-ext2
  %-  parser
  """
  (module
  ;;
    (import "env" "read-from-world" (func $read-from-world (result i32 i64)))
    (import "env" "import-to-call" (func $import-to-call (result i32 i64)))
  ;;
    (func (export "foo") (result i32 i64) (call $import-to-call))
    (func (export "export-to-call") (result i32 i64) (call $read-from-world))
  )
  """
::
|%
++  test-continue
  %+  expect-eq
    !>  &
    !>
    =/  print
      |=  a=@
      =/  m  runnable:wasm
      ^-  form:m
      =,  arr
      ;<  *  try:m  (call 'print' a ~)
      (return:m ~)
    ::
    =/  =(import vase)
      :-  *vase
      =/  m  (script (list cw) vase)
      %-  malt
      :~
        :-  'env'^'print-i32'
        |=  args=(pole cw)
        ^-  form:m
        ?>  ?=([[%i32 a=@] ~] args)
        =,  arr  =,  args
        ~&  a
        (return:m ~)
      ==
    ::
    =/  =seed  [bin-continue (return:runnable:wasm ~) ~ import]
    =/  hint=@tas  %omit
    =/  sed-1
      =^  *  seed  (run:wasm &+(print 1) seed hint)
      =^  *  seed  (run:wasm &+(print 2) seed hint)
      =^  *  seed  (run:wasm &+(print 3) seed hint)
      =^  *  seed  (run:wasm &+(print 4) seed hint)
      seed
    ::
    =.  hint  %gent
    =/  sed-2
      =^  *  seed  (run:wasm &+(print 1) seed hint)
      =^  *  seed  (run:wasm &+(print 2) seed hint)
      =^  *  seed  (run:wasm &+(print 3) seed hint)
      =^  *  seed  (run:wasm &+(print 4) seed hint)
      seed
    ::
    =.  hint  %rand
    =/  sed-3
      =^  *  seed  (run:wasm &+(print 1) seed hint)
      =^  *  seed  (run:wasm &+(print 2) seed hint)
      =^  *  seed  (run:wasm &+(print 3) seed hint)
      =^  *  seed  (run:wasm &+(print 4) seed hint)
      seed
    ::
    =.  hint  %oust
    =/  sed-4
      =^  *  seed  (run:wasm &+(print 1) seed hint)
      =^  *  seed  (run:wasm &+(print 2) seed hint)
      =^  *  seed  (run:wasm &+(print 3) seed hint)
      =^  *  seed  (run:wasm &+(print 4) seed hint)
      seed
    ::
    ::  repeat
    =.  hint  %omit
    =/  sed-1r
      =^  *  seed  (run:wasm &+(print 1) seed hint)
      =^  *  seed  (run:wasm &+(print 2) seed hint)
      =^  *  seed  (run:wasm &+(print 3) seed hint)
      =^  *  seed  (run:wasm &+(print 4) seed hint)
      seed
    ::
    =.  hint  %gent
    =/  sed-2r
      =^  *  seed  (run:wasm &+(print 1) seed hint)
      =^  *  seed  (run:wasm &+(print 2) seed hint)
      =^  *  seed  (run:wasm &+(print 3) seed hint)
      =^  *  seed  (run:wasm &+(print 4) seed hint)
      seed
    ::
    =.  hint  %rand
    =/  sed-3r
      =^  *  seed  (run:wasm &+(print 1) seed hint)
      =^  *  seed  (run:wasm &+(print 2) seed hint)
      =^  *  seed  (run:wasm &+(print 3) seed hint)
      =^  *  seed  (run:wasm &+(print 4) seed hint)
      seed
    ::
    =.  hint  %oust
    =/  sed-4r
      =^  *  seed  (run:wasm &+(print 1) seed hint)
      =^  *  seed  (run:wasm &+(print 2) seed hint)
      =^  *  seed  (run:wasm &+(print 3) seed hint)
      =^  *  seed  (run:wasm &+(print 4) seed hint)
      seed
    ::
    =.  hint  %none
    =/  sed-n
      =^  *  seed  (run:wasm &+(print 1) seed hint)
      =^  *  seed  (run:wasm &+(print 2) seed hint)
      =^  *  seed  (run:wasm &+(print 3) seed hint)
      =^  *  seed  (run:wasm &+(print 4) seed hint)
      seed
    ::
    ?&  =(sed-1 sed-1r)
        =(sed-2 sed-2r)
        =(sed-3 sed-3r)
        =(sed-4 sed-4r)
        =(sed-1 sed-2)
        =(sed-3 sed-4)
        =(sed-1 sed-3)
        =(sed-1 sed-n)
    ==
::
++  test-ext-naive
  %+  expect-eq
    !>  &
    !>
    =/  script
      =/  m  runnable:wasm
      ^-  form:m
      =,  arr
      ;<  res=(list lv)  try:m  (call-ext %resolve-me ~)
      ;<  res=(list lv)  try:m  (call-ext %resolve-me ~)
      (return:m res)
    ::
    =/  =seed  [bin-empty (return:runnable:wasm ~) ~ *vase^~]
    =/  hint=@tas  %omit
    =/  res-1
      =^  res  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-2
      =^  res  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-3
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-4
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    ::  repeat
    =.  hint  %omit
    =/  res-5
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-6
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-7
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-8
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %none
    =/  res-9
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    ?&  =(res-1 res-2)
        =(res-1 res-3)
        =(res-1 res-4)
        =(res-1 res-5)
        =(res-1 res-6)
        =(res-1 res-7)
        =(res-1 res-8)
        =(res-1 res-9)
    ==
::
++  test-reenter-once
   %+  expect-eq
    !>  &
    !>
    =/  =(import vase)
      :-  *vase
      =/  m  (script (list cw) vase)
      %-  malt
      :~
        :-  'env'^'read-from-world'
        |=  args=(pole cw)
        ^-  form:m
        =,  arr
        ;<  res=(list lv)  try:m  (call-ext %read-from-world ~)
        (return:m ;;((list cw) res))
      ==
    ::
    =/  script
      =/  m  runnable:wasm
      ^-  form:m
      =,  arr
      ;<  res=(list @)  try:m  (call 'foo' ~)
      (return:m i32+&1.res i64+&2.res ~)
    ::
    =/  =seed  [bin-import-ext (return:runnable:wasm ~) ~ import]
    =/  hint=@tas  %omit
    =/  res-1
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-2
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-3
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-4
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    ::  repeat
    =.  hint  %omit
    =/  res-5
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-6
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-7
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-8
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %none
    =/  res-9
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    ?&  =(res-1 res-2)
        =(res-1 res-3)
        =(res-1 res-4)
        =(res-1 res-5)
        =(res-1 res-6)
        =(res-1 res-7)
        =(res-1 res-8)
        =(res-1 res-9)
    ==
::
++  test-reenter-twice
   %+  expect-eq
    !>  &
    !>
    =/  =(import vase)
      :-  *vase
      =/  m  (script (list cw) vase)
      %-  malt
      :~
        :-  'env'^'read-from-world'
        |=  args=(pole cw)
        ^-  form:m
        =,  arr
        ;<  res=(list lv)  try:m  (call-ext %read-from-world ~)
        (return:m ;;((list cw) res))
      ::
        :-  'env'^'import-to-call'
        |=  args=(pole cw)
        ^-  form:m
        =,  arr
        ;<  res=(list @)  try:m  (call 'export-to-call' ~)
        (return:m ~[i32+&1.res i64+&2.res])
      ==
    ::
    =/  script
      =/  m  runnable:wasm
      ^-  form:m
      =,  arr
      ;<  res=(list @)  try:m  (call 'foo' ~)
      (return:m i32+&1.res i64+&2.res ~)
    ::
    =/  =seed  [bin-import-ext2 (return:runnable:wasm ~) ~ import]
    =/  hint=@tas  %omit
    =/  res-1
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-2
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-3
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-4
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    ::  repeat
    =.  hint  %omit
    =/  res-5
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-6
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-7
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-8
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %none
    =/  res-9
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    ?&  =(res-1 res-2)
        =(res-1 res-3)
        =(res-1 res-4)
        =(res-1 res-5)
        =(res-1 res-6)
        =(res-1 res-7)
        =(res-1 res-8)
        =(res-1 res-9)
    ==
::
++  test-urwasm-urwasm
  %+  expect-eq
    !>  &
    !>
    =/  =(import vase)
      :-  *vase
      =/  m  (script (list cw) vase)
      %-  malt
      :~
        :-  'env'^'read-from-world'
        |=  args=(pole cw)
        ^-  form:m
        =,  arr
        ;<  res=(list lv)  try:m  (call-ext %read-from-world ~)
        (return:m ;;((list cw) res))
      ::
        :-  'env'^'import-to-call'
        |=  args=(pole cw)
        ^-  form:m
        =,  arr
        ;<  res=(list @)  try:m  (call 'export-to-call' ~)
        (return:m ~[i32+&1.res i64+&2.res])
      ==
    ::
    =/  =seed  [bin-import-ext2 (return:runnable:wasm ~) ~ import]
    =/  script1
      =/  m  runnable:wasm
      ^-  form:m
      =,  arr
      ;<  res=(list @)  try:m  (call 'foo' ~)
      (return:m i32+&1.res i64+&2.res ~)
    ::
    =/  script2
      |=  hint=@tas
      =/  m  runnable:wasm
      ^-  form:m
      =,  arr
      =+  (run:wasm &+script1 seed hint)
      =>  +
      ;<  res=(list @)  try:m  (call 'foo' ~)
      (return:m i32+&1.res i64+&2.res ~)
    ::
    =/  hint=@tas  %omit
    =/  res-1
      =^  *  seed  (run:wasm &+(script2 hint) seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-2
      =^  *  seed  (run:wasm &+(script2 hint) seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-3
      =^  *  seed  (run:wasm &+(script2 hint) seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-4
      =^  *  seed  (run:wasm &+(script2 hint) seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    ::  repeat
    =.  hint  %omit
    =/  res-5
      =^  *  seed  (run:wasm &+(script2 hint) seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-6
      =^  *  seed  (run:wasm &+(script2 hint) seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-7
      =^  *  seed  (run:wasm &+(script2 hint) seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-8
      =^  *  seed  (run:wasm &+(script2 hint) seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    ?&  =(res-1 res-5)
        =(res-2 res-6)
        =(res-3 res-7)
        =(res-4 res-8)
    ==
::
++  test-simple
  %+  expect-eq
    !>  &
    !>
    =/  =seed  [bin-import-ext2 (return:runnable:wasm ~) ~ *vase^~]
    =/  script
      =/  m  runnable:wasm
      ^-  form:m
      =,  arr
      (call-ext %read-from-world ~)
    ::
    =/  hint=@tas  %omit
    =/  res-1
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-2
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-3
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-4
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    ::  repeat
    =.  hint  %omit
    =/  res-5
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-6
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-7
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-8
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    =.  hint  %none
    =/  res-9
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      [res seed]
    ::
    ?&  =(res-1 res-2)
        =(res-1 res-3)
        =(res-1 res-4)
        =(res-1 res-5)
        =(res-1 res-6)
        =(res-1 res-7)
        =(res-1 res-8)
        =(res-1 res-9)
    ==
++  test-ext-order
  %+  expect-eq
    !>  &
    !>
    =/  script
      =/  m  runnable:wasm
      ^-  form:m
      =,  arr
      ;<  res=(list lv)  try:m  (call-ext %resolve-me ~)
      ;<  res=(list lv)  try:m  (call-ext %resolve-me ~)
      (return:m res)
    ::
    =/  =seed  [bin-empty (return:runnable:wasm ~) ~ *vase^~]
    =/  hint=@tas  %omit
    =/  res-1
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  res  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-2
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  res  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-3
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-4
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    ::  repeat
    =.  hint  %omit
    =/  res-5
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %rand
    =/  res-6
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %gent
    =/  res-7
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %oust
    =/  res-8
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    =.  hint  %none
    =/  res-9
      =^  res  seed  (run:wasm |+~[i32+42 i64+121] seed hint)
      =^  *  seed  (run:wasm &+script seed hint)
      =^  res  seed  (run:wasm |+~[i32+11 i64+555] seed hint)
      [res seed]
    ::
    ?&  =(res-1 res-2)
        =(res-1 res-3)
        =(res-1 res-4)
        =(res-1 res-5)
        =(res-1 res-6)
        =(res-1 res-7)
        =(res-1 res-8)
        =(res-1 res-9)
    ==
::
--