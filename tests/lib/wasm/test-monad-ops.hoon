/+  *test
/+  wasm-lib=wasm-lia
/*  parser-bin  %wasm  /tests/lib/wasm/wat2wasm/wasm
::
=/  lv  lia-value:lia-sur:wasm-lib
=/  cw  coin-wasm:wasm-sur:wasm-lib
=/  import  import:lia-sur:wasm-lib
=/  script  script:lia-sur:wasm-lib
=/  arr  ^?((arrows:wasm-lib vase))
=/  wasm  ^?(wasm-lib)
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
=>
  =/  print-time=?  |
  |%
  ::
  ++  run-once-comp
    =/  m  runnable:wasm
    |=  [sed=[module=octs =(import vase)] script=form:m]
    ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ?.  print-time
      =/  nock  -:((run-once:wasm (list lv) vase) sed %none script)
      =/  fast  -:((run-once:wasm (list lv) vase) sed %$ script)
      ?:  =(nock fast)  &+~
      [%| nock+nock fast+fast]
    =/  nock
      ~&  %nock
      ~>  %bout
      -:((run-once:wasm (list lv) vase) sed %none script)
    =/  fast
      ~&  %fast
      ~>  %bout
      -:((run-once:wasm (list lv) vase) sed %$ script)
    ?:  =(nock fast)  &+~
    [%| nock+nock fast+fast]
  ::
  ++  run-once-comp-list
    =/  m  runnable:wasm
    |=  [sed=[module=octs =(import vase)] scripts=(list form:m)]
    =/  script
      =/  m  (script:lia-sur:wasm-lib (list (list lv)) vase)
      =|  out=(list (list lv))
      |-  ^-  form:m
      ?~  scripts  (return:m out)
      ;<  res=(list lv)  try:m  i.scripts
      $(out [res out], scripts t.scripts)
    ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ?.  print-time
      =/  nock  -:((run-once:wasm (list (list lv)) vase) sed %none script)
      =/  fast  -:((run-once:wasm (list (list lv)) vase) sed %$ script)
      ?>  ?=(%0 -.nock)
      ?>  ?=(%0 -.fast)
      |-
      ?:  =(~ p.nock)  &+~
      ?.  =(-.p.nock -.p.fast)  [%| nock+0+,.-.p.nock fast+0+,.-.p.fast]
      $(p.nock +.p.nock, p.fast +.p.fast)
    =/  nock
      ~&  %nock
      ~>  %bout
      -:((run-once:wasm (list (list lv)) vase) sed %none script)
    =/  fast
      ~&  %fast
      ~>  %bout
      -:((run-once:wasm (list (list lv)) vase) sed %$ script)
    ?>  ?=(%0 -.nock)
    ?>  ?=(%0 -.fast)
    |-
    ?:  =(~ p.nock)  &+~
    ?.  =(-.p.nock -.p.fast)  [%| nock+0+,.-.p.nock fast+0+,.-.p.fast]
    $(p.nock +.p.nock, p.fast +.p.fast)
  ::
  ++  m-inputs
    ^~
    ^-  (map tape (list @))
    %-  malt
    ^-  (list [tape (list @)])
    :~
      :-  "f32"
      ^-  (list @rs)
      :~  .nan
          +(.nan)
          (con .nan (bex 31))  ::  .-nan
          +((con .nan (bex 31)))
          .inf
          .-inf
          .8.589935e9
          .-8.589935e9
          .3.689349e19
          .-3.689349e19
          .1000.5
          .1000.7
          .1000.3
          .-1000.5
          .-1000.7
          .-1000.3
          .1
          .-1
          .1.1
          .-1.1
          .0.7
          .0.5
          .0.3
          .-0.7
          .-0.5
          .-0.3
          .0
          .-0
          `@rs`1
          ^~((con 1 (bex 31)))
          ^~((bex 22))
          ^~((con (bex 22) (bex 31)))
          ^~((dec (bex 23)))
          ^~((con (dec (bex 23)) (bex 31)))
          ^~((bex 23))
          ^~((con (bex 23) (bex 31)))
          ^~((con (lsh [0 23] 254) (dec (bex 23))))
          ^~(:(con (lsh [0 23] 254) (dec (bex 23)) (bex 31)))
      ==
    ::
      :-  "f64"
      ^-  (list @rd)
      :~  .~nan
          +(.~nan)
          (con .nan (bex 63))  ::  .~-nan
          +((con .nan (bex 63)))
          .~inf
          .~-inf
          .~8589934592
          .~-8589934592
          .~3.6893488147419103e19
          .~-3.6893488147419103e19
          .~1000.5
          .~1000.7
          .~1000.3
          .~-1000.5
          .~-1000.7
          .~-1000.3
          .~1
          .~-1
          .~1.1
          .~-1.1
          .~0.7
          .~0.5
          .~0.3
          .~-0.7
          .~-0.5
          .~-0.3
          .~0
          .~-0
          `@rd`1
          ^~((con 1 (bex 63)))
          ^~((bex 51))
          ^~((con (bex 51) (bex 63)))
          ^~((dec (bex 52)))
          ^~((con (dec (bex 52)) (bex 63)))
          ^~((bex 52))
          ^~((con (bex 52) (bex 63)))
          ^~((con (lsh [0 52] (sub (bex 11) 2)) (dec (bex 52))))
          ^~(:(con (lsh [0 52] (sub (bex 11) 2)) (dec (bex 52)) (bex 63)))
      ==
    ::
      :-  "i32"
      ^-  (list @)
      :~
        (dec (bex 32))
        (bex 31)
        (dec (bex 31))
        0
        1
        2
        1.000
        (bex 16)
        (dec (bex 16))
        (sub (bex 16) 2)
        2
        (i32neg 1)
        (i32neg 1.000)
        (i32neg (bex 16))
        (i32neg (dec (bex 16)))
        (i32neg (sub (bex 16) 2))
      ==
    ::
      :-  "i64"
      ^-  (list @)
      :~
        (dec (bex 64))
        (bex 63)
        (dec (bex 63))
        0
        1
        2
        1.000
        (bex 32)
        (dec (bex 32))
        (sub (bex 32) 2)
        (i64neg 1)
        (i64neg 2)
        (i64neg 1.000)
        (i64neg (bex 32))
        (i64neg (dec (bex 32)))
        (i64neg (sub (bex 32) 2))
      ==
    ::
      :-  "i8"
      ^-  (list @)
      :~
        (dec (bex 8))
        (bex 7)
        (dec (bex 7))
        0
        1
        2
        100
        (bex 4)
        (dec (bex 4))
        (sub (bex 4) 2)
        (i8neg 1)
        (i8neg 2)
        (i8neg 100)
        (i8neg (bex 4))
        (i8neg (dec (bex 4)))
        (i8neg (sub (bex 4) 2))
      ==
    ::
      :-  "i16"
      ^-  (list @)
      :~
        (dec (bex 16))
        (bex 15)
        (dec (bex 15))
        0
        1
        2
        1.000
        (bex 8)
        (dec (bex 8))
        (sub (bex 8) 2)
        (i16neg 1)
        (i16neg 2)
        (i16neg 1.000)
        (i16neg (bex 8))
        (i16neg (dec (bex 8)))
        (i16neg (sub (bex 8) 2))
      ==
    ==
  --
|%
++  test-trunc-convert
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  from=(list tape)  ~["f32" "f64"]
    |-  =*  from-loop  $
    ?~  from  &+~
    =/  to=(list tape)  ~["i32" "i64"]
    |-  =*  to-loop  $
    ?~  to  from-loop(from t.from)
    =/  sign=(list tape)  ~["u" "s"]
    |-  =*  sign-loop  $
    ?~  sign  to-loop(to t.to)
    =/  binary=octs  (bin-trunc-convert i.from i.to i.sign)
    =/  input=(list @)  (~(got by m-inputs) i.from)
    |-  =*  input-loop  $
    ?~  input  sign-loop(sign t.sign)
    ::
    =;  res=(each ~ [[%nock yield:m] [%fast yield:m]])
      ?:  ?=(%| -.res)  res
      input-loop(input t.input)
    %+  run-once-comp  [binary *vase^~]
    =/  m  runnable:wasm
    =,  arr
    ;<  a=@  try:m  (call-1 'test' i.input ~)
    (return:m ;;(lv [(crip i.to) a]) ~)
    ::
    ++  bin-trunc-convert
      |=  [from=tape to=tape sign=tape]
      ^-  octs
      =/  op=tape  "{to}.trunc_sat_{from}_{sign}"
      %-  parser
      """
      (module
        (func (export "test") (param {from}) (result {to})
          local.get 0
          {op}
      ))
      """
    ::
    --
:: ::
++  test-ceil-floor-trunc-near
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  op=(list tape)  ~["ceil" "floor" "trunc" "nearest"]
    :: =/  op=(list tape)  ~["nearest"]
    |-  =*  op-loop  $
    ?~  op  &+~
    =/  type=(list tape)  ~["f32" "f64"]
    |-  =*  type-loop  $
    ?~  type  op-loop(op t.op)
    =/  binary=octs  (bin i.op i.type)
    =/  input=(list @)  (~(got by m-inputs) i.type)
    |-  =*  input-loop  $
    ?~  input  type-loop(type t.type)
    ::
    =;  res=(each ~ [[%nock yield:m] [%fast yield:m]])
      ?:  ?=(%| -.res)  res
      input-loop(input t.input)
    %+  run-once-comp  [binary *vase^~]
    =/  m  runnable:wasm
    =,  arr
    ;<  a=@  try:m  (call-1 'test' i.input ~)
    (return:m ;;(lv [(crip i.type) a]) ~)
    ::
    ++  bin
      |=  [op=tape type=tape]
      ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param {type}) (result {type})
          local.get 0
          {type}.{op}
      ))
      """
    --
::
++  test-promote
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  input=(list @)  (~(got by m-inputs) "f32")
    |-  =*  input-loop  $
    ?~  input  &+~
    ::
    =;  res=(each ~ [[%nock yield:m] [%fast yield:m]])
      ?:  ?=(%| -.res)  res
      input-loop(input t.input)
    %+  run-once-comp  [bin *vase^~]
    =/  m  runnable:wasm
    =,  arr
    ;<  a=@  try:m  (call-1 'test' i.input ~)
    (return:m [%f64 a] ~)
    ::
    ++  bin
      ^~  ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param f32) (result f64)
          local.get 0
          f64.promote_f32
      ))
      """
    --
::
++  test-demote
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  input=(list @)  (~(got by m-inputs) "f64")
    |-  =*  input-loop  $
    ?~  input  &+~
    ::
    =;  res=(each ~ [[%nock yield:m] [%fast yield:m]])
      ?:  ?=(%| -.res)  res
      input-loop(input t.input)
    %+  run-once-comp  [bin *vase^~]
    =/  m  runnable:wasm
    =,  arr
    ;<  a=@  try:m  (call-1 'test' i.input ~)
    (return:m [%f32 a] ~)
    ::
    ++  bin
      ^~  ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param f64) (result f32)
          local.get 0
          f32.demote_f64
      ))
      """
    --
::
++  test-convert
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  from=(list tape)  ~["i32" "i64"]
    |-  =*  from-loop  $
    ?~  from  &+~
    =/  to=(list tape)  ~["f32" "f64"]
    |-  =*  to-loop  $
    ?~  to  from-loop(from t.from)
    =/  sign=(list tape)  ~["s" "u"]
    |-  =*  sign-loop  $
    ?~  sign  to-loop(to t.to)
    =/  binary=octs  (bin i.from i.to i.sign)
    =/  input=(list @)  (~(got by m-inputs) i.from)
    |-  =*  input-loop  $
    ?~  input  sign-loop(sign t.sign)
    ::
    =;  res=(each ~ [[%nock yield:m] [%fast yield:m]])
      ?:  ?=(%| -.res)  res
      input-loop(input t.input)
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ;<  a=@  try:m  (call-1 'test' i.input ~)
    (return:m ;;(lv [(crip i.to) a]) ~)
    ::
    ++  bin
      |=  [from=tape to=tape sign=tape]
      ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param {from}) (result {to})
          local.get 0
          {to}.convert_{from}_{sign}
      ))
      """
    --
::
++  test-clz-ctz-popcnt
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  ops=(list tape)  ~["clz" "ctz" "popcnt"]
    |-  =*  ops-loop  $
    ?~  ops  &+~
    =/  type=(list tape)  ~["i32" "i64"]
    |-  =*  type-loop  $
    ?~  type  ops-loop(ops t.ops)
    =/  binary=octs  (bin i.ops i.type)
    =/  input=(list @)  (~(got by m-inputs) i.type)
    |-  =*  input-loop  $
    ?~  input  type-loop(type t.type)
    ::
    =;  res=(each ~ [[%nock yield:m] [%fast yield:m]])
      ?:  ?=(%| -.res)  res
      input-loop(input t.input)
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ;<  a=@  try:m  (call-1 'test' i.input ~)
    (return:m ;;(lv [(crip i.type) a]) ~)
    ::
    ++  bin
      |=  [op=tape type=tape]
      ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param {type}) (result {type})
          local.get 0
          {type}.{op}))
      """
    --
::
++  test-abs-neg
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  ops=(list tape)  ~["abs" "neg"]
    |-  =*  ops-loop  $
    ?~  ops  &+~
    =/  type=(list tape)  ~["f32" "f64"]
    |-  =*  type-loop  $
    ?~  type  ops-loop(ops t.ops)
    =/  binary=octs  (bin i.ops i.type)
    =/  input=(list @)  (~(got by m-inputs) i.type)
    |-  =*  input-loop  $
    ?~  input  type-loop(type t.type)
    ::
    =;  res=(each ~ [[%nock yield:m] [%fast yield:m]])
      ?:  ?=(%| -.res)  res
      input-loop(input t.input)
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ;<  a=@  try:m  (call-1 'test' i.input ~)
    (return:m ;;(lv [(crip i.type) a]) ~)
    ::
    ++  bin
      |=  [op=tape type=tape]
      ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param {type}) (result {type})
          local.get 0
          {type}.{op}))
      """
    --
::
++  test-extend-convert
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    =/  input=(list @)  (~(got by m-inputs) "i32")
    ::
    =/  sign=(list tape)  ~["u" "s"]
    |-  =*  sign-loop  $
    ?~  sign  &+~
    =/  binary=octs  (bin i.sign)
    |-  =*  input-loop  $
    ?~  input  sign-loop(sign t.sign)
    ::
    =;  res=(each ~ [[%nock yield:m] [%fast yield:m]])
      ?:  ?=(%| -.res)  res
      input-loop(input t.input)
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ;<  a=@  try:m  (call-1 'test' i.input ~)
    (return:m ;;(lv [%i64 a]) ~)
    ::
    ++  bin
      |=  sign=tape
      ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param i32) (result i64)
          local.get 0
          i64.extend_i32_{sign}))
      """
    --
::
++  test-extend
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  types=(list tape)  ~["i32" "i64"]
    |-  =*  types-loop  $
    ?~  types  &+~
    =/  width=(list tape)  ~["8" "16" "32"]
    |-  =*  width-loop  $
    ?~  width  types-loop(types t.types)
    ?:  &(=(i.width "32") =(i.types "i32"))
      width-loop(width t.width)
    =/  binary=octs  (bin i.types i.width)
    =/  input=(list @)  (~(got by m-inputs) "i{i.width}")
    |-  =*  input-loop  $
    ?~  input  width-loop(width t.width)
    ::
    =;  res=(each ~ [[%nock yield:m] [%fast yield:m]])
      ?:  ?=(%| -.res)  res
      input-loop(input t.input)
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ;<  a=@  try:m  (call-1 'test' i.input ~)
    (return:m ;;(lv [(crip i.types) a]) ~)
    ::
    ++  bin
      |=  [type=tape width=tape]
      ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param {type}) (result {type})
          local.get 0
          {type}.extend{width}_s))
      """
    --
::
++  test-reinterpret
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  from-to=(list (pair tape tape))
      ~["i32"^"f32" "i64"^"f64" "f32"^"i32" "f64"^"i64"]
    |-  =*  from-to-loop  $
    ?~  from-to  &+~
    =/  binary=octs  (bin i.from-to)
    =/  input=(list @)  (~(got by m-inputs) p.i.from-to)
    |-  =*  input-loop  $
    ?~  input  from-to-loop(from-to t.from-to)
    ::
    =;  res=(each ~ [[%nock yield:m] [%fast yield:m]])
      ?:  ?=(%| -.res)  res
      input-loop(input t.input)
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ;<  a=@  try:m  (call-1 'test' i.input ~)
    (return:m ;;(lv [(crip q.i.from-to) a]) ~)
    ::
    ++  bin
      |=  [from=tape to=tape]
      ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param {from}) (result {to})
          local.get 0
          {to}.reinterpret_{from}
      ))
      """
    --
::
++  test-rem-div
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  types=(list tape)  ~["i32" "i64"]
    |-  =*  types-loop  $
    ?~  types  &+~
    =/  signs=(list tape)  ~["s" "u"]
    |-  =*  signs-loop  $
    ?~  signs  types-loop(types t.types)
    =/  ops=(list tape)  ~["div" "rem"]
    |-  =*  ops-loop  $
    ?~  ops  signs-loop(signs t.signs)
    =/  binary=octs  (bin i.types i.signs i.ops)
    =/  input0=(list @)  (~(got by m-inputs) i.types)
    |-  =*  input0-loop  $
    ?~  input0  ops-loop(ops t.ops)
    =/  input1=(list @)  (~(got by m-inputs) i.types)
    |-  =*  input1-loop  $
    ?~  input1  input0-loop(input0 t.input0)
    ::
    =;  res=(each ~ [[%nock yield:m] [%fast yield:m]])
      ?:  ?=(%| -.res)  res
      input1-loop(input1 t.input1)
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ;<  a=@  try:m  (call-1 'test' i.input0 i.input1 ~)
    (return:m ;;(lv [(crip i.types) a]) ~)
    ::
    ++  bin
      |=  [type=tape sign=tape op=tape]
      ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param {type} {type}) (result {type})
          local.get 0
          local.get 1
          {type}.{op}_{sign}))
      """
    --
::
++  test-shl-rotl-rotr
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  types=(list tape)  ~["i32" "i64"]
    |-  =*  types-loop  $
    ?~  types  &+~
    =/  ops=(list tape)  ~["shl" "rotl" "rotr"]
    |-  =*  ops-loop  $
    ?~  ops  types-loop(types t.types)
    =/  binary=octs  (bin i.types i.ops)
    =/  input0=(list @)  (~(got by m-inputs) i.types)
    =/  input1=(list @)  (~(got by m-inputs) i.types)
    =;  scripts=(list form:m)
      (run-once-comp-list [binary *vase^~] scripts)
    =|  scripts=(list form:m)
    |-
    ?~  input0  scripts
    =/  input1-copy  input1
    |-
    ?~  input1  ^$(input0 t.input0, input1 input1-copy)
    :_  $(input1 t.input1)
    =/  m  runnable:wasm
    =,  arr
    ;<  a=@  try:m  (call-1 'test' i.input0 i.input1 ~)
    (return:m ;;(lv [(crip i.types) a]) ~)  
    ::
    ++  bin
      |=  [type=tape op=tape]
      ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param {type} {type}) (result {type})
          local.get 0
          local.get 1
          {type}.{op}))
      """
    --
::
++  test-shr
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  types=(list tape)  ~["i32" "i64"]
    |-  =*  types-loop  $
    ?~  types  &+~
    =/  signs=(list tape)  ~["s" "u"]
    |-  =*  signs-loop  $
    ?~  signs  types-loop(types t.types)
    =/  binary=octs  (bin i.types i.signs)
    =/  input0=(list @)  (~(got by m-inputs) i.types)
    =/  input1=(list @)  (~(got by m-inputs) i.types)
    =;  scripts=(list form:m)
      (run-once-comp-list [binary *vase^~] scripts)
    =|  scripts=(list form:m)
    |-
    ?~  input0  scripts
    =/  input1-copy  input1
    |-
    ?~  input1  ^$(input0 t.input0, input1 input1-copy)
    :_  $(input1 t.input1)
    =/  m  runnable:wasm
    =,  arr
    ;<  a=@  try:m  (call-1 'test' i.input0 i.input1 ~)
    (return:m ;;(lv [(crip i.types) a]) ~)  
    ::
    ++  bin
      |=  [type=tape sign=tape]
      ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param {type} {type}) (result {type})
          local.get 0
          local.get 1
          {type}.shr_{sign}))
      """
    --
++  test-min-max-copysign
  %+  expect-eq
    !>  &+~
    !>
    :: =.  print-time  & 
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    ::
    =/  types=(list tape)  ~["f32" "f64"]
    |-  =*  types-loop  $
    ?~  types  &+~
    =/  ops=(list tape)  ~["min" "max" "copysign"]
    |-  =*  ops-loop  $
    ?~  ops  types-loop(types t.types)
    =/  binary=octs  (bin i.types i.ops)
    =/  input0=(list @)  (~(got by m-inputs) i.types)
    =/  input1=(list @)  (~(got by m-inputs) i.types)
    =;  scripts=(list form:m)
      (run-once-comp-list [binary *vase^~] scripts)
    =|  scripts=(list form:m)
    |-
    ?~  input0  scripts
    =/  input1-copy  input1
    |-
    ?~  input1  ^$(input0 t.input0, input1 input1-copy)
    :_  $(input1 t.input1)
    =/  m  runnable:wasm
    =,  arr
    ;<  a=@  try:m  (call-1 'test' i.input0 i.input1 ~)
    (return:m ;;(lv [(crip i.types) a]) ~)  
    ::
    ++  bin
      |=  [type=tape op=tape]
      ^-  octs
      %-  parser
      """
      (module
        (func (export "test") (param {type} {type}) (result {type})
          local.get 0
          local.get 1
          {type}.{op}))
      """
    --
::  monadic ops
::
++  test-call-ext
  %+  expect-eq
    !>  &+~
    !>
    :: =.  print-time  & 
    =/  m  runnable:wasm
    |^  ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ;<  *  try:m  (call-ext %foo i32+42 octs+3^'foo' ~)
    (return:m ~)
    ::
    ++  binary
      ^-  octs
      (parser "(module)")
    --
::
++  test-global-set-get
  %+  expect-eq
    !>  &+~
    !>
    :: =.  print-time  &
    =/  m  runnable:wasm
    =/  binary
      (parser "(module (global (export \"foo\") (mut i32) i32.const 42))")
    ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ;<  a=@  try:m  (global-get 'foo')
    ?.  =(a 42)  !!
    ;<  ~    try:m  (global-set 'foo' (i32neg 42))
    ;<  b=@  try:m  (global-get 'foo')
    (return:m i32+b ~)
::
++  test-memsize-grow
  %+  expect-eq
    !>  &+~
    !>
    :: =.  print-time  &
    =/  m  runnable:wasm
    =/  binary=octs
      (parser "(module (memory 3 16))")
    ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ;<  a=@  try:m  memory-size
    ?.  =(a 3)  !!
    ;<  b=@    try:m  (memory-grow 13)
    ?.  =(b 3)  !!
    ;<  c=@  try:m  memory-size
    ?.  =(c 16)  !!
    (return:m ~)
::
++  test-write-close-32
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    =/  binary=octs
      %-  parser
      """
      (module
        (memory 1)
        (func (export "write_i32") (param i32)
          (loop   ;; test suspension stack
            (i32.store (local.get 0) (i32.const 42))
          )
        )
      )
      """
    ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ^-  form:m
    =/  ptr0=@  65.499
    ;<  ptr1=@  try:m
      =/  m  (script @ vase)
      |-  ^-  form:m
      ;<  res=@  catch:m
        :-  ;<  *  try:m  (call 'write_i32' ptr0 ~)
            $(ptr0 +(ptr0))
        (return:m ptr0)
      (return:m res)
    ::
    (return:m i32+ptr1 ~)
::
++  test-write-close-64
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    =/  binary=octs
      %-  parser
      """
      (module
        (memory 1)
        (func (export "write_i64") (param i32)
          (loop   ;; test suspension stack
            (i64.store (local.get 0) (i64.const 42))
          )
        )
      )
      """
    ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ^-  form:m
    =/  ptr0=@  65.499
    ;<  ptr1=@  try:m
      =/  m  (script @ vase)
      |-  ^-  form:m
      ;<  res=@  catch:m
        :-  ;<  *  try:m  (call 'write_i64' ptr0 ~)
            $(ptr0 +(ptr0))
        (return:m ptr0)
      (return:m res)
    ::
    (return:m i32+ptr1 ~)
::
++  test-read-close-32
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    =/  binary=octs
      %-  parser
      """
      (module
        (memory 1)
        (func (export "read_i32") (param i32)
          (loop   ;; test suspension stack
            (i32.load (local.get 0))
            drop
          )
        )
      )
      """
    ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ^-  form:m
    =/  ptr0=@  65.499
    ;<  ptr1=@  try:m
      =/  m  (script @ vase)
      |-  ^-  form:m
      ;<  res=@  catch:m
        :-  ;<  *  try:m  (call 'read_i32' ptr0 ~)
            $(ptr0 +(ptr0))
        (return:m ptr0)
      (return:m res)
    ::
    (return:m i32+ptr1 ~)
::
++  test-read-close-64
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    =/  binary=octs
      %-  parser
      """
      (module
        (memory 1)
        (func (export "read_i64") (param i32)
          (loop   ;; test suspension stack
            (i64.load (local.get 0))
            drop
          )
        )
      )
      """
    ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ^-  form:m
    =/  ptr0=@  65.499
    ;<  ptr1=@  try:m
      =/  m  (script @ vase)
      |-  ^-  form:m
      ;<  res=@  catch:m
        :-  ;<  *  try:m  (call 'read_i64' ptr0 ~)
            $(ptr0 +(ptr0))
        (return:m ptr0)
      (return:m res)
    ::
    (return:m i32+ptr1 ~)
::
++  test-globals-io
  %+  expect-eq
    !>  &+~
    !>
    =/  m  runnable:wasm
    =/  binary=octs
      %-  parser
      """
      (module
        (global (mut i32) i32.const 42)
        (global i32 i32.const 12)
      )
      """
    ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    %+  run-once-comp  [binary *vase^~]
    =,  arr
    =/  m  runnable:wasm
    ^-  form:m
    ;<  a=(list @)  try:m  get-all-local-globals
    ?>  =(a ~[42 12])
    ;<  ~           try:m  (set-all-local-globals 10 11 ~)
    ;<  b=(list @)  try:m  get-all-local-globals
    ?>  =(b ~[10 11])
    (return:m ~)
::
--