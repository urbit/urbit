/+  *test
/+  wasm=wasm-lia
/+  parser=wasm-tools-wat-parser-lia
::
=/  lv  lia-value:lia-sur:wasm
=/  cw  coin-wasm:wasm-sur:wasm
=/  import  import:lia-sur:wasm
=/  wasm  ^?(wasm)
=>
  |%
  ++  run-once-comp
    =/  m  runnable:wasm
    |=  [sed=[module=octs =import] script=form:m]
    ^-  (each ~ [[%nock yield:m] [%fast yield:m]])
    =/  nock  ((run-once:wasm (list lv)) sed %none script)
    =/  fast  ((run-once:wasm (list lv)) sed %$ script)
    ?:  =(nock fast)  &+~
    [%| nock+nock fast+fast]
  ::
  ++  m-inputs
    ^-  (map tape (list @))
    %-  malt
    ^-  (list [tape (list @)])
    :~
      :-  "f32"
      ^-  (list @rs)
      :~  .nan
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
      ==
    ::
      :-  "f64"
      ^-  (list @rd)
      :~  .~nan
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
      ==
    ::
      :-  "i32"
      ^-  (list @)
      :~
        (dec (bex 32))
        (dec (bex 31))
        0
        1
        2
        1.000
        (bex 16)
      ==
    ::
      :-  "i64"
      ^-  (list @)
      :~
        (dec (bex 64))
        (dec (bex 63))
        0
        1
        2
        1.000
        (bex 32)
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
    %+  run-once-comp  [binary ~]
    =/  m  runnable:wasm
    ;<  a=@  try:m  (call-1:wasm 'test' i.input ~)
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
    %+  run-once-comp  [binary ~]
    =/  m  runnable:wasm
    ;<  a=@  try:m  (call-1:wasm 'test' i.input ~)
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
    %+  run-once-comp  [bin ~]
    =/  m  runnable:wasm
    ;<  a=@  try:m  (call-1:wasm 'test' i.input ~)
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
    %+  run-once-comp  [bin ~]
    =/  m  runnable:wasm
    ;<  a=@  try:m  (call-1:wasm 'test' i.input ~)
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
    %+  run-once-comp  [binary ~]
    =,  wasm
    =/  m  runnable
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
--