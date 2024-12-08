::  urwasm AST definition and binary opcode classification
::
::    The first part mirrors Structure chapter of WebAssembly Core Specification
::    https://www.w3.org/TR/2022/WD-wasm-core-2-20220419/syntax/index.html.
::    The second part is based on binary format specification.
::
::::  /hoon/wasm/sur
  ::
=>  ..part
~%  %wasm-sur-v0  +  ~
::
|%
+$  octs  (pair @ud @)
++  wasm-sur
  |%
  ::  Types
  ::
  +$  num-type  ?(%i32 %i64 %f32 %f64)
  +$  vec-type  %v128
  +$  ref-type  ?(%extn %func)  ::  externref and funcref
  +$  valtype
    $~  %i32
    $?  num-type
        vec-type
        ref-type
    ==
  ::
  ::  $coin-wasm: type-annotated value
  ::
  +$  coin-wasm
    $~  [%i32 *@F]
    $%  [%i32 @F]
        [%i64 @G]
        [%f32 @rs]
        [%f64 @rd]
        [vec-type @H]
        $:  %ref                  ::  function reference, null or not
            $%  [%func (unit @)]  ::  local
                $:  %extn         ::  external
                    %-  unit
                    $:  [mod=cord name=cord]
                        type=func-type
        ==  ==  ==  ==
    ::
    ==
  ::
  +$  limits
    $%  [%flor p=@]      ::  min only
        [%ceil p=@ q=@]  ::  min and max
    ==
  ::
  ::  Instructions
  ::
  +$  memarg
    $+  memarg
    [align=@ offset=@]
  ::
  +$  block-type  $@(@ func-type)  ::  typeidx in type section or func-type
  +$  func-type
    $:  params=(list valtype)
        results=(list valtype)
    ==
  ::
  +$  lane-type  ?(%i8 %i16 num-type)
  +$  instruction
    $%([%vec instr-vec] instr-short instr-num instr-dbug)
  ::
  +$  instr-dbug
    $%
      [%dbug %print-tee term]
    ==
  +$  instr-num  ?(instr-num-zero instr-num-one instr-num-two)
  +$  instr-num-zero
    $%
      [%const p=$<(?(%v128 %ref) coin-wasm)]
    ==
  ::
  +$  instr-num-one
    $%
      [%eqz type=?(%i32 %i64)]
      [%clz type=?(%i32 %i64)]
      [%ctz type=?(%i32 %i64)]
      [%popcnt type=?(%i8 %i32 %i64)]
      [%abs type=lane-type]
      [%neg type=lane-type]
      [%ceil type=?(%f32 %f64)]
      [%floor type=?(%f32 %f64)]
      ::
      $:  %trunc
          type=num-type
          source-type=(unit ?(%f32 %f64))
          mode=(unit ?(%s %u))
          sat=?
      ==
      ::
      [%nearest type=?(%f32 %f64)]
      [%sqrt type=?(%f32 %f64)]
      [%wrap ~]
    ::
      $:  %extend
          type=?(%i32 %i64)
          source-type=?(%i32 %i64)
          source-size=?(%8 %16 %32)
          mode=?(%s %u)
      ==
    ::
      [%convert type=?(%f32 %f64) source-type=?(%i32 %i64) mode=?(%s %u)]
      [%demote ~]
      [%promote ~]
      [%reinterpret type=num-type source-type=num-type]
    ==
  ::
  +$  instr-num-two
    $%
      [%eq type=lane-type]
      [%ne type=lane-type]
      [%lt type=lane-type mode=(unit ?(%s %u))]
      [%gt type=lane-type mode=(unit ?(%s %u))]
      [%le type=lane-type mode=(unit ?(%s %u))]
      [%ge type=lane-type mode=(unit ?(%s %u))]
      [%add type=lane-type]
      [%sub type=lane-type]
      [%mul type=lane-type]
      [%div type=lane-type mode=(unit ?(%s %u))]
      [%rem type=?(%i32 %i64) mode=?(%s %u)]
      [%and type=?(%i32 %i64)]
      [%or type=?(%i32 %i64)]
      [%xor type=?(%i32 %i64)]
      [%shl type=?(%i8 %i16 %i32 %i64)]
      [%shr type=?(%i8 %i16 %i32 %i64) mode=?(%s %u)]
      [%rotl type=?(%i32 %i64)]
      [%rotr type=?(%i32 %i64)]
      [%min type=?(%f32 %f64)]
      [%max type=?(%f32 %f64)]
      [%copysign type=?(%f32 %f64)]
    ==
  ::
  +$  instr-short
    $%
    ::  Control instructions
    ::
      [%unreachable ~]
      [%nop ~]
      [%block type=block-type body=expression]
      [%loop type=block-type body=expression]
      $:  %if
          type=block-type
          branch-true=expression
          branch-false=expression
      ==
    ::
      [%br label=@]
      [%br-if label=@]
      [%br-table label-vec=(list @) label-default=@]
      [%return ~]
      [%call func-id=@]
      [%call-indirect type-id=@ table-id=@]
    ::  Reference instructions
    ::
      [%ref-null t=ref-type]
      [%ref-is-null ~]
      [%ref-func func-id=@]
    ::  Parametric instructions
    ::
      [%drop ~]
      [%select (unit (list valtype))]
    ::  Variable instructions
    ::
      [%local-get index=@]
      [%local-set index=@]
      [%local-tee index=@]
      [%global-get index=@]
      [%global-set index=@]
    ::  Table instructions
    ::
      [%table-get tab-id=@]
      [%table-set tab-id=@]
      [%table-init elem-id=@ tab-id=@]
      [%elem-drop elem-id=@]
      [%table-copy tab-id-x=@ tab-id-y=@]
      [%table-grow tab-id=@]
      [%table-size tab-id=@]
      [%table-fill tab-id=@]
      ::  Memory instructions
      ::
      $:  %load
          type=num-type
          m=memarg
          n=(unit ?(%8 %16 %32))
          mode=(unit ?(%s %u))
      ==
      ::
      $:  %store
          type=num-type
          m=memarg
          n=(unit ?(%8 %16 %32))
      ==
      ::
      [%memory-size mem-id=%0]
      [%memory-grow mem-id=%0]
      [%memory-init x=@ mem-id=%0]
      [%data-drop x=@]
      [%memory-copy mem-1=%0 mem-2=%0]
      [%memory-fill mem-id=%0]
    ==  ::  $instr-short
  ::
  ::  0xFD prefixed instructions
  ::
  +$  instr-vec
    $%
    ::  Load
    ::
      $:  %load
          m=memarg
          $=  kind  %-  unit
          $:  p=?(%8 %16 %32 %64)
              q=?(%splat %zero [%extend ?(%s %u)])
      ==  ==
    ::
      [%load-lane m=memarg p=?(%8 %16 %32 %64) l=@]
    ::  Store
    ::
      [%store m=memarg]
      [%store-lane m=memarg p=?(%8 %16 %32 %64) l=@]
    ::  Misc
      [%const p=$>(%v128 coin-wasm)]
      [%shuffle lane-ids=(list @)]
      [%extract p=lane-type l=@ mode=?(%s %u)]
      [%replace p=lane-type l=@]
    ::  Plain
    ::
      [%swizzle ~]
      [%splat p=lane-type]
      [%eq p=lane-type]
      [%ne p=lane-type]
      [%lt p=lane-type mode=?(%u %s)]
      [%gt p=lane-type mode=?(%u %s)]
      [%le p=lane-type mode=?(%u %s)]
      [%ge p=lane-type mode=?(%u %s)]
      [%not ~]
      [%and ~]
      [%andnot ~]
      [%or ~]
      [%xor ~]
      [%bitselect ~]
      [%any-true ~]
      [%abs p=lane-type]
      [%neg p=lane-type]
      [%popcnt ~]
      [%all-true p=?(%i8 %i16 %i32 %i64)]
      [%bitmask p=?(%i8 %i16 %i32 %i64)]
      [%narrow p=?(%i8 %i16) mode=?(%u %s)]
      [%shl p=?(%i8 %i16 %i32 %i64)]
      [%shr p=?(%i8 %i16 %i32 %i64) mode=?(%u %s)]
      [%add p=lane-type sat=(unit ?(%u %s))]
      [%sub p=lane-type sat=(unit ?(%u %s))]
      [%min p=lane-type mode=?(%u %s)]
      [%max p=lane-type mode=?(%u %s)]
      [%avgr p=?(%i8 %i16) mode=%u]
      [%extadd p=?(%i16 %i32) mode=?(%u %s)]
      [%q15mul-r-sat ~]
      [%extend p=?(%i16 %i32 %i64) mode=?(%u %s) half=?(%high %low)]
      [%mul p=lane-type]
      [%extmul p=?(%i16 %i32 %i64) mode=?(%u %s) half=?(%high %low)]
      [%dot ~]
      [%ceil p=?(%f32 %f64)]
      [%floor p=?(%f32 %f64)]
      [%trunc p=?(%i32 %f32 %f64) from=?(%f32 %f64) mode=?(%u %s)]
      [%nearest p=?(%f32 %f64)]
      [%sqrt p=?(%f32 %f64)]
      [%div p=?(%f32 %f64)]
      [%pmin p=?(%f32 %f64)]
      [%pmax p=?(%f32 %f64)]
      [%convert p=?(%f32 %f64) mode=?(%u %s)]
      [%demote ~]
      [%promote ~]
    ==  ::  $instr-vec
  ::
  +$  expression  (list instruction)
  +$  const-instr
    $~  [%const %i32 `@`0]
    $?  [%vec $>(%const instr-vec)]
        $>(?(%const %global-get %ref-null %ref-func) instruction)
    ==
  ::
  ::  Modules
  ::
  ::  $module: represents `module` type from WebAssembly specification,
  ::  but in a format closer to the binary representation, e.g. separate
  ::  code and function sections. This is done to simplify parsing.
  ::
  +$  module
    $:
      =type-section
      =import-section
      =function-section
      =table-section
      =memory-section
      =global-section
      =export-section
      =start-section
      =elem-section
      =datacnt-section
      =code-section
      =data-section
    ==
  ::
  ::  Definitions of sections
  ::
  ::  Type section
  ::
  +$  type-section
    $+  type-section
    (list func-type)
  ::
  ::  Import section
  ::
  +$  import-section
    $+  import-section
    (list import)
  ::
  +$  import
    $:  mod=cord
        name=cord
        $=  desc
        $%
          [%func type-id=@]
          [%tabl t=table]
          [%memo l=limits]
          [%glob v=valtype m=?(%con %var)]  ::  constant or variable
    ==  ==
  ::  Function section
  ::
  +$  function-section
    $+  function-section
    (list type-id=@)
  ::
  ::  Table section
  ::
  +$  table-section  (list table)
  +$  table  (pair ref-type limits)
  ::
  ::  Memory section
  ::
  +$  memory-section  (list limits)
  ::
  ::  Global section
  ::
  +$  global-section  (list global)
  ::  valtype, mutability and init value.
  ::  We use a single constant instruction as opposed to a
  ::  (list instruction) since there is no global value type
  ::  that would take multiple constant values
  ::
  +$  global
    $:  v=valtype
        m=?(%con %var)
        i=const-instr
    ==
  ::
  ::  Export section
  ::
  +$  export-section
    $+  export-section
    (list export)
  ::
  +$  export  [name=cord =export-desc]
  ::
  +$  export-desc
    $%  [%func i=@]
        [%tabl i=@]
        [%memo i=@]
        [%glob i=@]
    ==
  ::
  ::  Start section
  ::
  +$  start-section  (unit @)
  ::
  ::  Element section
  ::
  +$  elem-section  (list elem)
  ::
  +$  elem
    $~  [*ref-type ~ %pass ~]
    $:  t=ref-type
        i=(list $>(?(%ref-func %ref-null) instruction))
        $=  m
        $%  [%pass ~]
            [%decl ~]
            [%acti tab=@ off=const-instr]
    ==  ==
  ::
  ::  Code section
  ::
  +$  code-section
    $+  code-section
    (list code)
  ::
  +$  code
    $:  locals=(list valtype)
        =expression
    ==
  ::
  ::  Data section
  ::
  +$  data-section  (list data)
  ::  memid is implied to be 0
  ::
  +$  data
    $%
      [%acti off=const-instr b=octs]
      [%pass b=octs]
    ==
  ::
  ::  Data count section
  ::
  ++  datacnt-section  (unit @)
  ::
  ::  Binary opcode classification
  ::
  +$  opcode  $?  bin-opcodes-zero-args
                  bin-opcodes-one-arg
                  bin-opcodes-two-args
                  bin-opcodes-blocks
                  pseudo-opcode
              ==
  ::
  +$  bin-opcodes-zero-args
    $?
  ::  trap  nop   return  drop   select  wrap  demote  promote
      %0x0  %0x1  %0xf    %0x1a  %0x1b  %0xa7  %0xb6   %0xbb
  ::
      eqz-opcodes  eq-opcodes  ne-opcodes  lt-opcodes  gt-opcodes  le-opcodes
      ge-opcodes  clz-opcodes  ctz-opcodes  popcnt-opcodes  add-opcodes
      sub-opcodes  mul-opcodes  div-opcodes  rem-opcodes  and-opcodes  or-opcodes
      xor-opcodes  shl-opcodes  shr-opcodes  rotl-opcodes  rotr-opcodes
      abs-opcodes  neg-opcodes  ceil-opcodes  floor-opcodes  trunc-opcodes
      nearest-opcodes  sqrt-opcodes  min-opcodes  max-opcodes  copysign-opcodes
      extend-opcodes  convert-opcodes  reinterpret-opcodes
    ==
  ::
  +$  pseudo-opcode  ?(%0x5 %0xb)  ::  else, end
  +$  bin-opcodes-one-arg
    $?
  ::  br    br_if  call  local.get  local.set  local.tee  global.get  global.set
      %0xc  %0xd   %0x10  %0x20     %0x21      %0x22      %0x23       %0x24
  ::
      const-opcodes
      %0x3f  ::  memory.size
      %0x40  ::  memory.grow
    ==
  ::
  +$  bin-opcodes-two-args
    $?
      %0xe   ::  br_table
      %0x11  ::  call_indirect
      load-opcodes
      store-opcodes
    ==
  ::
  +$  bin-opcodes-blocks
    $?
      %0x2  ::  block
      %0x3  ::  loop
      %0x4  ::  if
    ==
  ::
  +$  const-opcodes
    $?
      %0x41  ::  i32
      %0x42  ::  i64
      %0x43  ::  f32
      %0x44  ::  f64
    ==
  ::
  +$  load-opcodes
    $?
      %0x28  ::  i32
      %0x29  ::  i64
      %0x2a  ::  f32
      %0x2b  ::  f64
      %0x2c  ::  i32 8 s
      %0x2d  ::  i32 8 u
      %0x2e  ::  i32 16 s
      %0x2f  ::  i32 16 u
      %0x30  ::  i64 8 s
      %0x31  ::  i64 8 u
      %0x32  ::  i64 16 s
      %0x33  ::  i64 16 u
      %0x34  ::  i64 32 s
      %0x35  ::  i64 32 u
    ==
  ::
  +$  store-opcodes
    $?
      %0x36  ::  i32
      %0x37  ::  i64
      %0x38  ::  f32
      %0x39  ::  f64
      %0x3a  ::  i32 8
      %0x3b  ::  i32 16
      %0x3c  ::  i64 8
      %0x3d  ::  i64 16
      %0x3e  ::  i64 32
    ==
  ::
  +$  eqz-opcodes  ?(%0x45 %0x50)              ::  i32, i64
  +$  eq-opcodes   ?(%0x46 %0x51 %0x5b %0x61)  ::  i32, i64, f32, f64
  +$  ne-opcodes   ?(%0x47 %0x52 %0x5c %0x62)  ::  i32, i64, f32, f64
  +$  lt-opcodes
    $?
      %0x48  ::  i32 s
      %0x49  ::  i32 u
      %0x53  ::  i64 s
      %0x54  ::  i64 u
      %0x5d  ::  f32
      %0x63  ::  f64
    ==
  ::
  +$  gt-opcodes
    $?
      %0x4a  ::  i32 s
      %0x4b  ::  i32 u
      %0x55  ::  i64 s
      %0x56  ::  i64 u
      %0x5e  ::  f32
      %0x64  ::  f64
    ==
  ::
  +$  le-opcodes
    $?
      %0x4c  ::  i32 s
      %0x4d  ::  i32 u
      %0x57  ::  i64 s
      %0x58  ::  i64 u
      %0x5f  ::  f32
      %0x65  ::  f64
    ==
  ::
  +$  ge-opcodes
    $?
      %0x4e  ::  i32 s
      %0x4f  ::  i32 u
      %0x59  ::  i64 s
      %0x5a  ::  i64 u
      %0x60  ::  f32
      %0x66  ::  f64
    ==
  ::
  +$  clz-opcodes  ?(%0x67 %0x79)              ::  i32, i64
  +$  ctz-opcodes  ?(%0x68 %0x7a)              ::  i32, i64
  +$  popcnt-opcodes  ?(%0x69 %0x7b)           ::  i32, i64
  +$  add-opcodes  ?(%0x6a %0x7c %0x92 %0xa0)  ::  i32, i64, f32, f64
  +$  sub-opcodes  ?(%0x6b %0x7d %0x93 %0xa1)  ::  i32, i64, f32, f64
  +$  mul-opcodes  ?(%0x6c %0x7e %0x94 %0xa2)  ::  i32, i64, f32, f64
  +$  div-opcodes
    $?
      %0x6d  ::  i32 s
      %0x6e  ::  i32 u
      %0x7f  ::  i64 s
      %0x80  ::  i64 u
      %0x95  ::  f32
      %0xa3  ::  f64
    ==
  ::
  +$  rem-opcodes
    $?
      %0x6f  ::  i32 s
      %0x70  ::  i32 u
      %0x81  ::  i64 s
      %0x82  ::  i64 u
    ==
  ::
  +$  and-opcodes  ?(%0x71 %0x83)  ::  i32, i64
  +$  or-opcodes   ?(%0x72 %0x84)  ::  i32, i64
  +$  xor-opcodes  ?(%0x73 %0x85)  ::  i32, i64
  +$  shl-opcodes  ?(%0x74 %0x86)  ::  i32, i64
  +$  shr-opcodes
    $?
      %0x75  ::  i32 s
      %0x76  ::  i32 u
      %0x87  ::  i64 s
      %0x88  ::  i64 u
    ==
  ::
  +$  rotl-opcodes   ?(%0x77 %0x89)  ::  i32, i64
  +$  rotr-opcodes   ?(%0x78 %0x8a)  ::  i32, i64
  +$  abs-opcodes    ?(%0x8b %0x99)  ::  f32, f64
  +$  neg-opcodes    ?(%0x8c %0x9a)  ::  f32, f64
  +$  ceil-opcodes   ?(%0x8d %0x9b)  ::  f32, f64
  +$  floor-opcodes  ?(%0x8e %0x9c)  ::  f32, f64
  +$  trunc-opcodes
    $?
      %0x8f  ::  f32
      %0x9d  ::  f64
      %0xa8  ::  f32 -> i32 s
      %0xa9  ::  f32 -> i32 u
      %0xaa  ::  f64 -> i32 s
      %0xab  ::  f64 -> i32 u
      %0xae  ::  f32 -> i64 s
      %0xaf  ::  f32 -> i64 u
      %0xb0  ::  f64 -> i64 s
      %0xb1  ::  f64 -> i64 u
    ==
  ::
  +$  nearest-opcodes   ?(%0x90 %0x9e)  ::  f32, f64
  +$  sqrt-opcodes      ?(%0x91 %0x9f)  ::  f32, f64
  +$  min-opcodes       ?(%0x96 %0xa4)  ::  f32, f64
  +$  max-opcodes       ?(%0x97 %0xa5)  ::  f32, f64
  +$  copysign-opcodes  ?(%0x98 %0xa6)  ::  f32, f64
  +$  extend-opcodes
    $?
      %0xac  ::  i32 -> i64 s
      %0xad  ::  i32 -> i64 u
      %0xc0  ::  i8  -> i32 s
      %0xc1  ::  i16 -> i32 s
      %0xc2  ::  i8  -> i64 s
      %0xc3  ::  i16 -> i64 s
      %0xc4  ::  i32 -> i64 s  ??  same as 0xac??? (yes)
    ==
  +$  convert-opcodes
    $?
      %0xb2  ::  i32 s -> f32
      %0xb3  ::  i32 u -> f32
      %0xb4  ::  i64 s -> f32
      %0xb5  ::  i64 u -> f32
      %0xb7  ::  i32 s -> f64
      %0xb8  ::  i32 u -> f64
      %0xb9  ::  i64 s -> f64
      %0xba  ::  i64 u -> f64
    ==
  ::
  +$  reinterpret-opcodes
    $?
      %0xbc  ::  f32 -> i32
      %0xbd  ::  f64 -> i64
      %0xbe  ::  i32 -> f32
      %0xbf  ::  i64 -> f64
    ==
  --  ::  |wasm-sur
--