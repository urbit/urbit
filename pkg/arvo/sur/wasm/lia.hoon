/-  wasm-engine
=>  wasm-engine
~%  %monad-sur-v0  +  ~
|%
++  lia-sur
  =,  wasm-sur
  =,  engine-sur
  |%
  ++  lia-value
    $~  [%i32 0]
    $%  [%octs octs]
        $<(%ref coin-wasm)
    ==
  ::
  +$  import
    %+  map  (pair cord cord)
    $-  (list coin-wasm)
    (script-raw-form (list coin-wasm))
  ::
  +$  lia-state  (trel store (list (list lia-value)) import)
  ++  script-yield
    |$  [a]
    $%  [%0 p=a]
        [%1 name=term args=(list lia-value)]
        [%2 ~]
    ==
  ::
  ++  script-result
    |$  [a]
    [(script-yield a) lia-state]
  ::
  ++  script-raw-form
    |*  a=mold
    $-(lia-state (script-result a))
  ::
  ++  script
    |*  a=mold
    |%
    ++  output  (script-result a)
    ++  yield  (script-yield a)
    ++  form  (script-raw-form a)
    ++  return  ::  pure
      |=  arg=a
      :: =*  sam  +<
      ^-  form
      |=  s=lia-state
      :: ~&  !.(ret-sam+!=(sam))
      [0+arg s]
    ::
    ++  try  ::  monadic bind
      |*  b=mold
      |=  [m-b=(script-raw-form b) gat=$-(b form)]
      :: =*  sam  +<
      ^-  form
      |=  s=lia-state
      :: ~&  !.(try-sam+!=(sam))
      =^  b-yil=(script-yield b)  s  (m-b s)
      ^-  output
      ?.  ?=(%0 -.b-yil)  [b-yil s]
      ((gat p.b-yil) s)
    ::
    ++  catch  ::  bind either
      |*  b=mold
      |=  $:
            $:  m-try=(script-raw-form b)
                m-err=(script-raw-form b)
            ==
            gat=$-(b form)
          ==
      ^-  form
      |=  s=lia-state
      =^  try-yil=(script-yield b)  s  (m-try s)
      ^-  output
      ?:  ?=(%0 -.try-yil)
        ((gat p.try-yil) s)
      ?:  ?=(%1 -.try-yil)
        [try-yil s]
      =^  err-yil  s  (m-err s)
      ?.  ?=(%0 -.err-yil)  [err-yil s]
      ((gat p.err-yil) s)
    ::
    --  ::  |script
  ::
  +$  seed
    ::  Lia formal state, assumes that the past script evaluates to %0 or %1
    ::  static fields assumed to not change between invocations
    ::
    ::  module: binary, static
    ::  past: script accumulator, fixed type
    ::  shop: external results accumulator
    ::  import: Wasm module imports, static:
    ::    /import/name ->
    ::  script Kleisli arrow ((list coin-wasm) -> (list coin-wasm))
    ::
    $:
      module=octs
      past=(script-raw-form (list lia-value))
      shop=(list (list lia-value))
      =import
    ==
  --
--  ::  |lia-sur