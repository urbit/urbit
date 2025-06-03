/-  wasm-engine
=>  wasm-engine
~%  %monad-sur-v1  +  ~
|%
++  lia-sur
  =,  wasm-sur
  =,  engine-sur
  |%
  ++  lia-value
    $~  [%i32 0]
    $%  [%octs octs]
        [%noun *]
        [%vase vase]
        $<(%ref coin-wasm)
    ==
  ::
  ++  import
    |$  [acc]
    %+  pair  acc
    %+  map  (pair cord cord)
    $-  (list coin-wasm)
    (script-raw-form (list coin-wasm) acc)
  ::
  ++  lia-state
    |$  [acc]
    (trel store (list (list lia-value)) (import acc))
  ::
  ++  script-yield
    |$  [a]
    $%  [%0 p=a]
        [%1 name=term args=(list lia-value)]
        [%2 ~]
    ==
  ::
  ++  script-result
    |$  [m-yil m-acc]
    [(script-yield m-yil) (lia-state m-acc)]
  ::
  ++  script-raw-form
    |*  [yil=mold acc=mold]
    $-((lia-state acc) (script-result yil acc))
  ::
  ++  script
    |*  [m-yil=mold m-acc=mold]
    |%
    ++  output  (script-result m-yil m-acc)
    ++  yield  (script-yield m-yil)
    ++  form  (script-raw-form m-yil m-acc)
    ++  m-sat  (lia-state m-acc)
    ++  return  ::  pure
      |=  arg=m-yil
      :: =*  sam  +<
      ^-  form
      |=  s=m-sat
      :: ~&  !.(ret-sam+!=(sam))
      [0+arg s]
    ::
    ++  fail
      ^-  form
      |=  s=m-sat
      [2+~ s]
    ::
    ++  try  ::  monadic bind
      |*  m-mond=mold
      |=  [mond=(script-raw-form m-mond m-acc) cont=$-(m-mond form)]
      :: =*  sam  +<
      ^-  form
      |=  s=m-sat
      :: ~&  !.(try-sam+!=(sam))
      =^  mond-yil=(script-yield m-mond)  s  (mond s)
      ^-  output
      ?.  ?=(%0 -.mond-yil)  [mond-yil s]
      ((cont p.mond-yil) s)
    ::
    ++  catch  ::  bind either
      |*  m-mond=mold
      |=  $:
            $:  try=(script-raw-form m-mond m-acc)
                err=(script-raw-form m-mond m-acc)
            ==
            cont=$-(m-mond form)
          ==
      ^-  form
      |=  s=m-sat
      =^  try-yil=(script-yield m-mond)  s  (try s)
      ^-  output
      ?:  ?=(%0 -.try-yil)
        ((cont p.try-yil) s)
      ?:  ?=(%1 -.try-yil)
        [try-yil s]
      =^  err-yil  s  (err s)
      ?.  ?=(%0 -.err-yil)  [err-yil s]
      ((cont p.err-yil) s)
    ::
    --  ::  |script
  :: ::
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
      past=(script-raw-form (list lia-value) vase)
      shop=(list (list lia-value))
      import=(import vase)
    ==
  --
--  ::  |lia-sur