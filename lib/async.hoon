|*  [input-type=mold card-type=mold contract-type=mold]
|%
+$  async-input  [=bowl:gall in=(unit [=wire sign=input-type])]
+$  async-move  (pair bone card-type)
::
::  cards:     cards to send immediately.  These will go out even if a
::             later stage of the computation fails, so they shouldn't have
::             any semantic effect on the rest of the system.
::             Alternately, they may record an entry in contracts with
::             enough information to undo the effect if the computation
::             fails.
::  effects:   moves to send after the computation ends.
::  contracts: stuff to cancel at end of computation.
::  wait:      don't move on, stay here.  The next sign should come back
::             to this same callback.
::  cont:      continue computation with new callback.
::  fail:      abort computation; don't send effects
::  done:      finish computation; send effects
::
+$  contract-delta
  $%  [%gain =bone]
      [%lose ~]
  ==
::
++  async-output-raw
  |*  a=mold
  $~  [~ ~ ~ %done *a]
  $:  cards=(list card-type)
      effects=(list async-move)
      contracts=(map contract-type contract-delta)
      $=  next
      $%  [%wait ~]
          [%cont self=(async-form-raw a)]
          [%fail err=(pair term tang)]
          [%done value=a]
      ==
  ==
::
++  async-form-raw
  |*  a=mold
  $-(async-input (async-output-raw a))
::
::  Abort asynchronous computation with error message
::
++  async-fail
  |=  err=(pair term tang)
  |=  async-input
  [~ ~ ~ %fail err]
::
::  Asynchronous transcaction monad.
::
::  Combo of four monads:
::  - Reader on input-type
::  - Writer on card-type
::  - Continuation
::  - Exception
::
++  async
  |*  a=mold
  |%
  ++  output  (async-output-raw a)
  ::
  ::  Type of an asynchronous computation.
  ::
  ++  form  (async-form-raw a)
  ::
  ::  Monadic pure.  Identity computation for bind.
  ::
  ++  pure
    |=  arg=a
    ^-  form
    |=  async-input
    [~ ~ ~ %done arg]
  ::
  ::  Monadic bind.  Combines two computations, associatively.
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(async-form-raw b) fun=$-(b form)]
    ^-  form
    |=  input=async-input
    =/  b-res=(async-output-raw b)
      (m-b input)
    ^-  output
    :^  cards.b-res  effects.b-res  contracts.b-res
    ?-    -.next.b-res
      %wait  [%wait ~]
      %cont  [%cont ..$(m-b self.next.b-res)]
      %fail  [%fail err.next.b-res]
      %done  [%cont (fun value.next.b-res)]
    ==
  ::
  ::  The async monad must be evaluted in a particular way to maintain
  ::  its monadic character.  +take:eval implements this.
  ::
  ++  eval
    |%
    ::  Indelible state of a async
    ::
    +$  eval-form
      $:  effects=(list async-move)
          contracts=(map contract-type bone)
          =form
      ==
    ::
    ::  Convert initial form to eval-form
    ::
    ++  from-form
      |=  =form
      ^-  eval-form
      [~ ~ form]
    ::
    ::  The cases of results of +take
    ::
    +$  eval-result
      $%  [%next ~]
          [%fail contracts=(map contract-type bone) err=(pair term tang)]
          [%done contracts=(map contract-type bone) value=a]
      ==
    ::
    ::  Take a new sign and run the async against it
    ::
    ++  take
      ::  moves: accumulate throughout recursion the moves to be
      ::         produced now
      =|  moves=(list async-move)
      |=  [=eval-form =bone =async-input]
      ^-  [[(list async-move) =eval-result] _eval-form]
      =*  take-loop  $
      ::  run the async callback
      ::
      =/  =output  (form.eval-form async-input)
      ::  add cards to moves
      ::
      =.  moves
        %+  welp
          moves
        %+  turn  cards.output
        |=  card=card-type
        ^-  async-move
        [bone card]
      ::  add effects to list to be produced when done
      ::
      =.  effects.eval-form
        (weld effects.eval-form effects.output)
      ::  add or remove contracts
      ::
      =>
        =*  loop-result  .
        =/  new=(list [contract=contract-type delta=contract-delta])
          ~(tap by contracts.output)
        |-  ^+  loop-result
        =*  loop  $
        ?~  new
          loop-result
        =/  exists=?
          (~(has by contracts.eval-form) contract.i.new)
        ?-  -.delta.i.new
        ::  add contract and bone
        ::
            %gain
          ?:  exists
            %=  loop-result
              next.output  [%fail %contract-already-exists >contract.i.new< ~]
            ==
          %=  loop
            contracts.eval-form  (~(put by contracts.eval-form) [contract bone.delta]:i.new)
            new                  t.new
          ==
        ::  remove contract
        ::
            %lose
          ?:  exists
            %=  loop
              contracts.eval-form  (~(del by contracts.eval-form) contract.i.new)
              new                  t.new
            ==
          %=  loop-result
            next.output  [%fail %contract-doesnt-exist >contract.i.new< ~]
          ==
        ==
      ::  if done, produce effects
      ::
      =?  moves  ?=(%done -.next.output)
        %+  welp
          moves
        effects.eval-form
      ::  case-wise handle next steps
      ::
      ?-  -.next.output
          %wait  [[moves %next ~] eval-form]
          %fail  [[moves %fail contracts.eval-form err.next.output] eval-form]
          %done  [[moves %done contracts.eval-form value.next.output] eval-form]
          %cont
        ::  recurse to run continuation with initialization input
        ::
        %_  take-loop
          form.eval-form  self.next.output
          async-input     [bowl.async-input ~]
        ==
      ==
    --
  --
--
