|*  [input-type=mold card-type=mold contract-type=mold]
|%
+$  trad-input  [=bowl:gall in=(unit [=wire sign=input-type])]
+$  trad-move  (pair bone card-type)
::
::  notes:     notes to send immediately.  These will go out even if a
::             later stage of the process fails, so they shouldn't have
::             any semantic effect on the rest of the system.  Path is
::             included exclusively for documentation and |verb.
::  effects:   moves to send after the process ends.
::  contracts: stuff to cancel at end of transaction.
::  wait:      don't move on, stay here.  The next sign should come back
::             to this same callback.
::  cont:      continue process with new callback.
::  fail:      abort process; don't send effects
::  done:      finish process; send effects
::
++  trad-output-raw
  |*  a=mold
  $~  [~ ~ ~ %done *a]
  $:  notes=(list [path card-type])
      effects=(list trad-move)
      contracts=(set [add=? contract=contract-type])
      $=  next
      $%  [%wait ~]
          [%cont self=(trad-form-raw a)]
          [%fail err=(pair term tang)]
          [%done value=a]
      ==
  ==
::
++  trad-form-raw
  |*  a=mold
  $-(trad-input (trad-output-raw a))
::
++  trad-fail
  |=  err=(pair term tang)
  |=  trad-input
  [~ ~ ~ %fail err]
::
++  trad
  |*  a=mold
  |%
  ++  output  (trad-output-raw a)
  ++  form  (trad-form-raw a)
  ++  pure
    |=  arg=a
    ^-  form
    |=  trad-input
    [~ ~ ~ %done arg]
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(trad-form-raw b) fun=$-(b form)]
    ^-  form
    |=  input=trad-input
    =/  b-res=(trad-output-raw b)
      (m-b input)
    ^-  output
    :^  notes.b-res  effects.b-res  contracts.b-res
    ?-    -.next.b-res
      %wait  [%wait ~]
      %cont  [%cont ..$(m-b self.next.b-res)]
      %fail  [%fail err.next.b-res]
      %done  [%cont (fun value.next.b-res)]
    ==
  ::
  ::  The trad monad must be evaluted in a particular way to maintain
  ::  its monadic character.  +take:eval implements this.
  ::
  ++  eval
    |%
    ::  Indelible state of a trad
    ::
    +$  eval-form
      $:  effects=(list trad-move)
          contracts=(set contract-type)
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
          [%fail contracts=(set contract-type) err=(pair term tang)]
          [%done contracts=(set contract-type) value=a]
      ==
    ::
    ::  Take a new sign and run the trad against it
    ::
    ++  take
      ::  moves: accumulate throughout recursion the moves to be
      ::         produced now
      =|  moves=(list trad-move)
      |=  [=eval-form =bone =trad-input]
      ^-  [[(list trad-move) =eval-result] _eval-form]
      =*  take-loop  $
      ::  run the trad callback
      ::
      =/  =output  (form.eval-form trad-input)
      ::  add notes to moves
      ::
      =.  moves
        %+  welp
          moves
        %+  turn  notes.output
        |=  [=path card=card-type]
        ^-  trad-move
        [bone card]
      ::  add effects to list to be produced when done
      ::
      =.  effects.eval-form
        (weld effects.eval-form effects.output)
      ::  add or remove contracts
      ::
      =.  .
        =*  loop-result  .
        =/  new=(list [add=? contract=contract-type])
          ~(tap in contracts.output)
        |-  ^+  loop-result
        =*  loop  $
        ?~  new
          loop-result
        ?:  add.i.new
          ?:  (~(has in contracts.eval-form) contract.i.new)
            %=  loop-result
              next.output  [%fail %contract-already-exists >contract.i.new< ~]
            ==
          %=  loop
            contracts.eval-form  (~(put in contracts.eval-form) contract.i.new)
            new                  t.new
          ==
        ?:  (~(has in contracts.eval-form) contract.i.new)
          %=  loop
            contracts.eval-form  (~(del in contracts.eval-form) contract.i.new)
            new                  t.new
          ==
        %=  loop-result
          next.output  [%fail %contract-doesnt-exist >contract.i.new< ~]
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
          trad-input      [bowl.trad-input ~]
        ==
      ==
    --
  --
--
