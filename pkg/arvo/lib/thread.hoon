|%
+$  card          card:agent:mall
+$  input
  $%  [%poke =cage]
      [%sign =wire =sign-arvo]
  ==
+$  thread-input  [=bowl:mall in=(unit input)]
::
::  cards:  cards to send immediately.  These will go out even if a
::          later stage of the computation fails, so they shouldn't have
::          any semantic effect on the rest of the system.
::          Alternately, they may record an entry in contracts with
::          enough information to undo the effect if the computation
::          fails.
::  wait:   don't move on, stay here.  The next sign should come back
::          to this same callback.
::  skip:   didn't expect this input; drop it down to be handled
::          elsewhere
::  cont:   continue computation with new callback.
::  fail:   abort computation; don't send effects
::  done:   finish computation; send effects
::
++  thread-output-raw
  |*  a=mold
  $~  [~ %done *a]
  $:  cards=(list card)
      $=  next
      $%  [%wait ~]
          [%skip ~]
          [%cont self=(thread-form-raw a)]
          [%fail err=(pair term tang)]
          [%done value=a]
      ==
  ==
::
++  thread-form-raw
  |*  a=mold
  $-(thread-input (thread-output-raw a))
::
::  Abort thread computation with error message
::
++  thread-fail
  |=  err=(pair term tang)
  |=  thread-input
  [~ ~ ~ %fail err]
::
::  Asynchronous transcaction monad.
::
::  Combo of four monads:
::  - Reader on input
::  - Writer on card
::  - Continuation
::  - Exception
::
++  thread
  |*  a=mold
  |%
  ++  output  (thread-output-raw a)
  ::
  ::  Type of an thread computation.
  ::
  ++  form  (thread-form-raw a)
  ::
  ::  Monadic pure.  Identity computation for bind.
  ::
  ++  pure
    |=  arg=a
    ^-  form
    |=  thread-input
    [~ %done arg]
  ::
  ::  Monadic bind.  Combines two computations, associatively.
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(thread-form-raw b) fun=$-(b form)]
    ^-  form
    |=  input=thread-input
    =/  b-res=(thread-output-raw b)
      (m-b input)
    ^-  output
    :-  cards.b-res
    ?-    -.next.b-res
      %wait  [%wait ~]
      %skip  ~|(%bind-got-skip !!)
      %cont  [%cont ..$(m-b self.next.b-res)]
      %fail  [%fail err.next.b-res]
      %done  [%cont (fun value.next.b-res)]
    ==
  ::
  ::  The thread monad must be evaluted in a particular way to maintain
  ::  its monadic character.  +take:eval implements this.
  ::
  ++  eval
    |%
    ::  Indelible state of a thread
    ::
    +$  eval-form
      $:  =form
      ==
    ::
    ::  Convert initial form to eval-form
    ::
    ++  from-form
      |=  =form
      ^-  eval-form
      form
    ::
    ::  The cases of results of +take
    ::
    +$  eval-result
      $%  [%next ~]
          [%fail err=(pair term tang)]
          [%done value=a]
      ==
    ::
    ::  Take a new sign and run the thread against it
    ::
    ++  take
      ::  cards: accumulate throughout recursion the cards to be
      ::         produced now
      =|  cards=(list card)
      |=  [=eval-form =thread-input]
      ^-  [[(list card) =eval-result] _eval-form]
      =*  take-loop  $
      ::  run the thread callback
      ::
      =/  =output  (form.eval-form thread-input)
      ::  add cards to cards
      ::
      =.  cards
        %+  welp
          cards
        ::  XX add tag to wires?
        cards.output
      ::  case-wise handle next steps
      ::
      ?-  -.next.output
          %wait  [[cards %next ~] eval-form]
          %skip  ~|(%take-got-skip !!)
          %fail  [[cards %fail err.next.output] eval-form]
          %done  [[cards %done value.next.output] eval-form]
          %cont
        ::  recurse to run continuation with initialization input
        ::
        %_  take-loop
          form.eval-form  self.next.output
          thread-input    [bowl.thread-input ~]
        ==
      ==
    --
  --
--
