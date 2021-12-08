|%
+$  card          card:agent:gall
+$  input
  $%  [%poke =cage]
      [%sign =wire =sign-arvo]
      [%agent =wire =sign:agent:gall]
      [%watch =path]
  ==
+$  strand-input  [=bowl in=(unit input)]
+$  tid   @tatid
+$  bowl
  $:  our=ship
      src=ship
      tid=tid
      mom=(unit tid)
      wex=boat:gall
      sup=bitt:gall
      eny=@uvJ
      now=@da
      byk=beak
  ==
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
++  strand-output-raw
  |*  a=mold
  $~  [~ %done *a]
  $:  cards=(list card)
      $=  next
      $%  [%wait ~]
          [%skip ~]
          [%cont self=(strand-form-raw a)]
          [%fail err=(pair term tang)]
          [%done value=a]
      ==
  ==
::
++  strand-form-raw
  |*  a=mold
  $-(strand-input (strand-output-raw a))
::
::  Abort strand computation with error message
::
++  strand-fail
  |=  err=(pair term tang)
  |=  strand-input
  [~ %fail err]
::
::  Asynchronous transcaction monad.
::
::  Combo of four monads:
::  - Reader on input
::  - Writer on card
::  - Continuation
::  - Exception
::
++  strand
  |*  a=mold
  |%
  ++  output  (strand-output-raw a)
  ::
  ::  Type of an strand computation.
  ::
  ++  form  (strand-form-raw a)
  ::
  ::  Monadic pure.  Identity computation for bind.
  ::
  ++  pure
    |=  arg=a
    ^-  form
    |=  strand-input
    [~ %done arg]
  ::
  ::  Monadic bind.  Combines two computations, associatively.
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(strand-form-raw b) fun=$-(b form)]
    ^-  form
    |=  input=strand-input
    =/  b-res=(strand-output-raw b)
      (m-b input)
    ^-  output
    :-  cards.b-res
    ?-    -.next.b-res
      %wait  [%wait ~]
      %skip  [%skip ~]
      %cont  [%cont ..$(m-b self.next.b-res)]
      %fail  [%fail err.next.b-res]
      %done  [%cont (fun value.next.b-res)]
    ==
  ::
  ::  The strand monad must be evaluted in a particular way to maintain
  ::  its monadic character.  +take:eval implements this.
  ::
  ++  eval
    |%
    ::  Indelible state of a strand
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
    ++  validate-mark
      |=  [in=* =mark =bowl]
      ^-  cage
      =+  .^  =dais:clay  %cb
              /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[mark]
          ==
      =/  res  (mule |.((vale.dais in)))
      ?:  ?=(%| -.res)
        ~|(%spider-mark-fail (mean leaf+"spider: ames vale fail {<mark>}" p.res))
      [mark p.res]
    ::
    ::  Take a new sign and run the strand against it
    ::
    ++  take
      ::  cards: accumulate throughout recursion the cards to be
      ::         produced now
      =|  cards=(list card)
      |=  [=eval-form =strand-input]
      ^-  [[(list card) =eval-result] _eval-form]
      =*  take-loop  $
      =.  in.strand-input
        ?~  in.strand-input  ~
        =/  in  u.in.strand-input
        ?.  ?=(%agent -.in)      `in
        ?.  ?=(%fact -.sign.in)  `in
        ::
        :-  ~
        :+  %agent  wire.in
        [%fact (validate-mark q.q.cage.sign.in p.cage.sign.in bowl.strand-input)]
      ::  run the strand callback
      ::
      =/  =output  (form.eval-form strand-input)
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
          %skip  [[cards %next ~] eval-form]
          %fail  [[cards %fail err.next.output] eval-form]
          %done  [[cards %done value.next.output] eval-form]
          %cont
        ::  recurse to run continuation with initialization input
        ::
        %_  take-loop
          form.eval-form  self.next.output
          strand-input    [bowl.strand-input ~]
        ==
      ==
    --
  --
--
::
