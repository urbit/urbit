::  Produce a glass pill
::
::::  /hoon/glass/gen
  ::
/?    310
/+  pill
::
::::
  !:
:-  %say
|=  {{now/@da * bec/beak} {~ ~}}
::  See also: ++bootstrap in hoon.hoon, ++pill in arvo.hoon
::
::  we're creating an event series E whose lifecycle can be computed
::  with the urbit lifecycle formula L, `[2 [0 3] [0 2]]`.  that is:
::  if E is the list of events processed by a computer in its life,
::  its final state is S, where S is nock(E L).
::
::  in practice, the first four nouns in E are: two boot formulas,
::  a hoon compiler as a nock formula, and the system source code
::
::  after the first four special events, we enter an iterative
::  sequence of regular events which continues for the rest of the
::  computer's life.  during this sequence, each state is a function
::  that, passed the next event, produces the next state.
::
::  a regular event is a `[date wire type data]` tuple, where `date` is a
::  128-bit Urbit date; `wire` is an opaque path which output can
::  match to track causality; `type` is a symbol describing the type
::  of input; and `data` is input data specific to `type`.
::
::  in real life we don't actually run the lifecycle loop,
::  since real life is updated incrementally and also cares
::  about things like output.  we couple to the internal
::  structure of the state machine and work directly with
::  the underlying arvo engine.
::
::  this arvo core, which is at `+7` (Lisp `cddr`) of the state
::  function (see its public interface in `sys/arvo`), gives us
::  extra features, like output, which are relevant to running
::  a real-life urbit vm, but don't affect the formal definition.
::
::  so a real-life urbit interpreter is coupled to the shape of
::  the arvo core.  it becomes very hard to change this shape.
::  fortunately, it is not a very complex interface.
::
:-  %noun
::
::  boot-one: lifecycle formula
::
=+  ^=  boot-one
    ::
    ::  event 1 is the lifecycle formula which computes the final
    ::  state from the full event sequence.
    ::
    ::  the formal urbit state is always just a gate (function)
    ::  which, passed the next event, produces the next state.
    ::
    =>  [boot-formula=* full-sequence=*]
    !=  ::
        ::  first we use the boot formula (event 1) to set up
        ::  the pair of state function and main sequence.  the boot
        ::  formula peels off the first 5 events
        ::  to set up the lifecycle loop.
        ::
        =+  [state-gate main-sequence]=.*(full-sequence boot-formula)
        ::
        ::  in this lifecycle loop, we replace the state function
        ::  with its product, called on the next event, until
        ::  we run out of events.
        ::
        |-  ?@  main-sequence
              state-gate
            %=  $
              main-sequence  +.main-sequence
              state-gate  .*(state-gate [%9 2 %10 [6 %1 -.main-sequence] %0 1])
            ==
::
::  boot-two: startup formula
::
=+  ^=  boot-two
    ::
    ::  event 2 is the startup formula, which verifies the compiler
    ::  and starts the main lifecycle.
    ::
    =>  :*  ::  event 3: a formula producing the hoon compiler
            ::
            compiler-formula=**
            ::
            ::  event 4: system source, unpacked by hoon to multiple events
            ::
            [*wire system-source=*@t]
            ::
            ::  events 5..n: main sequence with normal semantics
            ::
            main-sequence=**
        ==
    !=  :_  main-sequence
        ::
        ::  activate the bootstrap gate.  the product of this formula
        ::  is smaller than the formula.  so you might think we should
        ::  save the gate itself rather than the formula producing it.
        ::  but we have to run the formula at runtime, to register jets.
        ::
        ::  as always, we have to use raw nock as we have no type.
        ::  the gate is in fact ++tide.
        ::
        ~>  %slog.[0 leaf+"1-b"]
        =+  ^=  compiler-bootstrap
            .*(0 compiler-formula)
        ::
        ::  invoke the pill bootstrap
        ::
        .*(compiler-bootstrap [%9 2 %10 [6 %1 %pill system-source] %0 1])
::
::  sys: root path to boot system, `/~me/[desk]/now/sys`
::
=+  sys=`path`/(scot %p p.bec)/[q.bec]/(scot %da now)/sys
::
::  compiler-source: hoon source file producing compiler, `sys/hoon`
::
=+  compiler-source=.^(@t %cx (welp sys /hoon/hoon))
::
::  compiler-twig: compiler as hoon expression
 :: ::
 :: ~&  %glass-parsing
 :: =+  compiler-twig=(ream compiler-source)
 :: ~&  %glass-parsed
 :: ::
 :: ::  compiler-formula: compiler as nock formula
 :: ::
 :: ~&  %glass-compiling
 :: =+  compiler-formula=q:(~(mint ut %noun) %noun compiler-twig)
 :: ~&  %glass-compiled
 :: ::
::  padded: pill byte alignment
::
=/  padded
  |=  [[pad=@u pod=@u] system-source=@t]  ^-  (list *)
  :~  boot-one
      boot-two
      'compiler-formula'
      [(welp pad/(reap pad '-') '\  engaging-text-mode\  ' (reap pod '-')) system-source]
  ==
=/  pad  (end 0 3 (met 0 (jam (padded [0 2] ''))))
::
::  system-source: textual encoding of all files in `sys/`
::
~&  %coalescing
=+  system-source=(coalesce:pill sys)
~&  %coalesced
::
(padded [pad 3] system-source)

